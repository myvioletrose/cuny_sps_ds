# load packages
library(SASxport)
library(tidyverse)
library(sqldf)
library(janitor)

# get file names
fileNames <- dir() %>% lapply(., function(x) gsub(".XPT", "", .)) %>% unlist %>% unique
files <- dir()

# read XPT data
for(i in 1:length(files)){
        assign(bquote(.(fileNames[i])), value = read.xport(files[i]))
}

# check ids
id1 <- DPQ_J$SEQN %>% as.integer
id2 <- HSQ_J$SEQN %>% as.integer
id3 <- OCQ_J$SEQN %>% as.integer
id4 <- SLQ_J$SEQN %>% as.integer
id5 <- SMQ_J$SEQN %>% as.integer
        
seqnList <- list(id1, id2, id3, id4, id5)

lapply(seqnList, function(x) length(unique(x)))

lapply(seqnList, function(x) length(x))

# check unique ids and see if we can get a sizable data frame
seqnList %>% unlist %>% unique %>% length()

# combine
df <- DPQ_J %>%
        inner_join(HSQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(OCQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(SLQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(SMQ_J, by = c("SEQN", "SEQN"))

dim(df)

# check missing
tempDf <- data.frame(fields = names(df),
                     missing = colSums(is.na(df)))

# get a list of variables that do not have significant number of missing values
fields <- tempDf %>%
        dplyr::filter(missing < 1000) %>%
        .$fields %>%
        as.character

# subset fields
df <- df %>%
        dplyr::select(fields)

# clean up field names
df <- df %>% janitor::clean_names()

# check again
dim(df)
head(df)

# write output data
write.csv(df, "df.csv", row.names = FALSE)








