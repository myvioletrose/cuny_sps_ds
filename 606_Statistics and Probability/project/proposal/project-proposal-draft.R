# https://raw.githubusercontent.com/jbryer/DATA606Fall2018/master/Project/DATA606_proposal_template.Rmd
# http://htmlpreview.github.io/?https://github.com/jbryer/DATA606Fall2018/blob/master/Project/Example_proposal.html

# setwd("C:/Users/traveler/Desktop/SPS/606_Statistics and Probability/project/proposal")
#if(!require(readr)){install.packages("readr"); require(readr)}

# load packages
library(plyr)
library(lubridate)
library(tidyverse)

myfile <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv"
df <- readr::read_csv(myfile)

###################
### have a look ###
###################
# head, dim, str
head(df)
dim(df)  # [1] 3401    9 
str(df)

############
# clean-up #
############
# check duplication: is there any movie that's duplicated in this data set?
plyr::count(df, "movie") %>%
        filter(freq >1)

# what is this movie?         
df %>%
        filter(movie == "Tau ming chong") %>%
        print
# id == 2974, 2975

# let's remove either one of the rows
df <- df %>%
        filter(X1 != 2974)

# is there any movie that has 0 or even negative domestic/worldwide gross?
df %>%
        filter(domestic_gross <=0 | worldwide_gross <= 0)
# there are 66 of these in this data set

# let's not remove them but create a flag for each of these variables
df$domestic_flag <- ifelse(df$domestic_gross <=0, 0, 1); sum(df$domestic_flag)
df$worldwide_flag <- ifelse(df$worldwide_gross <=0, 0, 1); sum(df$worldwide_flag)

# let's rename the X1 column and rename it as a movie id column
names(df)[1] <- "movie_id"

# change "date": turn the release_date column as date data type; add release day, month, year
df <- df %>%
        mutate(release_date = lubridate::mdy(release_date),
               release_day = lubridate::wday(release_date, 
                                             week_start = getOption("lubridate.week.start", 1)),
               release_month = lubridate::month(release_date),
               release_year = lubridate::year(release_date))

# rescale the production_budget, domestic_gross & worldwide_gross (by dividing 1 million)
# so that they are easier to read
df <- df %>%
        mutate(production_budget = production_budget / 1000000,
               domestic_gross = domestic_gross / 1000000,
               worldwide_gross = worldwide_gross / 1000000)
               
# make "factor" for mpaa_rating & genre
df <- df %>%
        mutate(mpaa_rating = factor(mpaa_rating, 
                                    levels = c("G", "PG", "PG-13", "R")),
               genre = as.factor(genre))

# complete.cases - remove all NA's
dfComplete <- df[complete.cases(df), ]

# head, dim, str
head(dfComplete)
dim(dfComplete)  # [1] 3230   14
str(dfComplete)

# lm
df_lm <- lm(worldwide_gross ~ production_budget + mpaa_rating + genre + release_month
            , data = dfComplete %>% filter(worldwide_flag == 1))
summary(df_lm)







