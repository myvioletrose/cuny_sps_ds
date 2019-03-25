# load packages
library(tidyverse)
library(plyr)
library(Hmisc)
library(sqldf)

# load csv
setwd("C:/Users/traveler/Desktop/SPS/607. Data Acquisition and Management/Projects/Project3/code")
current_wd <- getwd()
setwd("../"); setwd("data")
df <- read.csv("indeed_job_dataset.csv", stringsAsFactors = F)
setwd(current_wd)

# extract "jk_id", "fcc_id" from link
df <- df %>%
        dplyr::mutate(jk_id = str_extract_all(Link, pattern = "jk=[[:alnum:]]+&") %>% 
                              str_replace_all(., pattern = "jk=|&", replacement = ""),
                      fcc_id = str_extract_all(Link, pattern = "fccid=[[:alnum:]]+&") %>% 
                              str_replace_all(., pattern = "fccid=|&", replacement = ""))
dim(df)

# clean up header
names(df) <- tolower(names(df))

# check Ids
sapply(list(df$jk_id, df$fcc_id), function(x){length(unique(x))})

# lookup both Ids, some links are missing the "jk_id"
jk_id.lookup <- plyr::count(df, "jk_id") %>% arrange(desc(freq))
table(jk_id.lookup$freq); head(jk_id.lookup)

# "fcc_id" can be duplicated because the same company can post the same job position with different attributes, most likely offering the same position in different locations
fcc_id.lookup <- plyr::count(df, "fcc_id") %>% arrange(desc(freq))
head(fcc_id.lookup)

# the result indicates that each "jk_id" is unique in the data set; there is no duplication for any "jk_id"
# however, 99 jobs have missing "jk_id"
# why? what are these 99 jobs? 

jk_id.missing <- df %>%
        dplyr::filter(jk_id == "character(0)") 
# write.csv(jk_id.missing, "clipboard", row.names = F)

# let's fix these "jk_id" - these are written differently as there's no "jk=" in these links        
# e.g. https://www.indeed.com/company/Wag!/jobs/Data-Engineer-0633d6309b9f2be8?fccid=381733c3e1596619&vjs=3
jk_id.missing <- jk_id.missing %>%
        dplyr::mutate(jk_id = str_extract_all(jk_id.missing$link, pattern = "-[[:alnum:]]+\\?fccid") %>%
                              str_replace_all(., pattern = "-|\\?fccid", replacement = ""))

# let's 'union all' both sets
df <- df %>%
        dplyr::filter(x %nin% jk_id.missing$x) %>%
        dplyr::bind_rows(., jk_id.missing)

##############################
##### data normalization #####
##############################
# we are going to create a simple star schema for this data set
# we need four tables, i.e. "job_post_specific", "job_position", "company", & "description"
# "job_post_specific" table - "jk_id" is the primary key. Each "jk_id" is unique and that represents a post for one job position from a company
# "job_position" table - beware of the original "fcc_id"! Note, the "job_post_specific" and "job_position" tables are different. The same job position is supposed to share an idential and unique "fcc_id"; however, there can be multiple posting. In other words, we expect to see the same "fcc_id" for multiple "jk_id". 
# For instance, Google posted four identical position "Google AI Resident, 2019 Start (Fixed-Term Employee)" with the same "fcc_id" (a5b4499d9e91a5c6) but four different "jk_id". These four positions were offered in different locations (NY, MA, CA, & WA)
# We should consider these four positions as one when counting for skill sets; otherwise, we will inflate our numbers when calculating for the percentage based on skill sets
# However, the data is also messy in terms of some companies posted different job positions with the same "fcc_id"!
# Using Google and the same "fcc_id" (a5b4499d9e91a5c6) as an example, there are actually 40 entries in the data set that share the same "fcc_id"! That simply means that there are different job positions share the same "fcc_id", but we also have identical jobs share the same "fcc_id" with different entries in the data set because they can be offered in different locations
# one extreme case, Booz Allen Hamilton posted 151 different jobs with identical "fcc_id" (4e041af1d0af1bc8)!
# we must clean up the messy "fcc_id" before splitting up the data set into four tables
# we must 1) remove duplication of identical jobs (job_title, queried_salary, job_type, skill, company), and 2) create unique "fcc_unique_id" as the primary key 
# last but not least, we also need to clean up the "company" table by creating a company Id and performing simple Change-Data-Capture

################
# job_position #
################
# distinct, create fcc_unique_id
job_position <- df %>%
        dplyr::select(fcc_id, job_title, queried_salary, job_type, skill, company) %>%
        dplyr::distinct() %>%
        # create a "fcc_unique_id" after the dplyr::distinct()
        dplyr::mutate(fcc_unique_id = paste(row_number(), fcc_id, sep = "_"))

#####################
# job_post_specific #
#####################
job_post_specific <- sqldf("
select df.jk_id
, jp.fcc_unique_id
, df.link
, df.date_since_posted
, df.location 
from job_position jp
join (
        select jk_id, fcc_id, job_title, queried_salary, job_type, skill, company
        , link, date_since_posted, location
        from df
) df on jp.fcc_id = df.fcc_id
and jp.job_title = df.job_title 
and jp.queried_salary = df.queried_salary 
and jp.job_type = df.job_type 
and jp.skill = df.skill 
and jp.company = df.company
")

# check again
sapply(list(job_position$fcc_unique_id, job_post_specific$fcc_unique_id), function(x) unique(x) %>% length)
sapply(list(df$jk_id, job_post_specific$jk_id), function(x) unique(x) %>% length)

# great, we do not lose any distinct job position or posting 
# we get rid of identical jobs that share the same "fcc_id", and we also create a "fcc_unique_id" to replace the existing "fcc_id" - this way, we don't see different jobs share the same "fcc_id", and we create a unique "fcc_unique_id" for all distinct jobs

#########################
# clean-up job_position #
#########################
# create a company ID
company_index <- df %>%
        dplyr::select(company) %>%
        distinct() %>% 
        arrange(company) %>%
        dplyr::mutate(company_id = paste("c", row_number(), sep = "_"))

job_position <- job_position %>%
        dplyr::left_join(., company_index) %>%
        dplyr::select(fcc_unique_id, job_title, queried_salary, job_type, skill, company_id)

###########
# company #
###########
company <- df %>%
        dplyr::select(company, no_of_reviews, no_of_stars, company_revenue, company_employees, company_industry) %>%
        distinct() %>%
        dplyr::left_join(., company_index) %>%
        dplyr::select(company_id, everything()) %>%
        arrange(company_id)

# perform simple CDC - Chang-Data-Capture
# get rid of multiple entries by returning the max of "no_of_stars" and "no_of_reviews" b/c we suppose that's the latest update for the company
company <- sqldf("
select company_id, company, company_revenue, company_employees, company_industry
, max(no_of_stars) as no_of_stars
, max(no_of_reviews) as no_of_reviews
from company
group by 1, 2, 3, 4, 5
order by company
"
)        

###############
# description #
###############
description <- df %>%
        dplyr::select(link, description) %>%
        distinct()

####################
# write csv output #
####################
setwd("../"); setwd("data")
write.csv(job_position, "job_position.csv", row.names = F)
write.csv(job_post_specific, "job_post_specific.csv", row.names = F)
write.csv(company, "company.csv", row.names = F)
write.csv(description, "description.csv", row.names = F)

# save the four objects
keep <- c("job_position", "job_post_specific", "company", "description")
remove <- c(ls()[ls() %nin% keep])
rm(list = setdiff(remove, keep))
save(list = ls(), file = "data_ETL.Rda")

setwd(current_wd)

















