# https://raw.githubusercontent.com/jbryer/DATA606Fall2018/master/Project/DATA606_proposal_template.Rmd
# http://htmlpreview.github.io/?https://github.com/jbryer/DATA606Fall2018/blob/master/Project/Example_proposal.html

setwd("C:/Users/traveler/Desktop/SPS/606_Statistics and Probability/project/proposal")
if(!require(readr)){install.packages("readr"); require(readr)}

myfile <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv"

df <- readr::read_csv(myfile)
df %>%
        mutate(release_date = as.Date(release_date, "%m/%d/%Y")) %>%
        arrange(release_date) %>%
        View










