setwd("C:/Users/traveler/Desktop/SPS/606_Statistics and Probability/R")

install.packages(c('openintro','OIdata','devtools','ggplot2','psych','reshape2',
                   'knitr','markdown','shiny'))

devtools::install_github("jbryer/DATA606")

library(DATA606)
startLab('Lab0')
