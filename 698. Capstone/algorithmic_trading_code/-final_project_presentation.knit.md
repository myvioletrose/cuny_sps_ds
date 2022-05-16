---
title: "data_698_presentation"
author: "Jimmy Ng"
date: "May 16, 2022"
output:
  rmdformats::readthedown:
    self_contained: yes
    thumbnails: yes
    lightbox: yes
    gallery: no
    highlight: tango
    code_folding: hide
editor_options: 
  chunk_output_type: console
---



# (1) set-up

## load packages, helper functions {.tabset .tabset-fade .tabset-pills}


```r
# load tictoc
if(!require(pacman)){install.packages("pacman"); require(pacman)}
pacman::p_load(tictoc)

# set environment, proxy servers if running the script at Bloomberg office desktop
if(tolower(Sys.getenv("username")) == "jng410"){
    proxy <- "proxy.bloomberg.com:81"
    Sys.setenv(
        https_proxy = proxy,
        http_proxy = proxy
    )
}

# set environment
readRenviron("~/I/config/.env")

# set local environment variables using global environment
SP500_LIST_PATH <- Sys.getenv("SP500_LIST_PATH")
PACKAGE_PATH <- Sys.getenv("PACKAGE_PATH")
PROJECT_HOME_DIRECTORY <- Sys.getenv("PROJECT_HOME_DIRECTORY")
STOCK_LIST_PATH <- Sys.getenv("STOCK_LIST_PATH")
FUNCTION_DIRECTORY <- Sys.getenv("FUNCTION_DIRECTORY")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
PLOT_PATH <- Sys.getenv("PLOT_PATH")
VOTES_PATH = Sys.getenv("VOTES_PATH")
TA_FOLDER <- Sys.getenv("TA_FOLDER")
DIAGNOSTIC_FOLDER <- Sys.getenv("DIAGNOSTIC_FOLDER")
DATA_DIRECTORY <- Sys.getenv("DATA_DIRECTORY")
DECRYPT <- Sys.getenv("DECRYPT")
SENDER <- Sys.getenv("SENDER") 
USERNAME <- Sys.getenv("USERNAME")
PASSWORD <- Sys.getenv("PASSWORD")
INTRADAY = FALSE
REAL_TIME = TRUE
LONG_LIST = TRUE
SP500 = TRUE
if(!SP500){REGULAR = TRUE}


# set project home directory
setwd(PROJECT_HOME_DIRECTORY)

# load packages
packages <- read.csv(PACKAGE_PATH, header = FALSE)
pacman::p_load(char = as.vector(packages$V1))

# source all functions
sapply(paste(FUNCTION_DIRECTORY, grep(pattern = "\\.[Rr]$", list.files(FUNCTION_DIRECTORY), value = TRUE), sep = "/"), function(x) source(x)) %>% invisible()
```

## connection to postgresql {.tabset .tabset-fade .tabset-pills}


```r
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- Sys.getenv("DB_PASSWORD")
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

# disconnect db
#dbDisconnect(con) 

# tables
sf = "stock_fact"
sd = "stock_dim"
si = "stock_indicator"
haf = "heikin_ashi_fact"
ir = "investment_risk"

df1 = "strategy_final_project_df1"
df2 = "strategy_final_project_df2"
df3 = "strategy_final_project_df3"

df4_cm = "strategy_final_project_df4_cm"
df4 = "strategy_final_project_df4"

df5_detail = "strategy_final_project_df5_backtest_detail"
df5_simplified = "strategy_final_project_df5_backtest_simplified"

df6 = "strategy_final_project_df6_analysis"

tbl_list = sapply(list(sf, sd, si, haf, ir, 
                       df1, df2, df3,
                       df4_cm, df4,
                       df5_detail, df5_simplified,
                       df6), function(x) paste0(schema, ".", x))

names(tbl_list) = wrapr::qc(sf, sd, si, haf, ir, 
                            df1, df2, df3,
                            df4_cm, df4,
                            df5_detail, df5_simplified,
                            df6)

sapply(1:length(tbl_list), function(x) assign(names(tbl_list)[x], value = tbl_list[[x]], envir = .GlobalEnv)) %>% 
    as.data.frame() %>%
    dplyr::select(table = ".")
##                                                   table
## 1                                      stock.stock_fact
## 2                                       stock.stock_dim
## 3                                 stock.stock_indicator
## 4                                stock.heikin_ashi_fact
## 5                                 stock.investment_risk
## 6                      stock.strategy_final_project_df1
## 7                      stock.strategy_final_project_df2
## 8                      stock.strategy_final_project_df3
## 9                   stock.strategy_final_project_df4_cm
## 10                     stock.strategy_final_project_df4
## 11     stock.strategy_final_project_df5_backtest_detail
## 12 stock.strategy_final_project_df5_backtest_simplified
## 13            stock.strategy_final_project_df6_analysis

# helper function
psql <- function(query = "", connection = con){
    dbGetQuery(connection, glue::glue(query))
}

# test connection
psql(query = "select * from {sf} limit 5")
##   symbol       date year quarter month   weekday day  open  high   low close
## 1      A 1999-11-18 1999       4    11  Thursday  18 45.50 50.00 40.00 44.00
## 2      A 1999-11-19 1999       4    11    Friday  19 42.94 43.00 39.81 40.38
## 3      A 1999-11-22 1999       4    11    Monday  22 41.31 44.00 40.06 44.00
## 4      A 1999-11-23 1999       4    11   Tuesday  23 42.50 43.63 40.25 40.25
## 5      A 1999-11-24 1999       4    11 Wednesday  24 40.13 41.94 40.00 41.06
##     volume adjusted
## 1 44739900 28.94786
## 2 10897100 26.56624
## 3  4705200 28.94786
## 4  4274400 26.48071
## 5  3464400 27.01362
```



## data exploration : physiological measures

We have 3 distinct species. Let's focus on the four main physiological measures, i.e. "bill_length_mm", "bill_depth_mm", "flipper_length_mm", and "body_mass_g", and see how they differ. 


```r
# bodyVars = metadata %>% dplyr::filter(unique >3) %>% .$colName %>% as.character
# 
# dfGather <- df %>%
#     dplyr::select(all_of(c("species", bodyVars))) %>%
#     dplyr::mutate(species = as.character(species)) %>%
#     tidyr::gather(key, value, -species)
# 
# ggplot(dfGather, aes(value, color = species)) +
#     geom_density() +
#     theme(legend.position = "top") +
#     geom_vline(data = aggregate(value ~ species + key, dfGather, median), 
#                aes(xintercept = value,
#                    color = species),
#                linetype = "dashed") +
#     facet_wrap(~ key, nrow = 5, scales = "free")
```

