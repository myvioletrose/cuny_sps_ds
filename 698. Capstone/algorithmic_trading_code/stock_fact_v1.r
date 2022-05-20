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

### GET SYMBOLS ###

# load a list of tracking entity
if(LONG_LIST){
    symbols <- read.csv(STOCK_LIST_PATH, header = FALSE) %>% 
        unlist() %>% 
        paste(., collapse = ", ") %>% 
        stringr::str_split(., pattern = ", ") %>%
        unlist() %>%
        unique()
} else {
    symbols = stocks_care_about
}

# if(SP500) {get SP500}
if(SP500){
    sp500 <- readit::readit(SP500_LIST_PATH) 
    symbols <- sp500 %>%
        dplyr::arrange(symbol) %>%
        dplyr::select(symbol) %>%
        .$symbol
}

# detect, use multicores
numCores <- parallel::detectCores()

# create a simple cluster on the local machine using all available threads
cl <- parallel::makeCluster(detectCores(), methods = FALSE)

# register our cluster
doParallel::registerDoParallel(cl)

# return a single list of xts objects from the valid symbols
if(REAL_TIME){
    
    xtsList <- vector(mode = "list", length = length(symbols))
    
    if(INTRADAY){p = "intraday"} else {p = "daily"}
    
    for(i in 1:length(symbols)){
        
        xtsList[[i]] <- try({
            
            quantmod::getSymbols(
                
                symbols[i], 
                env = NULL,  # set env = NULL and that is equivalent to auto.assign = FALSE
                src = "av",
                periodicity = p,
                output.size = "full", 
                adjusted = !INTRADAY,
                api.key = ALPHA_VANTAGE_API
                
            )
            
        }, silent = TRUE)
        
        #Sys.sleep(12)
        
    }
    
} else {
    # loop through a list of stocks tickers - super fast! 5x faster than the traditional for-loop approach!!
    symbolsCheck <- foreach::foreach(i = 1:length(symbols), .errorhandling = 'remove') %dopar% { quantmod::getSymbols(symbols[i]) } %>% unlist  # change .errorhandling = 'pass' to see error
    
    # print out a list of invalid tickers
    if(length(setdiff(symbols, symbolsCheck)) >0){
        
        errorSymbols = setdiff(symbols, symbolsCheck)
        
        sapply(1:length(errorSymbols), function(x){
            print(paste0("the symbol ", errorSymbols[x], " cannot be fetched from quantmod"))
        })
        
        cat("###########################################\n###########################################\n")
        print(paste0("there are ", 
                     length(errorSymbols), 
                     " symbols that cannot be fetched from quantmod"))
        cat("###########################################\n###########################################\n")
        
    }
    
    symbols <- symbolsCheck
    
    xtsList <- foreach::foreach(i = 1:length(symbols)) %dopar% {quantmod::getSymbols(symbols[i], env = NULL, adjusted = TRUE)}
    # set env = NULL and that is equivalent to auto.assign = FALSE        
}

# get names for xtsList
symbols <- stringr::str_replace_all(symbols, "\\^GSPC", "SP500")
names(xtsList) <- symbols

# drop retrieval errors from list
if(any(sapply(xtsList, class) == "try-error")){
    
    i = which(sapply(xtsList, class) == "try-error")
    
    errorSymbols = names(xtsList)[i]
    
    xtsList[i] <- NULL
    
    sapply(1:length(errorSymbols), function(x){
        print(paste0("the symbol ", errorSymbols[x], " cannot be fetched from quantmod"))
    })
    
    cat("###########################################\n###########################################\n")
    print(paste0("there are ", 
                 length(errorSymbols), 
                 " symbols that cannot be fetched from quantmod"))
    cat("###########################################\n###########################################\n")
    
}

# stop the cluster
parallel::stopCluster(cl)

cat("###########################################\n###########################################\n")
print(paste0("data retrieval real time ", REAL_TIME))
print(Sys.time())

# source all functions
sapply(paste(FUNCTION_DIRECTORY, grep(pattern = "\\.[Rr]$", list.files(FUNCTION_DIRECTORY), value = TRUE), sep = "/"), function(x) source(x)) %>% invisible()

################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

############### stock.stock_fact
# transform data
temp <- vector(mode = "list", length = length(XTSLIST))
for(i in 1:length(XTSLIST)){
    temp[[i]] <- XTSLIST[[i]] %>%
        historicalTransformation(., transformOnly = TRUE) %>%
        dplyr::select(symbol, date, year, quarter, month, weekday, day,
                      open, high, low, close, volume, adjusted)
    row.names(temp[[i]]) <- NULL
}
tempDf <- temp %>% dplyr::bind_rows()
tempDf %>% dim()

# write table
tbl_name = "stock_fact"
tbl = paste0(schema, ".", tbl_name)
DBI::dbWriteTable(con, DBI::SQL(tbl), tempDf, overwrite = TRUE)

# data validation
query = glue::glue("
select count(1)
from {tbl}
")

dbGetQuery(con, query)

dbGetQuery(con, glue::glue("
select symbol, count(1)
, min(date) as min_date
, max(date) as max_date
from {tbl}
group by symbol
")) -> x

#####################################
query = glue::glue("
with sub
as (
	select symbol, count(1)
	, min(date) as min_date
	, max(date) as max_date
	from {tbl}
	group by symbol
)

select symbol
from sub
where extract(year from min_date) <=2015
and extract(year from max_date) = 2022
order by symbol
")

s = dbGetQuery(con, query)

write.table(s, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)

#############################
# upload sp500 dim
sp500 <- readit::readit(SP500_LIST_PATH) %>%
    dplyr::mutate(last_run = Sys.Date())

# write table
tbl_name = "stock_dim"
tbl = paste0(schema, ".", tbl_name)
DBI::dbWriteTable(con, DBI::SQL(tbl), sp500, overwrite = TRUE)

dbGetQuery(con, glue::glue("select * from {tbl}"))







###############################################################################################
# check table
dbGetQuery(con, glue::glue("select * from {tbl}")) %>% dim()

# list tables
#dbListTables(con)
dbGetQuery(con, glue::glue("SELECT 
   table_name, 
   column_name, 
   data_type 
FROM 
   information_schema.columns
WHERE 
   table_name = '{tbl_name}';"))

# disconnect db
dbDisconnect(con) 















