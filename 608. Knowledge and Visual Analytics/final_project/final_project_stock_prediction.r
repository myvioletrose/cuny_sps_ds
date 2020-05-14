############################################################
### SET UP ###

# load packages
packages <- c('broom', 'caret', 'doParallel', 'e1071', 'foreach', 'glue', 'gridExtra', 'Hmisc', 'htmlwidgets', 'InformationValue', 'kableExtra', 'parallel', 'plotly', 'quantmod', 'sqldf', 'tictoc', 'tidyverse', 'TTR', 'wrapr')
pacman::p_load(char = packages)

# set environment
readRenviron("~/I/config/.env")

# set project home directory
PROJECT_HOME_DIRECTORY <- Sys.getenv("PROJECT_HOME_DIRECTORY")
setwd(PROJECT_HOME_DIRECTORY)

# get real time data
REAL_TIME = TRUE
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")

### GET SYMBOLS ###

# stock symbol
stock <- c("AAPL")

# detect, use multicores
numCores <- parallel::detectCores()

# create a simple cluster on the local machine using all available threads
cl <- parallel::makeCluster(detectCores(), methods = FALSE)

# register our cluster
doParallel::registerDoParallel(cl)

# loop through a list of stock tickers - super fast! 5x faster than the traditional for-loop approach!!
symbols <- foreach::foreach(i = 1:length(stock), .errorhandling = 'remove') %dopar% { quantmod::getSymbols(stock[i]) } %>% unlist  # change .errorhandling = 'pass' to see error

# return a single list of xts objects from the valid symbols
if(REAL_TIME){
        
        xtsList <- vector(mode = "list", length = length(symbols))
        
        for(i in 1:length(symbols)){
                
                xtsList[[i]] <- quantmod::getSymbols(
                        
                        symbols[i], 
                        env = NULL,  # set env = NULL and that is equivalent to auto.assign = FALSE
                        src = "av",
                        periodicity = "daily", 
                        output.size = "full", 
                        adjusted = TRUE,
                        api.key = ALPHA_VANTAGE_API
                        
                )
                
                Sys.sleep(12)
                
        }
        
} else {
        
        xtsList <- foreach::foreach(i = 1:length(symbols)) %dopar% { quantmod::getSymbols(symbols[i], env = NULL, adjusted = TRUE) }  # set env = NULL and that is equivalent to auto.assign = FALSE        
        
}

# set names for xtsList
names(xtsList) <- symbols

############################################################
### CUSTOM FUNCTION ###
historicalTransformation <- function(xts,        
                                     l = 1,
                                     n = 1,
                                     originalSymbol = FALSE,
                                     transformOnly = FALSE
){
        
        # transform() function
        transformation <- function(xts, l, n, originalSymbol){
                
                # symbol
                symbol <- stringr::str_extract_all(names(xts), pattern = "^[[:alpha:]].*\\.") %>% unlist %>% stringr::str_to_lower(.) %>% unique %>% gsub("\\.", "", .)
                
                # rename columns
                names(xts) <- gsub("^[[:alpha:]].*\\.", "", names(xts)) %>% stringr::str_to_lower(.)
                
                # turn it into data.frame
                xts <- as.data.frame(xts)
                
                # insert columns
                xts <- xts %>%
                        dplyr::mutate(
                                # date
                                date = row.names(xts) %>% lubridate::ymd(.),
                                year = lubridate::year(date),
                                quarter = lubridate::quarter(date),
                                month = lubridate::month(date),
                                weekday = base::weekdays(date),
                                day = lubridate::day(date),
                                # Lag
                                op_l = quantmod::Lag(xts[, "open"], l),
                                hi_l = quantmod::Lag(xts[, "high"], l),
                                lo_l = quantmod::Lag(xts[, "low"], l),
                                cl_l = quantmod::Lag(xts[, "close"], l),
                                vol_l = quantmod::Lag(xts[, "volume"], l),
                                adj_l = quantmod::Lag(xts[, "adjusted"], l),
                                # Next
                                op_n = lead(xts[, "open"], n),
                                hi_n = lead(xts[, "high"], n),
                                lo_n = lead(xts[, "low"], n),
                                cl_n = lead(xts[, "close"], n),
                                vol_n = lead(xts[, "volume"], n),
                                adj_n = lead(xts[, "adjusted"], n),
                                # daily "Close - Open", "Hi - Low"
                                cl_op_diff = close - open,
                                hi_lo_diff = high - low,
                                # Lag "diff"
                                op_l_diff = open - op_l,
                                hi_l_diff = high - hi_l,
                                lo_l_diff = low - lo_l,
                                cl_l_diff = close - cl_l,
                                vol_l_diff = volume - vol_l,
                                adj_l_diff = adjusted - adj_l,
                                # Next "diff"
                                op_n_diff = op_n - open,
                                hi_n_diff = hi_n - high,
                                lo_n_diff = lo_n - low,
                                cl_n_diff = cl_n - close,
                                vol_n_diff = vol_n - volume,
                                adj_n_diff = adj_n - adjusted,
                                # oscillator - RSI, CCI                                
                                rsi_close = TTR::RSI(xts[, "close"]),
                                cci_close = TTR::CCI(xts[, "close"])
                        )
                
                # oscillator - MACD
                macd <- MACD(xts$close) %>% as.data.frame %>% dplyr::select(macd)
                signal <- MACD(xts$close) %>% as.data.frame %>% dplyr::select(signal)
                
                # add MACD
                xts <- xts %>%
                        dplyr::mutate(macd_close = macd$macd,
                                      signal_close = signal$signal,
                                      macd_close_diff = macd_close - signal_close) %>%
                        dplyr::select(date, year, quarter, month, weekday, day,
                                      open, high, low, close, volume, adjusted,
                                      rsi_close, cci_close, macd_close, signal_close, macd_close_diff,
                                      everything()) %>%
                        dplyr::arrange(date)
                
                # rename columns back with original symbol
                if(originalSymbol){names(xts) <- paste(symbol, names(xts), sep = "_")}
                
                # return xts
                return(xts)
                
        }
        
        # return a df object
        if(transformOnly){
                
                # return a transformed xts object
                transformObj <- transformation(xts, l, n, originalSymbol)
                return(transformObj)
                
        } else {
                
                # return the min and max of different variables associated with dates based on a transformed xts object
                SYMBOL <- transformation(xts, l, n, originalSymbol)
                
                minVector <- SYMBOL[complete.cases(SYMBOL), 6:ncol(SYMBOL)] %>% lapply(., min) %>% unlist
                maxVector <- SYMBOL[complete.cases(SYMBOL), 6:ncol(SYMBOL)] %>% lapply(., max) %>% unlist
                
                minList <- vector(mode = "list", length = length(minVector))
                for(i in 1:length(minList)){
                        minList[[i]] <- sqldf(sprintf("select date, %s from SYMBOL where floor(%s) = floor(%f)", names(minVector)[i], names(minVector)[i], minVector[i]))
                }
                
                maxList <- vector(mode = "list", length = length(maxVector))
                for(i in 1:length(maxList)){
                        maxList[[i]] <- sqldf(sprintf("select date, %s from SYMBOL where ceil(%s) = ceil(%f)", names(maxVector)[i], names(maxVector)[i], maxVector[i]))
                }
                
                minDf <- minList %>%
                        bind_rows() %>%
                        tidyr::gather(., key, value, -date) %>%
                        dplyr::filter(!is.na(value)) %>%
                        dplyr::mutate(type = "min") %>%
                        arrange(key, date)
                
                maxDf <- maxList %>%
                        bind_rows() %>%
                        tidyr::gather(., key, value, -date) %>%
                        dplyr::filter(!is.na(value)) %>%
                        dplyr::mutate(type = "max") %>%
                        arrange(key, date)
                
                historyObj <- bind_rows(minDf, maxDf) %>%
                        dplyr::select(., type, everything()) %>%
                        arrange(type, key, date)
                
                return(historyObj)
        }
        
}

############################################################
### SET PARAMETERS FOR MODELS ###

# trend
volatility = c("bullish", "bearish")

# target percentile threshold, e.g. top X% and bottom X% price change (cl_n_diff / close)
cl_percentile_threshold = 0.15

# xts subset
xts_data_subset = TRUE

# xts subset partition, i.e. number of most recent trading days
xts_data_subset_partition = 1000

# lag day(s)
l_period = c(1, 5, 10, 15, 20)

# next day(s)
n_period = c(1, 2, 3, 4, 5)

# baseTrainMethods
baseTrainMethods <- c("glm", "nb", "gbm")

# topTrainMethods
topTrainMethods <- c("glm", "nb", "gbm")

# tune length
tune.length = 3

# seed 
seed = 1234

# split data into train and test set by XX
size = 0.8

# Dependent Variable
DV <- "target"

# Features Selection Threshold
features_selection_threshold = .01

# dfEval, i.e. number of trading days for evaluation
dfEval_subset_partition = 90

# output
modelSummaryList <- vector(mode = "list")

evalList <- vector(mode = "list")

############################################################
### BUILD MODELS ###

# start timer
tic()

# start nested for() loop        
for(b in volatility){
        
        marketTrend = b
        
        for(l in l_period){
                
                lday = l
                
                for(i in n_period){
                        
                        nday = i
                        
                        for(j in stock){
                                
                                # get data
                                x <- xtsList[[j]]
                                
                                # get target threshold
                                if(xts_data_subset){
                                        y.t <- historicalTransformation(x, l = lday, n = nday, transformOnly = TRUE) %>%
                                                dplyr::mutate(percent_diff = cl_n_diff / close,
                                                              partition = (nrow(.)-1):0) %>%
                                                dplyr::filter(partition <= xts_data_subset_partition)
                                } else {
                                        y.t <- historicalTransformation(x, l = lday, n = nday, transformOnly = TRUE) %>%
                                                dplyr::mutate(percent_diff = cl_n_diff / close)
                                }
                                y <- y.t$percent_diff
                                y <- y[!is.na(y)]
                                z <- quantile(y, c(cl_percentile_threshold, 1 - cl_percentile_threshold))
                                
                                # features
                                features <- c(                                
                                        'rsi_close', 
                                        'cci_close',
                                        'macd_close', 
                                        'signal_close',
                                        'vol_n_diff',
                                        'op_l_percent_diff', 
                                        'hi_l_percent_diff', 
                                        'lo_l_percent_diff', 
                                        'cl_l_percent_diff', 
                                        'vol_l_percent_diff',    
                                        'cl_op_diff_by_op',
                                        'hi_lo_diff_by_op',
                                        'cl_op_diff_by_cl_l',
                                        'hi_lo_diff_by_cl_l',
                                        'cl_cl_l_ratio',
                                        'op_cl_l_ratio',
                                        'vol_cl_l_ratio',
                                        'vol_op_l_ratio',  
                                        'vol_diff_ratio',
                                        'sma_5',                        
                                        'sma_10',
                                        'sma_15'
                                )
                                
                                # transform, get DV, set partition
                                x.t <- historicalTransformation(x, l = lday, n = nday, transformOnly = TRUE) %>%
                                        dplyr::mutate(
                                                trend = marketTrend,
                                                symbol = j,                        
                                                l_period = lday,
                                                n_period = nday,
                                                op_l_percent_diff = ((open - op_l) / op_l) %>% as.vector,
                                                hi_l_percent_diff = ((high - hi_l) / hi_l) %>% as.vector,
                                                lo_l_percent_diff = ((low - lo_l) / lo_l) %>% as.vector,
                                                cl_l_percent_diff = ((close - cl_l) / cl_l) %>% as.vector,
                                                vol_l_percent_diff = ((volume - vol_l) / vol_l) %>% as.vector,
                                                cl_op_diff_by_op = ((close - open) / open) %>% as.vector,
                                                hi_lo_diff_by_op = ((high - low) / open) %>% as.vector,
                                                cl_op_diff_by_cl_l = ((close - open) / cl_l) %>% as.vector,
                                                hi_lo_diff_by_cl_l = ((high - low) / cl_l) %>% as.vector,
                                                cl_cl_l_ratio = (close / cl_l) %>% as.vector,
                                                op_cl_l_ratio = (open / cl_l) %>% as.vector, 
                                                vol_cl_l_ratio = (vol_l_percent_diff / cl_cl_l_ratio) %>% as.vector, 
                                                vol_op_l_ratio = (vol_l_percent_diff / op_cl_l_ratio) %>% as.vector,  
                                                vol_diff_ratio = ((vol_n_diff / vol_l_diff) %>% as.vector) %>% as.vector, 
                                                sma_5 = TTR::SMA(close, 5),                                
                                                sma_10 = TTR::SMA(close, 10),
                                                sma_15 = TTR::SMA(close, 15),
                                                target = dplyr::case_when(trend == "bullish" ~ ifelse( (cl_n_diff / close) > z[2], "Y", "N" ),
                                                                          trend == "bearish" ~ ifelse( (cl_n_diff / close) < z[1], "Y", "N" )) %>%
                                                        factor(., levels = c("Y", "N"), labels = c("Yes", "No")),            
                                                partition = (nrow(.)-1):0) %>%  # 0 is referred to the most recent (or last) trading day
                                        dplyr::select(trend, symbol, l_period, n_period, partition, date, year, quarter, month, weekday, day, everything()) %>%
                                        arrange(desc(partition))                
                                
                                # whether to subset and use only the most recent trading days or entire historical record
                                if(xts_data_subset){x.t <- x.t %>% dplyr::filter(partition <= xts_data_subset_partition)}
                                
                                # scale features
                                x.t.scaled <- x.t %>% dplyr::mutate_at(vars(features), scale)
                                
                                # remove any nan column
                                x.t.scaled <- x.t.scaled[, which(unlist(lapply(x.t.scaled, function(x) !any(is.nan(x)))))]
                                
                                # complete.cases
                                x.t.scaled <- x.t.scaled %>% complete.cases() %.>% x.t.scaled[., ]
                                
                                # new set of features
                                features <- features[features %in% names(x.t.scaled)]
                                
                                # feature selections by t.test        
                                t.test.list <- lapply(x.t.scaled[, features], function(x) t.test(x ~ x.t.scaled$target) %>% broom::tidy())
                                t.test.df <- t.test.list %>%
                                        dplyr::bind_rows() %>%
                                        dplyr::mutate(variables = features) %>%
                                        arrange(p.value)
                                
                                IV <- t.test.df$variables[t.test.df$p.value < features_selection_threshold]
                                if(length(IV) <=2){IV = unique(c(IV, 'rsi_close', 'cci_close', 'macd_close', 'signal_close'))}
                                
                                # subset features
                                x.t.scaled2 <- x.t.scaled %>% 
                                        dplyr::select(partition, date, all_of(IV), DV) %>%
                                        dplyr::filter(partition > dfEval_subset_partition)
                                
                                # evaluation set (include all variables and TODAY values)
                                dfEval <- x.t %>% 
                                        dplyr::filter(partition <= dfEval_subset_partition) %>%
                                        dplyr::mutate_at(vars(features), scale) %>%
                                        dplyr::select(partition, date, all_of(IV), DV) %>%                
                                        dplyr::mutate_at(IV, as.vector)
                                
                                # impute dataset
                                preProcValues <- caret::preProcess(dfEval[, IV], method = c("medianImpute"))
                                dfEval <- predict(preProcValues, dfEval) %>%
                                        dplyr::mutate_at(IV, as.vector)
                                
                                # oversampling target == "Yes" by 50%
                                x.t.scaled2_Yes <- x.t.scaled2 %>% dplyr::filter(target == "Yes")
                                x.t.scaled2_IndexUp <- sample(1:nrow(x.t.scaled2_Yes), size = floor(nrow(x.t.scaled2_Yes) * 1.5), replace = TRUE)
                                x.t.scaled2_Yes <- x.t.scaled2_Yes[x.t.scaled2_IndexUp, ]
                                
                                # undersampling target == "No" by 50%
                                x.t.scaled2_No <- x.t.scaled2 %>% dplyr::filter(target == "No")
                                x.t.scaled2_IndexDown <- sample(1:nrow(x.t.scaled2_No), size = floor(nrow(x.t.scaled2_No) * 0.5), replace = FALSE)
                                x.t.scaled2_No <- x.t.scaled2_No[x.t.scaled2_IndexDown, ]
                                
                                # put them together to come up with new x.t.scaled
                                x.t.scaled2 <- dplyr::bind_rows(x.t.scaled2_Yes, x.t.scaled2_No)
                                
                                # split data into train, test sets
                                set.seed(seed + nday + lday)
                                index <- caret::createDataPartition(x.t.scaled2$target, p = size, list = FALSE)
                                trainSet <- x.t.scaled2[index, ]
                                testSet <- x.t.scaled2[-index, ] %>% distinct 
                                
                                # trainSetControl
                                trainSet.control <- caret::trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)
                                
                                # train base layer
                                for(baseLayer in baseTrainMethods){
                                        
                                        # set parameters
                                        ml = baseLayer
                                        model = paste0("model_", ml)
                                        OOF_prediction = paste0("OOF_pred_", ml)
                                        prediction = paste0("pred_", ml)
                                        result = paste0("result_", ml)
                                        
                                        # model
                                        assign(bquote(.(model)), caret::train(trainSet[, IV], trainSet[, DV], 
                                                                              method = ml, 
                                                                              trControl = trainSet.control, 
                                                                              tuneLength = tune.length))
                                        
                                        # Out-Of-Fold probability predictions - trainSet    
                                        if(ml == "glm"){trainSet$OOF_pred_glm = eval(sym(model))$pred$Y[order(eval(sym(model))$pred$rowIndex)]}
                                        if(ml == "nb"){trainSet$OOF_pred_nb = eval(sym(model))$pred$Y[order(eval(sym(model))$pred$rowIndex)]}
                                        if(ml == "gbm"){trainSet$OOF_pred_gbm = eval(sym(model))$pred$Y[order(eval(sym(model))$pred$rowIndex)]}
                                        
                                        # Out-Of-Fold probability predictions - testSet 
                                        assign(bquote(.(OOF_prediction)), predict(eval(sym(model)), testSet[, IV], type = "prob")$Y)
                                        if(ml == "glm"){testSet$OOF_pred_glm = eval(sym(OOF_prediction))}
                                        if(ml == "nb"){testSet$OOF_pred_nb = eval(sym(OOF_prediction))}
                                        if(ml == "gbm"){testSet$OOF_pred_gbm = eval(sym(OOF_prediction))}
                                        
                                        # Out-Of-Fold probability predictions - dfEval 
                                        assign(bquote(.(OOF_prediction)), predict(eval(sym(model)), dfEval[, IV], type = "prob")$Y)
                                        if(ml == "glm"){dfEval$OOF_pred_glm = eval(sym(OOF_prediction))}
                                        if(ml == "nb"){dfEval$OOF_pred_nb = eval(sym(OOF_prediction))}
                                        if(ml == "gbm"){dfEval$OOF_pred_gbm = eval(sym(OOF_prediction))}
                                        
                                        # Y/N predictions for Confusion Matrix - testSet    
                                        assign(bquote(.(prediction)), predict(eval(sym(model)), testSet[, IV]))
                                        if(ml == "glm"){testSet$pred_glm = eval(sym(prediction))}
                                        if(ml == "nb"){testSet$pred_nb = eval(sym(prediction))}
                                        if(ml == "gbm"){testSet$pred_gbm = eval(sym(prediction))}
                                        
                                        # output
                                        assign(bquote(.(result)), broom::tidy(caret::confusionMatrix(testSet[, prediction], testSet$target)) %>%
                                                       dplyr::mutate(trend = marketTrend, symbol = j, n_period = nday, l_period = lday, trainMethod = ml) %>%
                                                       dplyr::select(trend, symbol, n_period, l_period, trainMethod, everything()))
                                        
                                        # store output into a list
                                        tempModelList <- list(eval(sym(result)))
                                        modelSummaryList <<- c(modelSummaryList, tempModelList)
                                        
                                }
                                
                                # train top layer
                                for(topLayer in topTrainMethods){
                                        
                                        # set parameters
                                        ml = topLayer
                                        model = paste0("model_", ml)        
                                        OOF_predictors_top = c("OOF_pred_glm", "OOF_pred_nb", "OOF_pred_gbm")
                                        OOF_prediction_top = paste0("OOF_pred_top_", ml)
                                        prediction_top = paste0("pred_top_", ml)
                                        result = paste0("result_top_", ml)
                                        
                                        # model
                                        assign(bquote(.(model)), caret::train(trainSet[, OOF_predictors_top], trainSet[, DV], 
                                                                              method = ml, 
                                                                              trControl = trainSet.control, 
                                                                              tuneLength = tune.length))
                                        
                                        # Out-Of-Fold probability predictions - testSet 
                                        assign(bquote(.(OOF_prediction_top)), predict(eval(sym(model)), testSet[, OOF_predictors_top], type = "prob")$Y)
                                        if(ml == "glm"){testSet$OOF_pred_top_glm = eval(sym(OOF_prediction_top))}
                                        if(ml == "nb"){testSet$OOF_pred_top_nb = eval(sym(OOF_prediction_top))}
                                        if(ml == "gbm"){testSet$OOF_pred_top_gbm = eval(sym(OOF_prediction_top))}
                                        
                                        # Out-Of-Fold probability predictions - dfEval 
                                        assign(bquote(.(OOF_prediction_top)), predict(eval(sym(model)), dfEval[, OOF_predictors_top], type = "prob")$Y)
                                        if(ml == "glm"){dfEval$OOF_pred_top_glm = eval(sym(OOF_prediction_top))}
                                        if(ml == "nb"){dfEval$OOF_pred_top_nb = eval(sym(OOF_prediction_top))}
                                        if(ml == "gbm"){dfEval$OOF_pred_top_gbm = eval(sym(OOF_prediction_top))}
                                        
                                        # Y/N predictions for Confusion Matrix - testSet    
                                        assign(bquote(.(prediction_top)), predict(eval(sym(model)), testSet[, OOF_predictors_top]))
                                        if(ml == "glm"){testSet$pred_top_glm = eval(sym(prediction_top))}
                                        if(ml == "nb"){testSet$pred_top_nb = eval(sym(prediction_top))}
                                        if(ml == "gbm"){testSet$pred_top_gbm = eval(sym(prediction_top))}
                                        
                                        # output
                                        assign(bquote(.(result)), broom::tidy(caret::confusionMatrix(testSet[, prediction_top], testSet$target)) %>%
                                                       dplyr::mutate(trend = marketTrend, symbol = j, n_period = nday, l_period = lday, trainMethod = paste0(ml, " - top layer")) %>%
                                                       dplyr::select(trend, symbol, n_period, l_period, trainMethod, everything()))
                                        
                                        # store output into a list
                                        tempModelList <- list(eval(sym(result)))
                                        modelSummaryList <<- c(modelSummaryList, tempModelList)
                                        
                                }
                                
                                # put together - final averaging
                                testSet <- testSet %>%
                                        dplyr::mutate(pred_final_avg = (OOF_pred_top_glm + OOF_pred_top_nb + OOF_pred_top_gbm) / length(topTrainMethods),
                                                      pred_final = ifelse(pred_final_avg > 0.5, "Y", "N") %>%
                                                              factor(., levels = c("Y", "N"), labels = c("Yes", "No")))
                                
                                dfEval <- dfEval %>%
                                        dplyr::mutate(pred_final_avg = (OOF_pred_top_glm + OOF_pred_top_nb + OOF_pred_top_gbm) / length(topTrainMethods),
                                                      pred_final = ifelse(pred_final_avg > 0.5, 1, 0),
                                                      trend = marketTrend, symbol = j, n_period = nday, l_period = lday) %>%
                                        dplyr::select(trend, symbol, n_period, l_period, date, target, pred_final_avg, pred_final)
                                
                                finalResult <- broom::tidy(caret::confusionMatrix(testSet$pred_final, testSet$target)) %>%
                                        dplyr::mutate(trend = marketTrend, symbol = j, n_period = nday, l_period = lday, trainMethod = "final - averaging") %>%
                                        dplyr::select(trend, symbol, n_period, l_period, trainMethod, everything())
                                
                                # store output into a list
                                tempModelList <- list(finalResult)
                                modelSummaryList <<- c(modelSummaryList, tempModelList)
                                
                                tempEvalList <- list(dfEval)
                                evalList <<- c(evalList, tempEvalList)
                                
                        }
                        
                }
                
        }
        
}

# stop timer
toc()

# consolidate lists into df
modelSummaryDf <- modelSummaryList %>% dplyr::bind_rows()
evalDf <- evalList %>% dplyr::bind_rows()

############################################################
### VISUALIZATION ###

###################################
### TECHNICAL ANALYSIS ###
x <- xts::xts(xtsList[[stock]])

quantmod::chartSeries( x["201801::202005"],
                       name = stock,
                       TA = c(addBBands(draw = 'bands'), 
                              addADX(), 
                              addMACD(), 
                              addRSI(), 
                              addCCI(), 
                              addCMF(), 
                              addBBands(draw = 'width'), 
                              addBBands(draw = 'percent'), 
                              addVo()) )

###################################
# density curve
tempObj <- vector(mode = "list", length = length(n_period))

for(i in n_period){
        
        tempDf <- xtsList[[stock]] %>% 
                historicalTransformation(transformOnly = TRUE, n = i) %>%
                dplyr::mutate(percent_change = (cl_n_diff / close),
                              t_period = i,
                              tday = dplyr::case_when(t_period == 1 ~ "t plus 1 day",
                                                      t_period == 2 ~ "t plus 2 days",
                                                      t_period == 3 ~ "t plus 3 days",
                                                      t_period == 4 ~ "t plus 4 days",
                                                      t_period == 5 ~ "t plus 5 days"),
                              index = nrow(.):1) %>%
                dplyr::filter(index <= xts_data_subset_partition) %>%
                dplyr::select(date, close, cl_n_diff, percent_change, tday)
        
        tempList <- list(tempDf)
        tempObj <- c(tempObj, tempList)
        
}

tempDf <- tempObj %>% dplyr::bind_rows()

# gather data frame
dfGather <- tempDf %>%
        dplyr::select(-date) %>%
        dplyr::mutate(tday = as.factor(tday)) %>%
        tidyr::gather(key, value, -tday) 

# plot close ($) by tday
densityCurveClose <- ggplot(dfGather %>% dplyr::filter(key == "close") %>% distinct, 
                            aes(value)) +
        geom_density() +
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "close") %>% distinct, 
                                    median), 
                   aes(xintercept = value),
                   linetype = "dashed",
                   col = "red") +
        theme(legend.position = "none") +
        labs(x = "Close ($)") +
        theme_light() +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::percent) +
        ggtitle(paste0(stock, " distribution of Close ($) for past ", xts_data_subset_partition, " trading days"))

# plot cl_n_diff
densityCurveCl_N_diff <- ggplot(dfGather %>% dplyr::filter(key == "cl_n_diff"), 
                                aes(value, fill = tday)) +
        geom_density(alpha = 0.5) +
        # median
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "cl_n_diff"), 
                                    median), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dashed") +
        # cl_percentile_threshold
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "cl_n_diff"),
                                    quantile,
                                    probs = cl_percentile_threshold), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dotted") +
        # 1 - cl_percentile_threshold
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "cl_n_diff"),
                                    quantile,
                                    probs = 1 -cl_percentile_threshold), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dotted") +
        theme_minimal() +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "right") +
        labs(x = "Close ($) between t and t plus n day(s)") +
        ggtitle(paste0(stock, " distributions of difference between today (t) and future (t plus n trading day) Close ($) for past ", xts_data_subset_partition, " trading days")) +
        facet_wrap(~tday, nrow = 5)

# plot percent_change
densityCurveCl_percent_change <- ggplot(dfGather %>% dplyr::filter(key == "percent_change"), 
                                        aes(value, fill = tday)) +
        geom_density(alpha = 0.5) +
        # median
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "percent_change"), 
                                    median), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dashed") +
        # cl_percentile_threshold
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "percent_change"),
                                    quantile,
                                    probs = cl_percentile_threshold), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dotted") +
        # 1 - cl_percentile_threshold
        geom_vline(data = aggregate(value ~ tday, 
                                    dfGather %>% dplyr::filter(key == "percent_change"),
                                    quantile,
                                    probs = 1 -cl_percentile_threshold), 
                   aes(xintercept = value,
                       color = tday),
                   linetype = "dotted") +
        theme_bw() +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "right") +
        labs(x = "% diff between t and t plus n day(s)") +
        ggtitle(paste0(stock, " distributions of change in percentage between today (t) and future (t plus n trading day) Close ($) for past ", xts_data_subset_partition, " trading days")) +
        facet_wrap(~tday, nrow = 5)

###################################
# evaluation chart
# data transformation for model evaluation

# spread table
modelSummarySpread <- modelSummaryDf %>% 
        dplyr::select(trend, symbol, n_period, l_period, trainMethod, term, estimate) %>%
        tidyr::spread(term, estimate) %>%
        arrange(trend, symbol, n_period, trainMethod)

# get measures
modelSummaryMeasure <- modelSummaryDf %>%
        dplyr::filter(term %in% c("recall", "precision", "f1") & trainMethod == "final - averaging") %>%
        dplyr::select(trend, symbol, n_period, l_period, measure = term, estimate) %>%
        dplyr::mutate(lday = dplyr::case_when(l_period == 1 ~ "lag 1 day",
                                              l_period == 5 ~ "lag 5 days",
                                              l_period == 10 ~ "lag 10 days",
                                              l_period == 15 ~ "lag 15 days",
                                              l_period == 20 ~ "lag 20 days") %>%
                              factor(., levels = c("lag 1 day", "lag 5 days", "lag 10 days", "lag 15 days", "lag 20 days")))

# get accuracy from eval set
accuracyDf <- evalDf %>%
        dplyr::mutate(flag = dplyr::case_when(pred_final == 0 ~ "No", TRUE ~ "Yes"),
                      accuracy = dplyr::case_when(target == flag ~ 1, TRUE ~ 0)) %>%
        dplyr::filter(!is.na(target)) %>%
        group_by(trend, symbol, n_period, l_period) %>%
        summarise(accuracy = mean(accuracy)) %>%
        dplyr::mutate(measure = "accuracy",
                      lday = dplyr::case_when(l_period == 1 ~ "lag 1 day",
                                              l_period == 5 ~ "lag 5 days",
                                              l_period == 10 ~ "lag 10 days",
                                              l_period == 15 ~ "lag 15 days",
                                              l_period == 20 ~ "lag 20 days") %>%
                              factor(., levels = c("lag 1 day", "lag 5 days", "lag 10 days", "lag 15 days", "lag 20 days"))) %>%
        dplyr::select(trend, symbol, n_period, l_period, measure, estimate = accuracy, lday) %>%
        arrange(trend, symbol, n_period, l_period)

# combine the key measures of model summary together
keyMeasures <- dplyr::bind_rows(modelSummaryMeasure, accuracyDf) %>%
        arrange(trend, symbol, n_period, l_period, measure)

# plot evaluation chart
evalChart <- keyMeasures %>%
        dplyr::filter(symbol == stock) %>%
        ggplot(aes(n_period, estimate, fill = factor(trend))) +
        geom_bar(stat = "identity", position = "dodge2") +
        geom_hline(yintercept = 0.5, linetype = "dashed", col = "black") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        facet_grid(measure ~ lday) +
        labs(x = "Prediction for t plus n trading day(s)", y = "", fill = "trend") +
        theme_light() +
        theme(legend.position = "bottom") +
        ggtitle(paste0(stock, " Model Evaluation ", min(evalDf$date), " : ", max(evalDf$date)))

###################################
# MA charts
MA_chart <- xtsList[[stock]] %>% 
        historicalTransformation(transformOnly = TRUE) %>%
        dplyr::mutate(`5 day MA` = TTR::SMA(close, 5),
                      `20 day MA` = TTR::SMA(close, 20),
                      index = nrow(.):1) %>%
        dplyr::filter(index <= dfEval_subset_partition) %>%
        dplyr::select(date, close, `5 day MA`, `20 day MA`) %>%
        tidyr::gather(key, value, -date) %>%
        ggplot(aes(date, value)) + 
        geom_line(aes(col = key)) +
        labs(x = "", y = "") +
        scale_x_date(labels = scales::date_format("%Y-%m-%d"), date_breaks = "2 day") +
        scale_color_manual(values = c("black", "blue", "red")) +
        theme_minimal() +
        theme(legend.position = "top", 
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(hjust = 1, angle = 60),
              legend.title = element_blank()) +
        scale_y_continuous(labels = scales::dollar) +
        guides(col = guide_legend(reverse = TRUE)) +
        ggtitle(paste0(stock, " Close ($) Moving Average(s) for past ", dfEval_subset_partition, " trading days"))


###################################
# prediction chart

# data transformation for prediction
predDf <- evalDf %>%
        group_by(trend, symbol, n_period, date) %>%
        summarise(prediction = round(mean(pred_final_avg), 3)) %>% 
        dplyr::mutate(tday = dplyr::case_when(n_period == 1 ~ "t plus 1 day",
                                              n_period == 2 ~ "t plus 2 days",
                                              n_period == 3 ~ "t plus 3 days",
                                              n_period == 4 ~ "t plus 4 days",
                                              n_period == 5 ~ "t plus 5 days")) %>%
        ungroup %>%
        dplyr::select(-n_period)

# get signal dates
signalDate <- predDf %>%
        dplyr::select(trend, symbol, date, prediction, tday) %>%
        tidyr::spread(tday, prediction) %>%
        dplyr::mutate(`t plus 1 day` = dplyr::case_when(`t plus 1 day` > 0.5 ~ 1, TRUE ~ 0),
                      `t plus 2 days` = dplyr::case_when(`t plus 2 days` > 0.5 ~ 1, TRUE ~ 0),
                      `t plus 3 days` = dplyr::case_when(`t plus 3 days` > 0.5 ~ 1, TRUE ~ 0),
                      `t plus 4 days` = dplyr::case_when(`t plus 4 days` > 0.5 ~ 1, TRUE ~ 0),
                      `t plus 5 days` = dplyr::case_when(`t plus 5 days` > 0.5 ~ 1, TRUE ~ 0),
                      flag = `t plus 1 day` + `t plus 2 days` + `t plus 3 days` + `t plus 4 days` + `t plus 5 days`) %>%
        dplyr::select(trend, symbol, date, flag) %>%
        tidyr::spread(trend, flag) %>%
        dplyr::mutate(signal = dplyr::case_when(bearish <= 2 & bullish == 5 ~ 1,
                                                bearish == 5 & bullish <= 2 ~ 1,
                                                TRUE ~ 0)) %>%
        dplyr::filter(signal == 1) %>% 
        dplyr::select(-signal) %>%
        tidyr::gather(trend, value, -symbol, -date) %>%
        dplyr::filter(value == 5) %>%
        dplyr::select(-value)

# plot prediction chart
predChart <- predDf %>%
        dplyr::filter(symbol == stock) %>%
        ggplot(aes(x = date, y = prediction, col = trend)) +
        geom_line() +
        geom_hline(yintercept = 0.5, linetype = "dashed", col = "black") +
        geom_vline(aes(xintercept = date, col = trend), 
                   data = signalDate %>% dplyr::filter(symbol == stock),
                   linetype = "dashed") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        scale_x_date(labels = scales::date_format("%Y-%m-%d"), 
                     date_breaks = "2 day", 
                     limits = c(min(predDf$date), max(predDf$date))) +
        facet_wrap(~ tday, nrow = 5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.position = "bottom") +
        labs(x = "", y = "") +
        ggtitle(paste0(stock, " Trend Prediction ", min(predDf$date), " : ", max(predDf$date)))

###################################
### PLOTLY ###

densityCurveClose %>% ggplotly()

densityCurveCl_N_diff %>% ggplotly()

densityCurveCl_percent_change %>% ggplotly()

evalChart %>% ggplotly()

MA_chart %>% ggplotly()

predChart  # vertical dashlines would disappear if we convert it into a plotly object, thus not recommend

###################################
### END ###
# stop the cluster
parallel::stopCluster(cl)