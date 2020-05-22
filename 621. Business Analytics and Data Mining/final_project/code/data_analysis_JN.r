########################################
########### data preparation ###########
########################################

# load packages
if(!require(pacman)){install.packages("pacman"); require(pacman)}
packages <- c("tidyverse", "sqldf", "broom", "Hmisc", "SASxport", "janitor", "sqldf", "ggmosaic", "htmlwidgets", "wrapr", "plotly")
pacman::p_load(char = packages)

# get files
surveyNames <- grep(pattern = ".XPT", dir(), ignore.case = TRUE, value = TRUE) %>% 
        lapply(., function(x) gsub(".XPT", "", .)) %>% unlist %>% unique
files <- grep(pattern = ".XPT", dir(), ignore.case = TRUE, value = TRUE)

# read XPT data
for(i in 1:length(files)){
        assign(bquote(.(surveyNames[i])), value = read.xport(files[i]))
}

# combine togther
df <- DPQ_J %>%
        inner_join(HSQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(OCQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(SLQ_J, by = c("SEQN", "SEQN")) %>%
        inner_join(SMQ_J, by = c("SEQN", "SEQN"))

# clean up field names
df <- df %>% janitor::clean_names()

# meta data
metaData <- data.frame(fields = names(df), 
                       missing = colSums(is.na(df)),
                       unique = sapply(df, function(x) length(unique(x)))) %>%
        dplyr::mutate(attribute = stringr::str_extract_all(fields, pattern = "^[a-z]+") %>% unlist)

# get a list of variables that do not have significant number of missing values
fields <- metaData %>%
        dplyr::filter(missing < 1000) %>%
        .$fields %>%
        as.character

# subset fields
df <- df %>%
        dplyr::select(fields)

# output data for downstream analysis
write.csv(df, "df.csv", row.names = FALSE)

# run meta data again
metaDataUpdated <- data.frame(fields = names(df), 
                       missing = colSums(is.na(df)),
                       unique = sapply(df, function(x) length(unique(x))),
                       type = sapply(df, class)) %>%
        dplyr::mutate(attribute = stringr::str_extract_all(fields, pattern = "^[a-z]+") %>% unlist)

##################################
########### odds ratio ###########
##################################

# set variables
variables <- metaDataUpdated %>%
        dplyr::filter(attribute %in% c(metaDataUpdated$attribute[metaDataUpdated$attribute %nin% c("seqn", "hsaquex", "smaquex")] %>% unique)) %>% 
        dplyr::select(fields) %>%
        .$fields %>%
        as.character

groupingVariable = "hsd010"

# transform data into long format
dfGather <- df %>%
        tidyr::gather(., fields, value, -seqn, -groupingVariable) %>%
        dplyr::inner_join(metaDataUpdated, by = c("fields", "fields"))

dfGathered <- dfGather[complete.cases(dfGather), ] %>%
        dplyr::select(attribute = fields, 
                      value,
                      class = bquote(.(groupingVariable))) %>%
        dplyr::filter(attribute %in% variables) %>%
        dplyr::filter(class <=5) %>%
        dplyr::mutate(class = case_when(class <=3 ~ 1,
                                        TRUE ~ 0))

# calculate odds ratio using sql
OR <- sqldf::sqldf("
        select attribute 
        , value 
        , interest_group_yes
        , control_group_yes
        , interest_group_no
        , control_group_no
        from (
                
                select x.attribute 
                , x.value
                , x.interest_group_yes
                , x.control_group_yes
                , y.interest_group_total - x.interest_group_yes as interest_group_no
                , y.control_group_total - x.control_group_yes as control_group_no
                from (
                        select attribute
                        , value
                        , sum(case when class = 1 then 1 else 0 end) as interest_group_yes
                        , sum(case when class = 0 then 1 else 0 end) as control_group_yes
                        from dfGathered
                        group by 1, 2
                ) x
                join (
                        select attribute
                        , sum(case when class = 1 then 1 else 0 end) as interest_group_total
                        , sum(case when class = 0 then 1 else 0 end) as control_group_total
                        from dfGathered
                        group by 1
                ) y on x.attribute = y.attribute
                
        ) z
        where interest_group_yes >= 25
        and interest_group_no >= 25
        and control_group_yes >= 25
        and control_group_no >= 25
        "
)

# Odds Ratio
ORplusCI <- OR %>%
        dplyr::mutate(OR = round((interest_group_yes / interest_group_no) / (control_group_yes / control_group_no), 2))

# Lower 95% CI
Lower_CI = with(ORplusCI, 
                exp(1) ^(log(OR) - 
                                 (1.96 * sqrt(1/interest_group_yes + 
                                                      1/interest_group_no + 
                                                      1/control_group_yes + 
                                                      1/control_group_no))))

# Upper 95% CI
Upper_CI = with(ORplusCI, 
                exp(1) ^(log(OR) + 
                                 (1.96 * sqrt(1/interest_group_yes + 
                                                      1/interest_group_no + 
                                                      1/control_group_yes + 
                                                      1/control_group_no))))

# display only significant results and sort by OR
ORplusCI <- ORplusCI %>%
        dplyr::mutate(lower_ci = round(Lower_CI, 2),
                      upper_ci = round(Upper_CI, 2),
                      survey = stringr::str_extract_all(attribute, pattern = "^[a-z]+") %>% unlist) %>%
        dplyr::filter(Upper_CI >1 & Lower_CI >1) %>%
        dplyr::select(survey, item = attribute, value, odds_ratio = OR, lower_ci, upper_ci) %>%
        arrange(desc(odds_ratio))

#####################################
########### data analysis ###########
#####################################

### data transformation ###

# depression score
depressionScore <- df %>%
        dplyr::select_at(vars(contains("seqn"), starts_with("dpq"))) %>%
        dplyr::filter_at(vars(starts_with("dpq")), all_vars(. <=3)) %>%
        dplyr::mutate(depressionScore = rowSums(select(., starts_with("dpq")))) %>%
        dplyr::select(seqn, depressionScore)

# average daily hours of sleep
sleepHrs <- df %>%
        dplyr::select(seqn, sld012, sld013) %>%
        dplyr::filter(!is.na(sld012) & !is.na(sld013)) %>%
        dplyr::mutate(sleepHrs = (sld012 + sld013) / 2) %>%
        dplyr::select(seqn, sleepHrs)

# health status
healthStatus <- df %>%
        dplyr::select(seqn, hsd010) %>%
        dplyr::filter(hsd010 <= 5) %>%
        dplyr::mutate(healthStatus = dplyr::case_when(hsd010 == 1 ~ "Excellent",
                                                      hsd010 == 2 ~ "Very good",
                                                      hsd010 == 3 ~ "Good",
                                                      hsd010 == 4 ~ "Fair",
                                                      hsd010 == 5 ~ "Poor") %>%
                              factor(., levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))) %>%
        dplyr::select(seqn, healthStatus)

# sleepiness
sleepiness <- df %>%
        dplyr::select(seqn, slq120) %>%
        dplyr::filter(slq120 <= 4) %>%
        dplyr::mutate(sleepiness = dplyr::case_when(slq120 == 0 ~ "Never",
                                                    slq120 == 1 ~ "Rarely",
                                                    slq120 == 2 ~ "Sometimes",
                                                    slq120 == 3 ~ "Often",
                                                    slq120 == 4 ~ "Almost always") %>%
                              factor(., levels = c("Never", "Rarely", "Sometimes", "Often", "Almost always"))) %>%
        dplyr::select(seqn, sleepiness)

#################################################################################
### data analysis using one-way subject anova

#################################################################################
# null hypothesis 1) there is no relationship between depression and sleepiness #

# get data
d1 <- merge(depressionScore, sleepiness) 

# one-way subject anova
d1_aov <- aov(depressionScore ~ sleepiness, data = d1)
summary(d1_aov)

# post-hoc test
TukeyHSD(d1_aov)

#############################################################################################
# null hypothesis 2) there is no relationship between depression and physical health status #

# get data
d2 <- merge(depressionScore, healthStatus) 

# one-way subject anova
d2_aov <- aov(depressionScore ~ healthStatus, data = d2)
summary(d2_aov)

# post-hoc test
TukeyHSD(d2_aov)

#################################################################################
### data analysis using chisq.test

#############################################################################################
# null hypothesis 3) there is no relationship between sleepiness and physical health status #

# get data
d3 <- merge(sleepiness, healthStatus)

# cross tab
round(prop.table(ftable(d3$sleepiness, d3$healthStatus), 2), 2)

# chisq.test
chisq.test(table(d3$healthStatus, d3$healthStatus))

##########################################
########### data visualization ###########
##########################################

# bar chart on odds ratio 
OR_bar <- ORplusCI %>%
        dplyr::filter(survey != "dpq") %>%
        dplyr::filter(odds_ratio >1.5) %>%
        select(survey, item, value, odds_ratio) %>%
        dplyr::mutate(survey = dplyr::case_when(survey == "hsq" ~ "General Health",
                                                survey == "slq" ~ "Sleep Behaviors",
                                                survey == "sld" ~ "Sleep Behaviors",
                                                survey == "ocd" ~ "Occupation",
                                                survey == "smq" ~ "Smoke Habit")) %>%
        arrange(item, value)
        
OR_bar$label <- c('have not had a cold in the past 30 days', 
                  'have not had a stomach or intestinal illness in the past 30 days', 
                  'have not had a flu, pneumonia, or ear infections in the past 30 days', 
                  'have donated blood in the past 12 months', 
                  'work at a job or business', 
                  'current job is the longest position that one has ever held', 
                  'average hours of sleep during weekdays or workdays is 7.5 hrs', 
                  'average hours of sleep during weekends is 7.5 hrs', 
                  'average hours of sleep during weekends is 8.5 hrs', 
                  'never snore while sleeping', 
                  'never snort or stop breathing while asleep', 
                  'never needed to seek professional help for trouble sleeping', 
                  'rarely feel overly sleepy during day', 
                  'ususal sleep time on weekdays or workdays is 22:30', 
                  'ususal sleep time on weekdays or workdays is 23:30', 
                  'ususal wake time on weekdays or workdays is 6:30', 
                  'ususal wake time on weekdays or workdays is 7:30', 
                  'ususal sleep time on weekends is 23:30', 
                  'smoke less than 100 cigarettes in lifetime')

OR_bar_chart <- OR_bar %>%
        ggplot(., aes(reorder(label, odds_ratio), odds_ratio)) +
        geom_bar(stat = "identity", aes(fill = survey)) + 
        xlab("") +
        ylab("Odds Ratio") +
        coord_flip() +
        theme_minimal() +
        geom_text(aes(x = label, y = odds_ratio, label = round(odds_ratio, 2)),
                  position = position_dodge(width = 1), hjust = 1.5,
                  col = "white", fontface = "bold", size = 3) +
        ggtitle("Features that are significantly important pertaining to healthy individuals")

# distribution of hours of sleep by health status
sleep_by_healthStatus <- merge(sleepHrs, healthStatus) %>% 
        ggplot(., aes(sleepHrs, fill = healthStatus)) +
        geom_density() +
        geom_vline(data = aggregate(sleepHrs ~ healthStatus, data = merge(sleepHrs, healthStatus), FUN = median),
                   aes(xintercept = sleepHrs,
                       color = healthStatus),
                   linetype = "dashed") +
        xlab("Hours of Sleep") +
        theme_classic() +
        ggtitle("Hours of sleep varied by health condition")

# boxplot on depression varied by sleepiness
depression_by_sleepiness <- d1 %>%
        ggplot(., aes(sleepiness, depressionScore, fill = sleepiness)) +
        geom_boxplot() +
        theme_bw() +
        xlab("How often feel overly sleepy during day?") +
        ylab("Depression Score") +
        theme(legend.position = "none") +
        ggtitle("Depression varied by sleepiness")

# boxplot on depression varied by health status
depression_by_healthStatus <- d2 %>%
        ggplot(., aes(healthStatus, depressionScore, fill = healthStatus)) +
        geom_boxplot() +
        theme_minimal() +
        xlab("General Health Condition") +
        ylab("Depression Score") +
        theme(legend.position = "none") +
        ggtitle("Depression varied by health condition")

# mosaic plot - sleepiness x health status
sleepiness_by_healthStatus <- ggplot(data = d3) +
        geom_mosaic(aes(x = product(sleepiness, healthStatus), fill = sleepiness), na.rm = TRUE) + 
        labs(x = "General Health Condition", y = "How often feel overly sleepy during day?") +
        theme_minimal() +
        ggtitle("Sleepiness during day varied by health condition")

################# save output in png and html #################
# chart objects
chartObjs <- list(OR_bar_chart, sleep_by_healthStatus, depression_by_sleepiness, depression_by_healthStatus, sleepiness_by_healthStatus)

names(chartObjs) <- wrapr::qc(OR_bar_chart, sleep_by_healthStatus, depression_by_sleepiness, depression_by_healthStatus, sleepiness_by_healthStatus)

# save as png
sapply(1:length(chartObjs), function(x) ggsave(filename = paste0(names(chartObjs[x]), ".png"), 
                                               plot = chartObjs[[x]])) %>% 
        invisible()

# save as html
sapply(1:length(chartObjs), function(x) htmlwidgets::saveWidget(widget = as_widget(chartObjs[[x]] %>% ggplotly),
                                                                file = paste0(names(chartObjs[x]), ".html"))) %>%
        invisible()







