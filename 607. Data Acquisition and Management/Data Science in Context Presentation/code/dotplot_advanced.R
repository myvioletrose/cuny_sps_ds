### Objective ###

# Suppose we work in a global ecommerce company. We have brands that can sell/ship to multiple global destinations. We can calculate an Average Order Value (AOV) by country and by brand. Here, there's a dataset that has only three columns, i.e. "brand", "country" and "aov". 

# The global marketing team wants to know the brand performance by country. Put it this way, how can we visually summarize the AOV by country for each brand in this data set? For each brand (or country), can we visually display and group countries with similar AOV (or any KPI) together in one chart? Suppose "brand 27" is sold to 100+ countries, can we visually present and cluster the markets together by similar AOV? Alternatively, over 20+ brands are shipping to Bermuda, which ones are having higher AOV?
 
# We can apply a for-loop and create an advanced dotplot using ggplot2 and ggrepel to solve the above questions.

###############################################################################################
# load sample df

### usually, we would connect to a database and extract data using sql like this one below ###
# select md.financial_entity_name
# , md.merch_name
# , md.merch_id
# , c.country_name as country
# , sum(o.mv) as mv
# , round(sum(o.mv) / count(o.oh_order_id), 1) as aov
# from agg.order_fact_totals o
# join dw.merchant_dim md on o.oh_merch_id = md.merch_id and md.ignore = 0 and md.date_to = '2199-12-31'
# join dw.country_dim c on o.shipping_country_key = c.country_key
# where o.ignore = 0
# and o.oh_checkout_status = 'GREEN'
# and o.oh_created_date_key between 20160101 and 20161231
# group by 1, 2, 3, 4
# order by 1, 2, 4

setwd("../"); setwd("data")
load("sample_df.rda")
View(df)

# load libraries
library(dplyr)
library(ggrepel)
library(ggplot2)

#########################
######### brand #########
#########################
setwd("../"); setwd("figure")
if(any(dir() == "brand")){unlink("brand", recursive = T)}; {dir.create("brand")}
setwd("brand")

# split by brand
df_brand_split <- split(df, df$brand)

# for loop - build and save chart for each brand
for(i in 1:length(df_brand_split)){
        x <- ggplot(df_brand_split[[i]], 
                    aes(x = reorder(country, aov), y = aov)) +
                labs(x = "Country", y = "AOV") +
                geom_point(stat = "identity", color = 'grey', aes(fill = factor(aov))) + 
                coord_flip() +
                geom_label_repel(aes(country, aov, fill = factor(aov), label = country),
                                 size = 4, fontface = 'bold', color = 'white', 
                                 box.padding = unit(0.5, "lines"),
                                 point.padding = unit(0.5, "lines"), 
                                 segment.color = 'grey50', segment.size = 1.5,
                                 force = 5) +
                # only display AOV values above the median to avoid showing too many numbers in one chart
                geom_text_repel(aes(label = ifelse(aov > median(aov), aov, '')), 
                                size = 4, color = 'red',
                                force = 2) +
                theme(legend.position = "none", 
                      axis.text.x = element_text(angle = 60, hjust = 1),
                      plot.title = element_text(hjust = 0.5)) +
                ggtitle(paste(unique(df_brand_split[[i]]$brand), "AOV by Country", sep = " : "))
        
        ggsave(file = paste(unique(df_brand_split[[i]]$brand), "_country_AOV.png", sep = ""),
               x, width = 15, height = 12)        
}


###########################
######### country #########
###########################
setwd("../")
if(any(dir() == "country")){unlink("country", recursive = T)}; {dir.create("country")}
setwd("country")

# split by country
df_country_split <- split(df, df$country)

# for loop - build and save chart for each country
for(i in 1:length(df_country_split)){
        x <- ggplot(df_country_split[[i]], 
                    aes(x = reorder(brand, aov), y = aov)) +
                labs(x = "Brand", y = "AOV") +
                geom_point(stat = "identity", color = 'grey', aes(fill = factor(aov))) + 
                coord_flip() + 
                geom_label_repel(aes(brand, aov, fill = factor(aov), label = brand),
                                 size = 4, fontface = 'bold', color = 'white', 
                                 box.padding = unit(0.5, "lines"),
                                 point.padding = unit(0.5, "lines"), 
                                 segment.color = 'grey50', segment.size = 1.5,
                                 force = 5) +
                # only display AOV values above the median to avoid showing too many numbers in one chart
                geom_text_repel(aes(label = ifelse(aov > median(aov), aov, '')), 
                                size = 4, color = 'red',
                                force = 2) +
                theme(legend.position = "none", 
                      axis.text.x = element_text(angle = 60, hjust = 1),
                      plot.title = element_text(hjust = 0.5)) +
                ggtitle(paste(unique(df_country_split[[i]]$country), "AOV by Brand", sep = " : "))
        
        ggsave(file = paste(unique(df_country_split[[i]]$country), "_brand_AOV.png", sep = ""),
               x, width = 15, height = 12)        
}
