setwd("C:/Users/traveler/Desktop/SPS/607. Data Acquisition and Management/Projects/Project2/code/data_set_3")

library(sf)  # must load first 
library(tidyverse)  # then load this one before choroplethr
library(choroplethrMaps)
library(choroplethr)
library(knitr)
library(RCurl)
library(ggrepel)
library(gridExtra)
library(sqldf)
library(shiny)
library(kableExtra)

# load file for data set 3
data3_url <- "https://raw.githubusercontent.com/myvioletrose/school_of_professional_studies/master/607.%20Data%20Acquisition%20and%20Management/Projects/Project2/data/unemployment.csv"

data3 <- data3_url %>% RCurl::getURL(.) %>% textConnection(.) %>% 
        read.csv(., stringsAsFactors = F, header = T)

# # before tidying
# head(data3) %>% kable %>% 
#         kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
#                       full_width = F, latex_options = "scale_down")

# let's just extract the FIPS, state, area name and all unemployment rates
# names(data3) %>% data.frame %>% arrange((.))
colKeep <- c("FIPStxt", "State", "Area_name", names(data3) %>% grep("Unemployment_rate_", x = ., value = T))
d3Tidy <- data3 %>%
        dplyr::select(colKeep) %>%
        # create a county_flag and remove state abbreviation from the Area_name
        dplyr::mutate(county_flag = ifelse( str_detect(Area_name, ", "), 1, 0 ), 
                      area_name = str_replace_all(Area_name, ", [[:upper:]]{2}", "")) %>%
        dplyr::select(FIPStxt, State, county_flag, area_name, starts_with("Unemployment")) %>%
        # gather the unemployment rate columns
        tidyr::gather(., year, unemployment_rate, 
                      starts_with("Unemployment")) %>%
        # extract year
        dplyr::mutate(year = str_extract(year, "[[:digit:]]{4}") %>% as.integer)

names(d3Tidy) <- tolower(names(d3Tidy))

###################################################################

d3Map <- d3Tidy %>% 
        dplyr::filter(county_flag == 1 & state != "PR") %>%
        dplyr::select(year, state, fipstxt, unemployment_rate) %>%
        dplyr::rename("region" = fipstxt, "value" = unemployment_rate) %>%
        dplyr::filter(!is.na(value))

# Define UI for miles per gallon app ----
ui <- fluidPage(
        
        # App title ----
        titlePanel("US Unemployment Rate, 2007 - 2017"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # Input: Selector for variable to plot ----
                        selectInput("year", 
                                    "Year:",
                                    c(unique(d3Map$year)),
                                    selected = 2017
                        ), 
                        
                        selectInput("state", 
                                    "State:",
                                    c(unique(d3Map$state)),
                                    selected = "NY"
                        )
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        # add tabs
                        tabsetPanel(
                                # Outputs: ----
                                tabPanel("US Unemployment Rate (%)", plotOutput("USMap")),
                                tabPanel("State Unemployment Rate (%)", plotOutput("StateMap")),
                                tabPanel("State Unemployment Rate (%) - table", tableOutput("StateMapTable"))
                        )
                )
        )
)

# Define server logic to plot various variables ----
server <- function(input, output) {

        stateInput <- reactive({
                switch(input$state,
                       'AL' = 'alabama',
                       'AK' = 'alaska',
                       'AZ' = 'arizona',
                       'AR' = 'arkansas',
                       'CA' = 'california',
                       'Co' = 'colorado',
                       'CT' = 'connecticut',
                       'DE' = 'delaware',
                       'DC' = 'district of columbia',
                       'FL' = 'florida',
                       'GA' = 'georgia',
                       'HI' = 'hawaii',
                       'ID' = 'idaho',
                       'IL' = 'illinois',
                       'IN' = 'indiana',
                       'IA' = 'iowa',
                       'KS' = 'kansas',
                       'KY' = 'kentucky',
                       'LA' = 'louisiana',
                       'ME' = 'maine',
                       'MD' = 'maryland',
                       'MA' = 'massachusetts',
                       'MI' = 'michigan',
                       'MN' = 'minnesota',
                       'MS' = 'mississippi',
                       'MO' = 'missouri',
                       'MT' = 'montana',
                       'NE' = 'nebraska',
                       'NV' = 'nevada',
                       'NH' = 'new hampshire',
                       'NJ' = 'new jersey',
                       'NM' = 'new mexico',
                       'NY' = 'new york',
                       'NC' = 'north carolina',
                       'ND' = 'north dakota',
                       'OH' = 'ohio',
                       'OK' = 'oklahoma',
                       'OR' = 'oregon',
                       'PA' = 'pennsylvania',
                       'RI' = 'rhode island',
                       'SC' = 'south carolina',
                       'SD' = 'south dakota',
                       'TN' = 'tennessee',
                       'TX' = 'texas',
                       'UT' = 'utah',
                       'VT' = 'vermont',
                       'VA' = 'virginia',
                       'WA' = 'washington',
                       'WV' = 'west virginia',
                       'WI' = 'wisconsin',
                       'WY' = 'wyoming'
                )
        })

        US_data_Input <- reactive({
                d3Map %>% 
                        dplyr::filter(year == input$year) %>%
                        dplyr::select(region, value)
                })
        
        State_data_Input <- reactive({
                d3Map %>% 
                        dplyr::filter(year == input$year & state == input$state) %>%
                        dplyr::select(region, value)
        })
        
        # Generate plots of the requested variable ----
        # US Map
        output$USMap <- renderPlot({
                county_choropleth(US_data_Input(),
                                  title = paste("United States", input$year),
                                  legend = "%",
                                  num_colors = 1)
                })

        # State Map
        output$StateMap <- renderPlot({
                county_choropleth(State_data_Input(),
                                  title = paste(stringr::str_to_title(stateInput()), input$year),
                                  legend = "%",
                                  state_zoom = stateInput(),
                                  num_colors = 1)
                })

        # print table output
        output$StateMapTable <- renderTable({
                d3Tidy %>%
                        dplyr::filter(county_flag == 1 & state != "PR" &
                                              year == input$year & state == input$state) %>%
                        dplyr::select(area_name, unemployment_rate) %>%
                        dplyr::rename("county" = area_name) %>%
                        dplyr::filter(!is.na(unemployment_rate)) %>%
                        arrange(desc(unemployment_rate))
                })

}

# Create Shiny app ----
shinyApp(ui, server, options = list(height = 1200, width = 1500))




        
        