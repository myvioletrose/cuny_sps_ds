# load packages
library(tidyverse)
library(sqldf)
library(shiny)

# # load data 
# load("data_ETL.Rda")

# extract and join data from "job_position" and "company" tables
j <- sqldf::sqldf("
select c.no_of_stars
, c.company_industry
, j.fcc_unique_id
, j.skill
from company c 
join job_position j on c.company_id = j.company_id
where c.no_of_stars is not null 
") %>%
        dplyr::filter(str_length(skill) >1 & str_length(company_industry) >1) %>%
        dplyr::mutate(skill = stringr::str_extract_all(skill,
                                                       pattern =  "(?!')((?:''|[^'])*)(?=(',)|(']))")) %>%
        unnest(skill, .id = "fcc_unique_id") %>%
        dplyr::select(no_of_stars, company_industry, skill)

# Define UI app ----
ui <- fluidPage(
        
        # App title ----
        titlePanel(em("Counting of Skills by Company Rating")),
        
        # Vertical layout with input and output definitions ----
        verticalLayout(
                
                sidebarPanel(
                        uiOutput("Out1"),
                        selectInput("industry", "Industry:",
                                    unique(j$company_industry) %>% sort),
                        sliderInput("rating", h3("Company Rating"),
                                    min = 1, max = 5, value = c(1, 5), step = 0.1)
                )
                
        ),
        
        # Main panel for displaying output ----
        mainPanel(
                conditionalPanel("input.choice === 'Industry'", plotOutput("plot")),
                conditionalPanel("input.choice === 'Overall'", plotOutput("plot2"))
        )
)

# Define server logic to display a selected plot ----
server <- function(input, output) {
        
        # Conditional Panel
        output$Out1 <- renderUI({
                radioButtons(
                        "choice",
                        "Would you like to see the output by industry or overall?",
                        choices = c("Overall", "Industry"),
                        selected = "Overall")
        })
        
        # Generate a plot by industry based on input ----
        output$plot <- renderPlot({
                
                j %>%
                        dplyr::filter(no_of_stars >= min(input$rating) & 
                                              no_of_stars <= max(input$rating) &
                                              company_industry == input$industry) %>%
                        dplyr::select(skill) %>%
                        group_by(skill) %>%
                        dplyr::summarise(n = n()) %>%
                        arrange(desc(n)) %>%
                        head(15) %>%
                        dplyr::mutate(skill = fct_reorder(skill, n)) %>%
                        ggplot(., aes(x = skill, y = n)) +
                        geom_bar(stat = "identity", aes(fill = skill)) + 
                        theme(legend.position = "none") +
                        coord_flip() +
                        labs(x = "", y = "jobs") +
                        ggtitle(paste("Top Skills in ", input$industry, sep = ""))
                
        })
        
        # Generate an overall plot based on input ----
        output$plot2 <- renderPlot({
                
                j %>%
                        dplyr::filter(no_of_stars >= min(input$rating) & 
                                              no_of_stars <= max(input$rating)) %>%
                        dplyr::select(skill) %>%
                        group_by(skill) %>%
                        dplyr::summarise(n = n()) %>%
                        arrange(desc(n)) %>%
                        head(15) %>%
                        dplyr::mutate(skill = fct_reorder(skill, n)) %>%
                        ggplot(., aes(x = skill, y = n)) +
                        geom_bar(stat = "identity", aes(fill = skill)) + 
                        theme(legend.position = "none") +
                        coord_flip() +
                        labs(x = "", y = "jobs") +
                        ggtitle("Overall Top Data Science Skills")
                
        })
        
}

# Create Shiny app ----
# shinyApp(ui, server)
shinyApp(ui, server, options = list(height = 700, width = 1000))




