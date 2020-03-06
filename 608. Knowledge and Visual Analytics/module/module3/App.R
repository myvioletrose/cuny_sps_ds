# load packages
library(tidyverse)
library(ggrepel)
library(shiny)
library(wrapr)

# load df
data_url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"

df <- read.csv(data_url)

# split df by disease
dfList <- df %>% arrange(ICD.Chapter, State, Year) %>% split(., .$ICD.Chapter)

# assign global variables by disease
list2env(dfList, envir = .GlobalEnv) %>% invisible

# Define UI
ui <- fluidPage(
        
        # App title 
        titlePanel("ICD Crude Mortality Rate"),
        
        # Sidebar layout with input and output definitions 
        verticalLayout(
                
                # Sidebar panel for inputs 
                sidebarPanel(
                        
                        # Input: Selector for variable to plot                         
                        selectInput("icd", 
                                    "ICD Chapter:",
                                    c(names(dfList))
                        ),
                        
                        selectInput("state", 
                                    "State (for Q2 only):",
                                    c(levels(df$State)),
                                    selected = "NY"
                        )                        
                        
                ),
                
                # Main panel for displaying outputs 
                mainPanel(
                        # add tabs
                        tabsetPanel(
                                # Outputs: 
                                tabPanel("Crude Mortality Rate by State Y2010 - plot (Q1)", plotOutput("statePlot")),
                                tabPanel("Crude Mortality Rate by State Y2010 - table (Q1)", tableOutput("stateTable")),
                                tabPanel("Crude Mortality Rate by State, Year (Q2)", plotOutput("StateYearPlot"))
                        )
                )
        )
)

# Define server logic to plot various variables 
server <- function(input, output) {
        
        datasetInput <- reactive({
                switch(input$icd,
                       "Certain conditions originating in the perinatal period" = `Certain conditions originating in the perinatal period`, 
                       "Certain infectious and parasitic diseases" = `Certain infectious and parasitic diseases`, 
                       "Codes for special purposes" = `Codes for special purposes`,
                       "Congenital malformations, deformations and chromosomal abnormalities" = `Congenital malformations, deformations and chromosomal abnormalities`, 
                       "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = `Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism`, 
                       "Diseases of the circulatory system" = `Diseases of the circulatory system`, 
                       "Diseases of the digestive system" = `Diseases of the digestive system`, 
                       "Diseases of the ear and mastoid process" = `Diseases of the ear and mastoid process`,
                       "Diseases of the genitourinary system" = `Diseases of the genitourinary system`, 
                       "Diseases of the musculoskeletal system and connective tissue" = `Diseases of the musculoskeletal system and connective tissue`, 
                       "Diseases of the nervous system" = `Diseases of the nervous system`, 
                       "Diseases of the respiratory system" = `Diseases of the respiratory system`, 
                       "Diseases of the skin and subcutaneous tissue" = `Diseases of the skin and subcutaneous tissue`, 
                       "Endocrine, nutritional and metabolic diseases" = `Endocrine, nutritional and metabolic diseases`, 
                       "External causes of morbidity and mortality" = `External causes of morbidity and mortality`, 
                       "Mental and behavioural disorders" = `Mental and behavioural disorders`, 
                       "Neoplasms" = `Neoplasms`, 
                       "Pregnancy, childbirth and the puerperium" = `Pregnancy, childbirth and the puerperium`, 
                       "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = `Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified`
                )
        })
        
        # Generate a plot of the requested variable 
        # statePlot (Q1)
        output$statePlot <- renderPlot({
                
                ggplot(datasetInput() %>% dplyr::filter(Year == 2010),
                       aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
                        labs(x = "State", y = "Crude Mortality Rate") +                        
                        geom_point(stat = "identity", color = "grey",
                                   aes(fill = factor(Crude.Rate))) +
                        coord_flip() +
                        geom_label_repel(aes(State, Crude.Rate, fill = factor(Crude.Rate),
                                             label = State),
                                         size = 3, fontface = 'bold', color = 'white',
                                         box.padding = unit(0.1, "lines"),
                                         point.padding = unit(0.1, "lines"),
                                         segment.color = 'grey50', segment.size = 1,
                                         force = 10) +
                        geom_text_repel(aes(label = Crude.Rate),
                                        size = 3.5, color = 'red',
                                        force = 30) +
                        theme_bw() +
                        theme(legend.position = "none",
                              axis.text.x = element_text(angle = 0, hjust = 1),
                              plot.title = element_text(hjust = 0.5)) +
                        ggtitle(paste0(input$icd, ": 2010"))
                
        })
        
        # stateTable (Q1)
        output$stateTable <- renderTable({          
                datasetInput() %>% 
                        dplyr::filter(Year == 2010) %>%
                        dplyr::select(-ICD.Chapter) %>%
                        dplyr::arrange(desc(Crude.Rate)) %>%
                        dplyr::mutate(Rank = 1:nrow(.)) %>%
                        dplyr::select(Rank, State, Deaths, Population, `Crude Mortality Rate` = Crude.Rate)
                
        })
        
        # StateYearPlot (Q2)
        output$StateYearPlot <- renderPlot({
                
                nationalAverage <- datasetInput() %>%
                        dplyr::select(Year, Deaths, Population) %>%
                        group_by(Year) %>%                         
                        summarise(d = sum(Deaths), 
                                  p = sum(as.numeric(Population))) %>% 
                        dplyr::mutate(Avg = round((d / p) * 10^5, 1)) %>% 
                        dplyr::select(Year, Avg)
                
                datasetInput() %>%
                        dplyr::filter(State == input$state) %>%
                        ggplot(., aes(x = State, y = Crude.Rate)) +
                        geom_bar(stat = "identity", aes(fill = Year)) +
                        labs(x = "", y = "crude mortality rate") +                        
                        ggtitle(paste0(input$state, ": ", input$icd, "\n**Red line indicated national average**")) +
                        theme_bw() +
                        theme(legend.position = "none",
                              axis.text.x = element_text(angle = 0, hjust = 1),
                              plot.title = element_text(hjust = 0.5)) +
                        facet_grid(~Year) +
                        geom_hline(data = nationalAverage, aes(yintercept = Avg, color = "red", linetype = "longdash"))
                
        })        
        
}

# run Shiny app 
shinyApp(ui, server, options = list(width = 2400, height = 2100))