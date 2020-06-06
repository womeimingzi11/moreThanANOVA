#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Package for Shiny
library(shiny)
library(shinythemes)
library(DT)

# Package for data manipulation
library(tidyverse)
# library(vegan)
# library(ggvegan)


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(sidebarPanel(
        radioButtons(
            'data_source',
            'Upload files or try the demo',
            choices = c('Upload files' = 'file',
                        'Try the demo' = 'demo'),
            selected = 'demo'
        ),
        conditionalPanel(
            condition = "input.data_source == 'file'",
            fileInput('df_data',
                      'Please upload your data')
        ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
        tabPanel('Overview',
                 includeMarkdown('resource/page/overview.md')),
        tabPanel('Data Viewer',
                 DTOutput("df_com"))
    ), )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    df_com <- reactive({
        read_csv('resource/data/df_com_smp.csv')
    })
    output$df_com <-
        renderDT(df_com())
}

# Run the application
shinyApp(ui = ui, server = server)
