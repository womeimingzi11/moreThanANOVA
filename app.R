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
library(plotly)

# Package for data manipulation
library(tidyverse)
# library(ggthemes)
# library(ggvegan)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
        h3 {
            font-weight:bold
        }
                        "))
    ),
    
    # Application title
    theme = shinytheme('yeti'),
    titlePanel("moreThanANOVA"),
    h4(
        'Creator:',
        a(href = "https://womeimingzi11.github.io", 'Han Chen')
    ),
    h5(
        a(href = "mailto://chenhan28@gmail.com", 'chenhan28@gmail.com')
    ),
    h6('Update version: 20600607'),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                'data_source',
                'Upload files or try the demo',
                choices = c('Upload files' = 'file',
                            'Try the demo' = 'demo'),
                selected = 'demo'
            ),
            conditionalPanel(condition = "input.data_source == 'file'",
                             fileInput('df_data',
                                       'Please upload your data')),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel('Overview',
                     includeMarkdown('resource/page/overview.md')),
            tabPanel('Data Viewer',
                     DTOutput("df_com")),
            tabPanel(
                'Distribution Determine',
                h3('Data Distributions'),
                DTOutput('dist_detect'),
                h3('Comparation Method'),
                DTOutput('analysis_method'),
                h3('Density plot'),
                plotlyOutput("df_hist")
            ),
            tabPanel(
                'Comparisons',
                DTOutput('compare_ls')
            )
        ), )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ########################################
    # Data Viewer
    #
    # Before any manipulation, we should
    # have a glimpse on our data.
    #
    #
    # The data should have at least two
    # different treamtments or groups at
    # the first column
    #
    ########################################
    
    ########################################
    # 1. we check the data frame, at this
    #    step, we rename the first column as
    #    Treatment.
    #    This reactive function is called
    #    rct_df_data
    #    return: tibble
    #    render: DT
    ########################################
    rct_df_data <- reactive({
        df <- read_csv('resource/data/df_com_smp.csv')
        tr_name <- colnames(df)[1]
        rename(df, 'Treatment' = tr_name)
    })
    
    ########################################
    # Render
    #    panel: main - DataViewer
    #    rct_df_data - DT
    ########################################
    
    output$df_com <-
        renderDT(rct_df_data())
    
    ########################################
    # Distribution Detector
    #
    # Before compare data across treatments
    # We should determine the distribution
    # of our data.
    #
    # If original data does not meet normal
    # distribution, what about transformed
    # data?
    ########################################
    
    ########################################
    # 1. we check the data frame, at this
    #    step, we rename the first column as
    #    Treatment.
    #    This reactive function is called
    #    rct_df_data
    #    return: tibble
    #    render: DT
    ########################################
    
    rct_dist_detect <- reactive({
        nested_data =
            rct_df_data() %>%
            nest(-Treatment) %>%
            dplyr::mutate(distribution = map(data, ~ {
                sapply(.x, function(variable) {
                    p_v = shapiro.test(variable) %>%
                        .[['p.value']]
                    if_else(p_v > .05, 'normal', 'skewed')
                })
            }))
        bind_cols(Treatment = nested_data$Treatment,
                  bind_rows(pull(nested_data, distribution)))
    })
    
    ########################################
    # 2. To determine wehter to use
    #    parametric test or nonparametric
    #    test.
    #
    #    Once a skewed distribution is
    #    found in a treatment in each
    #    variable, the parametric test is
    #    not suitable any more.
    #    This reactive function is called
    #    rct_analysis_method
    #    return: character vector
    #    render: DT
    ########################################
    
    rct_analysis_method <- reactive({
        rct_dist_detect()[, -1] %>%
            sapply(function(variable) {
                if_else(any(str_detect(variable, 'skewed')),
                        'Nonparametric tests',
                        'Parametric tests')
            }) %>%
            t()%>%
            as_tibble()
    })
    
    ########################################
    # 3. we plot the histgram of each
    #    variable by each treatment.
    #    This reactive function is called
    #    rct_df_hist
    #    return: ggobject
    #    render: plotly
    #    structure:
    #       row: Treatment
    #       col: name (Variable)
    #       axes: both x and y are free
    ########################################
    
    rct_df_hist <- reactive({
        rct_df_data() %>%
            pivot_longer(-Treatment) %>%
            ########################################
            # To make the orders of figure follow
            # the input table.
            # We convert the variable name into
            # factor and control the order of plot
            # by the level
            ########################################
        dplyr::mutate(name = factor(name, levels = colnames(rct_df_data()) %>%
                                        .[-1])) %>%
            ggplot(aes(value)) +
            # geom_histogram() +
            geom_density(fill = 'skyblue') +
            facet_grid(
                rows = vars(Treatment),
                cols = vars(name),
                scales = 'free'
            ) +
            theme_classic()
    })
    
    ########################################
    # Render
    #    panel: main - Distribution Determine
    #    rct_dist_detect - DT
    #    rct_analysis_method - DT
    #    rct_df_hist - plotly
    ########################################
    
    output$dist_detect <-
        renderDT({rct_dist_detect()},
                 extensions = 'Buttons',
                 options = list(
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                 ))
    
    output$analysis_method <-
        renderDT(rct_analysis_method())
    
    output$df_hist <-
        renderPlotly(rct_df_hist())
    
    ########################################
    # comparison
    #
    # Time to compare data across treatments
    ########################################
    
    rct_compare_ls <- reactive({
        map2(rct_analysis_method(), rct_df_data()[,-1], function(x,y){
            if(x == 'Nonparametric tests'){
                kruskal.test(y, rct_df_data()[,1][[1]], na.action = na.omit) %>%
                    broom::glance() %>%
                    select('statistic', 'p.value', 'df' = 'parameter','method')
            } else {
                aov(y~rct_df_data()[,1][[1]], na.action = na.omit) %>%
                    broom::glance() %>%
                    select('statistic', 'p.value', 'df') %>%
                    bind_cols(method = 'ANOVA')
            }}) %>%
            bind_rows() %>%
            bind_cols(
                 tibble(variable = colnames(rct_analysis_method())),
                .)
    })
    
    output$compare_ls <-
        renderDT({rct_compare_ls()},
                 extensions = 'Buttons',
                 options = list(
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                 ))
}

# Run the application
shinyApp(ui = ui, server = server)