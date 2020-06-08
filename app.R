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
library(broom)
library(ggstatsplot)
library(cowplot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$style(HTML(
        "
        h3 {
            font-weight:bold
        }
                        "
    ))),
    
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
    h6('Update version: 20600609'),
    
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
                             fileInput('df_upload_file',
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
                h3('Difference test between groups'),
                DTOutput('compare_ls'),
                h3('Post-Hoc Tests'),
                fluidRow(
                    column(3,
                           textInput(
                               'plot_x_lab',
                               "label of X axis",
                               'Treatment'
                           )),
                    column(3,
                           textInput('plot_y_lab',
                                     "label of Y axis",
                                     'value'), ),
                    column(3,
                           textInput(
                               'title_prefix',
                               "Prefix of plot",
                               "Variable"
                           ))
                ),
                fluidRow(column(
                    3,
                    selectInput(
                        'pairwise_display',
                        'Which pairwise comparisons to display?',
                        choices = c(
                            'Significant' = 'significant',
                            'All' = 'all',
                            'Non-significant' = 'non-significant'
                        ),
                        selected = 'significant'
                    )
                ),
                column(
                    3,
                    selectInput(
                        'pairwise_annotation',
                        'Annotations to use for pairwise comparisons',
                        choices = c("p.value",
                                    "asterisk"),
                        selected = 'asterisk'
                    )
                )),
                fluidRow(column(
                    3,
                    numericInput(
                        'figure_ncol',
                        'Figure numbers per row (up to 10)',
                        min = 1,
                        max = 10,
                        value = 3
                    )
                ),
                column(
                    3,
                    numericInput(
                        'figure_width',
                        'Downloaded figure width (inch)',
                        min = 3,
                        max = 20,
                        value = 12
                    )
                ),
                column(
                    3,
                    numericInput(
                        'figure_height',
                        'Downloaded figure height (inch)',
                        min = 3,
                        max = 20,
                        value = 9
                    )
                )),
                p('Once setting parameter up, click', strong('Plot Start'), 'to plot figure or click', strong('Download Figure'),'to download it directly.'),
                p('P.S. showing figure on this page and downloading figure implement in different code, even the figure in browser lookes massy or wired, the downloaded figure still can meet the standar of publication level with appropriate parameter sets.'),
                actionButton(
                    'plot_figure',
                    label = 'Plot Start',
                    icon = icon('paint-roller')
                ),
                downloadButton('dl_gg', label = 'Download Figure'),
                plotOutput('gg_post_hoc', width = 'auto'),
            )
        ))
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
        if (input$data_source == 'demo') {
            df <- read_csv('resource/data/df_com_smp.csv')
            tr_name <- colnames(df)[1]
            rename(df, 'Treatment' = tr_name)
        } else {
            if (is.null(input$df_upload_file)) {
                return("")
            } else {
                df <- read_csv(input$df_upload_file$datapath)
                tr_name <- colnames(df)[1]
                rename(df, 'Treatment' = tr_name)
            }
        }
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
    #    render: DT - with Buttons
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
        rct_dist_detect()[,-1] %>%
            sapply(function(variable) {
                if_else(any(str_detect(variable, 'skewed')),
                        'Nonparametric tests',
                        'Parametric tests')
            }) %>%
            t() %>%
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
    
    ########################################
    # Tame data by Treatment,
    # It will be used in rct_df_data_by_tr
    # (distribution plot) and
    # rct_gg_post_hoc (post hoc tests).
    # therefore, we wrap it up as reactive
    # function
    ########################################
    rct_df_data_by_tr <- reactive({
        rct_df_data() %>%
            pivot_longer(-Treatment) %>%
            ########################################
        # To make the orders of figure follow
        # the input table.
        # We convert the variable name into
        # factor and control the order of plot
        # by the level
        ########################################
        dplyr::mutate(name =
                          factor(name,
                                 levels = colnames(rct_df_data()) %>%
                                     .[-1]))
    })
    
    rct_df_hist <- reactive({
        rct_df_data_by_tr() %>%
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
        renderDT({
            rct_dist_detect()
        },
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
    
    ########################################
    # 1. compare data across treatments,
    #    the comparison method was
    #    determined by rct_analysis_method.
    #    This reactive function is called
    #    rct_compare_ls
    #    return: tibble
    #    render: DT - with Buttons
    ########################################
    rct_compare_ls <- reactive({
        map2(rct_analysis_method(), rct_df_data()[, -1], function(x, y) {
            if (x == 'Nonparametric tests') {
                ########################################
                # nonparametric test method choice
                #
                # wilcox tests family was only
                # suitable for two treatment or groups.
                # once there are triple or more groups
                # kruskal.test is an alternative
                ########################################
                if (length(unique(rct_df_data()[, 1][[1]])) == 2) {
                    wilcox.test(y ~ rct_df_data()[, 1][[1]], na.action = na.omit) %>%
                        broom::glance() %>%
                        select('statistic', 'p.value', 'df' = 'parameter', 'method')
                } else {
                    kruskal.test(y, rct_df_data()[, 1][[1]], na.action = na.omit) %>%
                        broom::glance() %>%
                        select('statistic', 'p.value', 'df' = 'parameter', 'method')
                }
            } else {
                ########################################
                # The result of ANOVA does not contain
                # method, however, the only possible
                # mehod is "ANOVA", so we can fill it
                # manually
                ########################################
                aov(y ~ rct_df_data()[, 1][[1]], na.action = na.omit) %>%
                    broom::glance() %>%
                    select('statistic', 'p.value', 'df') %>%
                    bind_cols(method = 'ANOVA')
            }
        }) %>%
            bind_rows() %>%
            bind_cols(tibble(variable = colnames(rct_analysis_method())),
                      .)
    })
    
    output$compare_ls <-
        renderDT({
            rct_compare_ls()
        },
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
    ########################################
    # 2. Post-Hoc Tests
    #    the comparison method was
    #    determined by rct_analysis_method.
    #    This reactive function is called
    #    rct_gg_post_hoc
    #    return: ggobject
    #    render: plotly
    #
    #   Customized parameter
    #
    #       xlab = input$plot_x_lab,
    #       ylab = input$plot_y_lab,
    #       pairwise.display = input$pairwise_display,
    #       pairwise.annotation = input$pairwise_annotation,
    #       title.prefix = input$title_prefix,
    ########################################
    rct_gg_post_hoc <-
        reactive({
            {
                if (is.null(input$title_prefix)) {
                    return('Please set title prefix')
                }
                if (is.null(input$plot_x_lab)) {
                    return('Please set label of X axis')
                }
                if (is.null(input$plot_y_lab)) {
                    return('Please set label of Y axis')
                }
                
                post_hoc_data <-
                    rct_df_data_by_tr() %>%
                    nest(-name) %>%
                    mutate(data = map2(name, data, function(x, y) {
                        bind_cols(name = x, y)
                    }))
                rct_analysis_method() %>%
                    map2(post_hoc_data$data, function(x, y) {
                        withProgress(expr = {
                            setProgress(
                                message = 'Calculation in progress',
                                detail = 'This may take a while...',
                                value = 1
                            )
                            if (x == 'Nonparametric tests') {
                                analysis_method = 'nonparametric'
                            } else {
                                analysis_method = 'parametric'
                            }
                            grouped_ggbetweenstats(
                                data =
                                    y,
                                x = Treatment,
                                y = value,
                                grouping.var = name,
                                type = analysis_method,
                                nboot = 10,
                                plot.type = 'box',
                                pairwise.comparisons = TRUE,
                                results.subtitle = FALSE,
                                ggstatsplot.layer = FALSE,
                                mean.plotting	= FALSE,
                                sample.size.label = FALSE,
                                messages = TRUE,
                                ggtheme = theme_classic(),
                                ########################################
                                # Customized parameter
                                ########################################
                                xlab = input$plot_x_lab,
                                ylab = input$plot_y_lab,
                                pairwise.display = input$pairwise_display,
                                pairwise.annotation = input$pairwise_annotation,
                                title.prefix = input$title_prefix,
                            )
                        })
                    }) %>%
                    plot_grid(
                        plotlist = .,
                        ncol = input$figure_ncol,
                        align = 'h'
                    )
            }
        })
    event_rct_gg_post_hoc <- eventReactive(input$plot_figure,
                                           rct_gg_post_hoc()
                                           )
    
    
    output$gg_post_hoc <-
        renderPlot({
            event_rct_gg_post_hoc()
        })
    ########################################
    # 3. save and download figure as PDF
    #    This downloadhandler is called
    #    dl_gg
    ########################################
    output$dl_gg <-
        downloadHandler(
            filename = "post_hoc_figure.pdf",
            content = function(file) {
                ggsave(file,
                       plot = rct_gg_post_hoc(),
                       width = input$figure_width,
                       height = input$figure_height)
            }
        )
}

# Run the application
shinyApp(ui = ui, server = server)