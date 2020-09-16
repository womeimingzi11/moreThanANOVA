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
library(coin)
library(broom)
library(rcompanion)
library(multcompView)
library(ggstatsplot)
library(cowplot)

# Fix font of CJK
library(showtext)
library(showtextdb)

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
    theme = shinytheme('flatly'),
    # moreThanANOVAtitlePanel("moreThanANOVA"),
    
    navbarPage(
        "moreThanANOVA",
        # id = "main_navbar",
        tabPanel('Overview',
                 fluidRow(
                     column(3,
                            h4(
                                'Creator:',
                                a(href = "https://womeimingzi11.github.io", 'Han Chen')
                            )),
                     column(3,
                            h5(
                                a(href = "mailto://chenhan28@gmail.com", 'chenhan28@gmail.com')
                            )
                     ),
                     column(6,
                            h6('Update version: 20200916')
                     )
                 ),
                 includeMarkdown('resource/page/overview.md')),
        tabPanel('Analysis',
                 sidebarLayout(
                     sidebarPanel(
                         img(src = "table_str.png", width = "100%"),
                         h4('Data Source'),
                         radioButtons(
                             'data_source',
                             'Upload files or try the demo',
                             choices = c('Upload files' = 'file',
                                         'Try the demo' = 'demo'),
                             selected = 'demo'
                         ),
                         conditionalPanel(
                             condition = "input.data_source == 'file'",
                             fileInput('df_upload_file',
                                       'Please upload your data')
                         ),
                         h4('Difference test between groups'),
                         selectInput(
                             'non_par_method',
                             'Test method for non-parametric tests',
                             choices = c('Rank Test', 'Monte Carlo Permutation Tests' = 'perm'),
                             selected = 'perm'
                         ),
                         h4('Post-Hoc Tests'),
                         selectInput(
                             'p_adjust_method',
                             'Adjustment method for p-value for multiple comparisons.',
                             choices = c(
                                 "holm",
                                 "hochberg",
                                 "hommel",
                                 "bonferroni",
                                 "BH",
                                 "BY",
                                 "fdr",
                                 "none"
                             ),
                             selected = 'fdr'
                         ),
                         p(
                             "The details of adjustment method fo p-value for multiple comparisons",
                             a(href = 'https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust',
                               'can be found here'),
                             'You can also found them from the vignette of',
                             code('stat::p.adjust')
                         )
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(tabsetPanel(
                         tabPanel('Data Viewer',
                                  DTOutput("df_com")),
                         tabPanel(
                             'Data Distribution',
                             h3('Data Distributions'),
                             DTOutput('dist_detect'),
                             h3('Comparation Method'),
                             DTOutput('analysis_method'),
                             h3('Density Plot'),
                             plotOutput("df_hist")
                         ),
                         tabPanel(
                             'Comparisons',
                             h3('Difference test between groups'),
                             downloadButton('dl_compare_ls',
                                            'Download'),
                             DTOutput('compare_ls'),
                             ########################################
                             # Why don't use the DT tool button?
                             # Becasue the DT solve data table works
                             # on the server rather than local by
                             # default.
                             #
                             # We can use the server = FALSE to load
                             # all the data to local, but once the
                             # data is a super large table, or
                             # your device is not high performance,
                             # it will frozen the browser or cause
                             # some crash.
                             #
                             # So, we seperate the DT and Download
                             # to make things work well.
                             ########################################
                             downloadButton('dl_compare_table',
                                            'Download'),
                             DTOutput('compare_table'),
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
                                 column(
                                     3,
                                     selectInput(
                                         'cow_lab',
                                         "label of each plot",
                                         choices = c('UPPER CASE' = "AUTO",
                                                     'lower case' = "auto"),
                                         selected = 'auto'
                                     ),
                                 )
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
                             fluidRow(
                                 column(
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
                                         value = 10
                                     )
                                 )
                             ),
                             p(
                                 'Once setting parameter up, click',
                                 strong('Plot Start'),
                                 'to plot figure or click',
                                 strong('Download Figure'),
                                 'to download it directly.'
                             ),
                             p(
                                 'P.S. showing figure on this page and downloading figure implement in different code, even the figure in browser lookes massy or wired, the downloaded figure still can meet the standar of publication level with appropriate parameter sets.'
                             ),
                             actionButton(
                                 'plot_figure',
                                 label = 'Plot Start',
                                 icon = icon('paint-roller')
                             ),
                             downloadButton('dl_gg', label = 'Download Figure'),
                             plotOutput('gg_post_hoc', width = 'auto')
                         )
                     ))

                 )),
        tabPanel(
            'Acknowledgements & References',
            includeMarkdown('resource/page/acknowledgements.md')
        )
    ))
    
    # Sidebar with a slider input for number of bins)
    # Define server logic required to draw a histogram
    server <- function(input, output) {
        font_install(source_han_sans())
        source('R/glance_coin.R')
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
                rename(df, 'Treatment' = tr_name) %>%
                    mutate(Treatment = as.factor(Treatment))
            } else {
                if (is.null(input$df_upload_file)) {
                    return("")
                } else {
                    df <- read_csv(input$df_upload_file$datapath)
                    tr_name <- colnames(df)[1]
                    rename(df, 'Treatment' = tr_name) %>%
                        mutate(Treatment = as.factor(Treatment))
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
            rct_dist_detect()[, -1] %>%
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
        #    render: plot
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
        #    rct_df_hist - plot
        ########################################
        
        output$dist_detect <-
            renderDT({
                rct_dist_detect()
            })
        
        output$analysis_method <-
            renderDT(rct_analysis_method())
        
        output$df_hist <-
            renderPlot(rct_df_hist())
        
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
            map2(rct_analysis_method(), rct_df_data()[,-1], function(x, y) {
                if (x == 'Nonparametric tests') {
                    if (input$non_par_method == 'perm') {
                        independence_test(y ~ factor(rct_df_data()[[1]]),
                                          distribution = approximate(nresample = 9999)) %>%
                            glance_coin()
                    } else {
                        ########################################
                        # nonparametric test method choice
                        #
                        # wilcox tests family was only
                        # suitable for two treatment or groups.
                        # once there are triple or more groups
                        # kruskal.test is an alternative
                        ########################################
                        if (length(unique(rct_df_data()[[1]])) == 2) {
                            wilcox.test(y ~ rct_df_data()[[1]], na.action = na.omit) %>%
                                broom::glance() %>%
                                select('statistic',
                                       'p.value',
                                       'df' = 'parameter',
                                       'method')%>%
                                mutate(df = as.character(df))
                        } else {
                            kruskal.test(y, rct_df_data()[[1]], na.action = na.omit) %>%
                                broom::glance() %>%
                                select('statistic',
                                       'p.value',
                                       'df' = 'parameter',
                                       'method')%>%
                                mutate(df = as.character(df))
                        }
                    }
                    
                } else {
                    ########################################
                    # The result of ANOVA does not contain
                    # method, however, the only possible
                    # mehod is "ANOVA", so we can fill it
                    # manually
                    ########################################
                    aov(y ~ rct_df_data()[[1]], na.action = na.omit) %>%
                        broom::glance() %>%
                        select('statistic', 'p.value', 'df') %>%
                        bind_cols(method = 'ANOVA')%>%
                        mutate(df = as.character(df))
                }
            }) %>%
                bind_rows() %>%
                bind_cols(tibble(variable = colnames(rct_analysis_method())),
                          .)
        })
        
        ########################################
        # 2. compare data across treatments,
        #    the comparison method was
        #    determined by rct_analysis_method.
        #    This reactive function is called
        #    rct_compare_ls
        #    return: tibble
        #    render: DT - with Buttons
        ########################################
        
        rct_compare_table <- reactive({
            map2(colnames(rct_df_data()[, -1]), rct_df_data()[, -1], function(x, y) {
                df_tr_var  <-
                    bind_cols(rct_df_data()[, 1], var = y)
                
                df_comp_ls <-
                    df_tr_var %>%
                    group_by(Treatment) %>%
                    summarize(
                        Mean = mean(var),
                        SE = sd(var),
                        Median = median(var),
                        IQR = IQR(var)
                    )
                
                ls_perm_res <-
                    pairwisePermutationMatrix(var ~ Treatment,
                                              data = df_tr_var,
                                              method = input$p_adjust_method)
                
                df_pair_perm <-
                    multcompLetters(
                        ls_perm_res$Adjusted,
                        # compare="<",
                        threshold = 0.05,
                        Letters = letters,
                        reversed = FALSE
                    )
                
                bind_cols(list(
                    Variable = x,
                    df_comp_ls,
                    sig_level = df_pair_perm$Letters
                ))
                #
                #             %>%
                #                 bind_cols(Variable = x, .)
            }) %>%
                bind_rows()
        })
        
        ########################################
        # 2. Post-Hoc Tests
        #    the comparison method was
        #    determined by rct_analysis_method.
        #    This reactive function is called
        #    rct_gg_post_hoc
        #    return: ggobject
        #    render: plot
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
                    # if (is.null(input$title_prefix)) {
                    #     return('Please set title prefix')
                    # }
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
                                showtext_auto()
                                ggbetweenstats(
                                    data =
                                        y,
                                    x = Treatment,
                                    y = value,
                                    # grouping.var = name,
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
                                    # title.prefix = input$title_prefix,
                                    p.adjust.method = input$p_adjust_method,
                                    # ggplot theme
                                    ggplot.component = theme(
                                        text = element_text(
                                            family = 'source-han-sans-cn'
                                        )
                                    )
                                )
                            })
                        }) %>%
                        plot_grid(
                            plotlist = .,
                            ncol = input$figure_ncol,
                            align = 'h',
                            labels = input$cow_lab
                        )
                }
            })
        event_rct_gg_post_hoc <- eventReactive(input$plot_figure,
                                               rct_gg_post_hoc())
        
        output$compare_ls <-
            renderDT({
                rct_compare_ls()
            })
        output$dl_compare_ls <-
            downloadHandler(
                filename = "compare_list.csv",
                content = function(file) {
                    write_csv(x = rct_compare_ls(),
                              path = file)
                }
            )
        
        output$compare_table <-
            renderDT({
                rct_compare_table()
            })
        output$dl_compare_table <-
            downloadHandler(
                filename = "compare_table.csv",
                content = function(file) {
                    write_csv(x = rct_compare_table(),
                              path = file)
                }
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
                    ggsave(
                        file,
                        plot = rct_gg_post_hoc(),
                        width = input$figure_width,
                        height = input$figure_height
                    )
                }
            )
    }
    
    # Run the application
    shinyApp(ui = ui, server = server
    )