#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Package to fix error from shinyapps.io
library(markdown)
#
# Package for Shiny
library(shiny)
library(shinythemes)
library(DT)
library(shinydisconnect)

# Package for data manipulation
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(broom)
library(cowplot)

# Package for tests

### Levene’s test
library(car)

### Permutation test
library(coin)

### Multiple Compare
library(rcompanion)
library(multcompView)

### Plot Multiple Compare
library(ggstatsplot)

# Fix font of CJK
library(showtext)

# Define UI for application that draws a histogram
ui <- fluidPage(
  disconnectMessage2(),
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
    tabPanel(
      'Overview',
      fluidRow(column(5,
                      h4(
                        'Author:',
                        a(href = "https://blog.washman.top", 'Han Chen'),
                        ', Wanyanhan Jiang'
                      )),
               column(3,
                      h5(
                        a(href = "mailto://chenhan28@gmail.com", 'chenhan28@gmail.com')
                      )),
               column(3,
                      h6(
                        'Version: 20210726'
                      ))),
      includeMarkdown('resource/page/overview.md')
    ),
    tabPanel('Analysis',
             sidebarLayout(
               sidebarPanel(
                 img(src = "table_str.png", width = "100%"),
                 h4('Data Source'),
                 radioButtons(
                   'data_source',
                   'Upload files or try the demo',
                   choices = c('Upload files' = 'file',
                               'Iris Data (Demo1)' = 'demo-iris',
                               'ToothGrowth (Demo2)' = 'demo-tooth'),
                   selected = 'demo-tooth'
                 ),
                 conditionalPanel(
                   condition = "input.data_source == 'file'",
                   fileInput('df_upload_file',
                             'Please upload your data')
                 ),
                 h4('Significant level of Shapiro-Wilk test'),
                 textInput(
                   'sw_signif_level',
                   'Set thresheld of Shapiro-Wilk test',
                   value = "0.05"
                   ),
                 h4('One and two sample tests'),
                 selectInput(
                   "try_paired",
                   "Whether you want a paired t-test/Wilcoxon Signed-Rank test?",
                   choices = c(
                     '2-Sample/Unpaired',
                     '1-Sample/Paired' = 'paired'
                   ),
                   selected = '2-Sample/Unpaired'
                 ),
                 helpText("Applicable for cases which each group has same number of observation."),
                 h4('Significance test between groups'),
                 selectInput(
                   'is_perm',
                   'Whether you want a permutation test?',
                   choices = c('No', 
                               'Monte Carlo Permutation Tests' = 'perm'),
                   selected = 'No'
                 ),
                 h4('Post-Hoc Test'),
                 selectInput(
                   'p_adjust_method',
                   'Adjustment method for p-value for multiple comparisons.',
                   choices = p.adjust.methods,
                   selected = ifelse(
                     length(grep("bonferroni",p.adjust.methods)) == 0, 
                     p.adjust.methods[[1]],
                     "bonferroni")
                   ),
                 helpText(
                   "The details of adjustment method fo p-value for multiple comparisons",
                   a(href = 'https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust',
                     'can be found here.'),
                   'You can also found them from the vignette of',
                   code('stat::p.adjust')
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(tabsetPanel(
                 tabPanel('Data Viewer',
                          DTOutput("df_com")),
                 tabPanel(
                   'Exploratory Data Analysis',
                   h3("Distribution and Method detection"),
                   DTOutput("df_dist_n_method"),
                   helpText("Once a p value < 0.0001, it will display as 0.0000."),
                   h3("Select statistic methods manually"),
                   fluidRow(column(width=12,uiOutput("method_determine_select"))),
                   h3('Density Plot'),
                   plotOutput("ggplot_hist"),
                   h3('Q-Q Plot'),
                   helpText("quantile-quantie plot, aka. Q-Q plot, ",
                            "is a somewhat subjective visual check. ",
                            "However, it is still a useful tool. ",
                            "In some cases, if the sample size is sufficiently large, ",
                            "Shapiro-Wilk Normality test may detect, ",
                            "even trivial departures from the null hypothesis, ",
                            " (i.e., although there may be some statistically significant effect, ",
                            "it may be too small to be of any practical significance); ",
                            "additional investigation by Q-Q plot is typically advisable."),
                   plotOutput("ggplot_qq")
                 ),
                 tabPanel(
                   'Comparisons',
                   h3('Significance test between groups'),
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
                   h3('Data Summary'),
                   downloadButton('dl_compare_table',
                                  'Download'),
                   DTOutput('compare_table'),
                   h3('Post-Hoc Test'),
                   fluidRow(
                     column(3,
                            textInput('plot_x_lab',
                                      "Label of X axis",
                                      'Treatment')
                     ),
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
                   fluidRow(
                     column(
                       3,
                       selectInput(
                         'show_statis',
                         'Do you need the statisiics tests?',
                         choices = c(
                           'Show' = 'show',
                           'Hide' = 'hide'
                         ),
                         selected = 'Show'
                       )
                     ),
                     column(
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
                   helpText(
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
  )
)