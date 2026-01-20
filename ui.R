# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Package to fix error from shinyapps.io
library(markdown)
library(PMCMRplus)
#
# Package for Shiny
library(shiny)
library(shinythemes)
library(DT)
library(shinydisconnect)
library(shiny.i18n)
library(shinyvalidate)

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

### Multiple Compare
# library(rcompanion)
library(multcompView)

### Plot Multiple Compare
library(ggstatsplot)

# Fix font of CJK
library(showtext)

# File with translations
i18n <- Translator$new(translation_json_path = "resource/i18n/translation.json")
i18n$set_translation_language("English") # here you select the default translation to display
# Define UI for application that draws a histogram
ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  disconnectMessage2(),
  tags$head(tags$style(HTML(
    "
        h3 {
            font-weight:bold
        }
                        "
  ))),
  # Application title
  theme = shinytheme("flatly"),
  navbarPage(
    "moreThanANOVA",
    tabPanel(
      i18n$t("Overview"),
      fluidRow(
        column(
          5,
          h4(
            i18n$t("Author:"),
            a(href = "https://blog.washman.top", i18n$t("Han Chen")),
            i18n$t(", Wanyanhan Jiang"),
            i18n$t(", and Wooliber")
          )
        ),
        column(
          3,
          h5(
            a(href = "mailto://chenhan28@gmail.com", "chenhan28@gmail.com")
          )
        ),
        column(
          3,
          h6(
            "Version: 20241106"
          )
        )
      ),
      includeMarkdown("resource/page/overview.md")
    ),
    tabPanel(
      title = i18n$t("Analysis"),
      sidebarLayout(
        sidebarPanel(
          img(src = "table_str.png", width = "100%"),
          selectInput(
            "selected_language",
            i18n$t("Switch language/切换语言"),
            choices = i18n$get_languages(),
            selected = i18n$get_key_translation()
          ),
          h4(i18n$t("Data Source")),
          helpText(i18n$t(
            "Please pay attention to the independence of the observations, which require no more than one pair of observations on every individual."
          )),
          radioButtons(
            "data_source",
            i18n$t("Upload files or try the demo"),
            choices = c(
              "Upload files" = "file",
              "Iris Data (Demo1)" = "demo-iris",
              "ToothGrowth Data (Demo2)" = "demo-tooth"
            ),
            selected = "demo-iris"
          ),
          helpText(
            a(
              href = "https://archive.ics.uci.edu/ml/datasets/iris",
              i18n$t("Iris Data Set")
            ),
            tags$br(),
            a(
              href = "https://academic.oup.com/jn/article-abstract/33/5/491/4726758?redirectedFrom=fulltext",
              i18n$t("ToothGrowth Data Set")
            )
          ),
          conditionalPanel(
            condition = "input.data_source == 'file'",
            fileInput(
              inputId = "df_upload_file",
              label = NULL,
              accept = ".csv"
            )
          ),
          h4(i18n$t("Significant level of Shapiro-Wilk test")),
          textInput(
            "sw_signif_level",
            i18n$t("Set threshold of Shapiro-Wilk test"),
            value = "0.05"
          ),
          h4(i18n$t("One and two sample tests")),
          selectInput(
            inputId = "try_paired",
            label = i18n$t(
              "Whether you want a paired t-test/Wilcoxon Signed-Rank test?"
            ),
            choices = c(
              "2-Sample/Unpaired",
              "1-Sample/Paired" = "paired"
            ),
            selected = "2-Sample/Unpaired"
          ),
          helpText(i18n$t(
            "Applicable for cases which each group has same number of observation."
          )),
          h4(i18n$t("Significance test between groups")),
          selectInput(
            inputId = "is_perm",
            label = i18n$t("Whether you want a permutation test?"),
            choices = c("No", "Monte Carlo Permutation Tests" = "perm"),
            selected = "No"
          ),
          helpText(i18n$t(
            "The Monte Carlo Permutation Tests may be involved in the unknown distribution or small sample size."
          )),
          h4(i18n$t("Post-Hoc Test")),
          selectInput(
            inputId = "p_adjust_method",
            label = i18n$t(
              "Adjustment method for p-value for multiple comparisons."
            ),
            choices = p.adjust.methods,
            selected = ifelse(
              length(grep("bonferroni", p.adjust.methods)) == 0,
              p.adjust.methods[[1]],
              "bonferroni"
            )
          ),
          helpText(
            i18n$t(
              "The details of adjustment method fo p-value for multiple comparisons"
            ),
            a(
              href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust",
              i18n$t("can be found here.")
            ),
            i18n$t("You can also found them from the vignette of"),
            code("stat::p.adjust")
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
          tabPanel(
            title = i18n$t("Data Viewer"),
            DTOutput("df_com")
          ),
          tabPanel(
            i18n$t("Exploratory Data Analysis"),
            h3(i18n$t("Distribution and Method detection")),
            DTOutput("df_dist_n_method"),
            helpText(i18n$t(
              "It will display as 0.0000 when a p value less than 0.0001."
            )),
            helpText(i18n$t(
              "When p-value >= 0.05, there is no statistical significance between these groups, accepting null hypothesis."
            )),
            helpText(i18n$t(
              "When p-value < 0.05, there is the statistical significance between groups, accepting alternative hypothesis. If you want to find the significant differences between groups, please continue with Post-Hoc Test in the side bar."
            )),
            helpText(i18n$t("")),
            h3(i18n$t("Density Plot")),
            plotOutput("ggplot_hist"),
            h3("Q-Q Plot"),
            helpText(i18n$t(
              "Quantile-Quantile plot, aka. Q-Q plot, is a somewhat subjective visual check. However, it is still a useful tool. In some cases, if the sample size is sufficiently large, Shapiro-Wilk Normality test may detect, even trivial departures from the null hypothesis, (i.e., although there may be some statistically significant effect, it may be too small to be of any practical significance); additional investigation by Q-Q plot is typically advisable."
            )),
            plotOutput("ggplot_qq")
          ),
          tabPanel(
            i18n$t("Comparisons"),
            h3(i18n$t("Select statistic methods")),
            fluidRow(column(width = 12, uiOutput("method_determine_select"))),
            helpText(i18n$t(
              "Select statistic methods automatically is not always suitable for every case. Histgram and Q-Q plot were also helpful for method selection."
            )),
            h3(i18n$t("Significance test between groups")),
            downloadButton(
              "dl_compare_ls",
              i18n$t("Download")
            ),
            DTOutput("compare_ls"),
            helpText(i18n$t(
              "When p-value >= 0.05, there is no statistical significance between these groups, accepting null hypothesis."
            )),
            helpText(i18n$t(
              "When p-value < 0.05, there is the statistical significance between groups, accepting alternative hypothesis. If you want to find the significant differences between groups, please continue with Post-Hoc Test in the side bar."
            )),
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
            h3(i18n$t("Data Summary")),
            downloadButton(
              "dl_compare_table",
              i18n$t("Download")
            ),
            DTOutput("compare_table"),
            helpText(i18n$t(
              "For groups/treatments with same significant level (sig_level above), it means there is no significance has been observed between these groups."
            )),
            h3(i18n$t("Post-Hoc Test")),
            fluidRow(
              column(
                3,
                textInput(
                  inputId = "plot_x_lab",
                  label = i18n$t("Label of X axis"),
                  placeholder = "Treatment"
                )
              ),
              column(
                3,
                selectInput(
                  inputId = "cow_lab",
                  label = i18n$t("Label of each plot"),
                  choices = c(
                    "UPPER CASE" = "AUTO",
                    "lower case" = "auto"
                  ),
                  selected = "auto"
                ),
              )
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  inputId = "show_statis",
                  label = i18n$t("Show the parameters of statistic?"),
                  choices = c(
                    "Show" = "show",
                    "Hide" = "hide"
                  ),
                  selected = "Show"
                )
              ),
              column(
                3,
                selectInput(
                  inputId = "pairwise_display",
                  label = i18n$t("Which pairwise comparisons to display?"),
                  choices = c(
                    "Significant" = "significant",
                    "All" = "all",
                    "Non-significant" = "non-significant"
                  ),
                  selected = "significant"
                )
              ),
              # Outdate and unavaible, Remove
              # column(
              #   3,
              #   selectInput(
              #     'pairwise_annotation',
              #     'Annotations to use for pairwise comparisons',
              #     choices = c("p.value",
              #                 "asterisk"),
              #     selected = 'asterisk'
              #   )
              # )
            ),
            fluidRow(
              column(
                3,
                numericInput(
                  inputId = "figure_ncol",
                  label = i18n$t("Figure numbers per row (up to 10)"),
                  min = 1,
                  max = 10,
                  value = 3
                )
              ),
              column(
                3,
                numericInput(
                  inputId = "figure_width",
                  label = i18n$t("Downloaded figure width (inch)"),
                  min = 3,
                  max = 20,
                  value = 12
                )
              ),
              column(
                3,
                numericInput(
                  inputId = "figure_height",
                  label = i18n$t("Downloaded figure height (inch)"),
                  min = 3,
                  max = 20,
                  value = 10
                )
              )
            ),
            p(
              "Once setting parameter up, click",
              strong("Plot Start"),
              "to plot figure or click",
              strong("Download Figure"),
              "to download it directly."
            ),
            helpText(
              i18n$t(
                "P.S. showing figure on this page and downloading figure implement in different code, even the figure in browser lookes massy or wired, the downloaded figure still can meet the standar of publication level with appropriate parameter sets."
              )
            ),
            actionButton(
              "plot_figure",
              label = "Plot Start",
              icon = icon("paint-roller")
            ),
            downloadButton("dl_gg", label = "Download Figure"),
            plotOutput("gg_post_hoc", width = "auto")
          )
        ))
      )
    ),
    tabPanel(
      title = i18n$t("Acknowledgements and References"),
      includeMarkdown("resource/page/acknowledgements.md")
    )
  )
)
