# Sidebar with a slider input for number of bins)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # font_install(source_han_sans())
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
      df <- select(iris, Species, everything())
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
    rct_df_data()[,-1] %>% 
      map_dfc(function(col_dat) {
        p_v = shapiro.test(col_dat) %>%
          .[['p.value']]
        if_else(p_v >= as.numeric(input$sw_signif_level), 'normal', 'non-normal')
      })
  })
  
  ########################################
  # 2. To determine wehter to use
  #    parametric test or nonparametric
  #    test.
  #
  #    Once a skewed distribution is
  #    found in each
  #    variable, the parametric test is
  #    not suitable any more.
  #    This reactive function is called
  #    rct_analysis_method
  #    return: character vector
  #    render: DT
  ########################################
  
  rct_analysis_method <- reactive({
    # to determine use the parametric or nonparametric tests
    nt_or_pt =
      rct_dist_detect() %>%
      lapply(function(variable) {
        if_else(any(str_detect(variable, 'non-normal')),
                'Nonparametric tests',
                'Parametric tests')
      })
    # to determine the number of treatment
    # and whether the sample numbers of each treatment
    # is equal or not
    sum_df <-
      rct_df_data() %>%
      pivot_longer(-Treatment) %>%
      group_by(name) %>%
      count(Treatment) %>%
      nest(-name) %>%
      # set the level of name as the order of nt_or_pt
      # to make sure that all tests are matched
      mutate(name = factor(name, levels = names(nt_or_pt))) %>%
      arrange(name)
    
    condition_ls <-
      lapply(sum_df$data, function(ind_sum_df) {
        tibble(
          Treatment_num = length(ind_sum_df$Treatment),
          is_paired = length(unique(ind_sum_df$n)) == 1
        )
      })
    
    # determine the detailed test method
    map2(nt_or_pt, condition_ls, function(x, y) {
      if (x == 'Parametric tests') {
        if (y$Treatment_num == 2) {
          'Student t-test'
        } else {
          'ANOVA'
        }
      } else {
        if (y$Treatment_num == 2) {
          if (y$is_paired) {
            'Wilcoxon Signed Rank test'
          } else {
            'Wilcoxon Rank Sum test'
          }
        } else {
          'Kruskal–Wallis H test'
        }
      }
    }) %>%
      as.data.frame() %>%
      as_tibble()
  })
  ########################################
  # 3. we plot the histgram of each
  #    variable.
  #    This reactive function is called
  #    rct_df_hist
  #    return: ggobject
  #    render: plot
  #    structure:
  #       wrap: name (Variable)
  #       axes: both x and y are free
  ########################################
  
  rct_df_hist <- reactive({
    rct_df_data()[,-1] %>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = unique(name))) %>% 
      ggplot(aes(value)) +
      geom_density(fill = 'skyblue') +
      facet_wrap(vars(name),
                 scales = 'free') +
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
      # To detect whether sig test is nonparm or parm test
      # if parm, permutation test can be detected
      if (x %in% c(
        'Wilcoxon Signed Rank test',
        'Wilcoxon Rank Sum test',
        'Kruskal–Wallis H test'
      )) {
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
          if (x %in% c('Wilcoxon Signed Rank test',
                       'Wilcoxon Rank Sum test')) {
            # wilcox will automatically determine which method is suitable
            wilcox.test(y ~ rct_df_data()[[1]], na.action = na.omit) %>%
              broom::glance() %>%
              select('statistic',
                     'p.value',
                     'df' = 'parameter',
                     'method') %>%
              mutate(df = as.character(df))
          } else {
            # the fallback method is kruskal.test
            kruskal.test(y, rct_df_data()[[1]], na.action = na.omit) %>%
              broom::glance() %>%
              select('statistic',
                     'p.value',
                     'df' = 'parameter',
                     'method') %>%
              mutate(df = as.character(df))
          }
        }} else if (x == 'ANOVA') {
          ########################################
          # The result of ANOVA does not contain
          # method, however, the only possible
          # mehod is "ANOVA", so we can fill it
          # manually
          ########################################
          aov(y ~ rct_df_data()[[1]], na.action = na.omit) %>%
            broom::tidy() %>%
            filter(term != "Residuals") %>%
            select('statistic', 'p.value', 'df') %>%
            bind_cols(method = 'ANOVA') %>%
            mutate(df = as.character(df))
        } else {(
          # for param which is not met the standard of anova
          # t-test will be performed
          t.test(y ~ rct_df_data()[[1]], na.action = na.omit) %>% 
            broom::glance() %>%
            select('statistic', 'p.value', df = parameter, 'method') %>% 
            mutate(df = as.character(df))
        )}
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
        bind_cols(Treatment = 
                    rct_df_data()[, 1], 
                  var = y)
      
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
        if (is.null(input$plot_x_lab)) {
          return('Please set label of X axis')
        }
        
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
        
        post_hoc_data <-
          rct_df_data_by_tr() %>%
          nest(-name) %>%
          mutate(data = map2(name, data, function(x, y) {
            bind_cols(name = x, y)
          }))
        
        list(
          rct_analysis_method(),
          post_hoc_data$data,
          post_hoc_data$name
        ) %>% 
          pmap(function(x, y, z) {
            withProgress(expr = {
              setProgress(
                message = 'Calculation in progress',
                detail = 'This may take a while...',
                value = 1
              )
              if (x %in% 
                  c('Wilcoxon Signed Rank test',
                    'Wilcoxon Rank Sum test',
                    'Kruskal–Wallis H test')
              ) {
                analysis_method = 'nonparametric'
              } else {
                # In ggstatsplot, parametric test will performed by
                # games howell post-hoc test
                # This is similar to tukey-karmer test
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
                ylab = z,
                pairwise.display = input$pairwise_display,
                pairwise.annotation = input$pairwise_annotation,
                p.adjust.method = input$p_adjust_method,
                # ggplot theme
                ggplot.component = theme(text = element_text(family = 'wqy-microhei'))
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
        showtext_auto()
        ggsave(
          file,
          plot = rct_gg_post_hoc(),
          width = input$figure_width,
          height = input$figure_height
        )
      }
    )
}