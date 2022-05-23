# Sidebar with a slider input for number of bins)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  inputVal <-
    InputValidator$new()
  inputVal$add_rule("df_upload_file", sv_required(message = "Upload a file is required"))
  inputVal$enable()
  
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
    if (input$data_source == "demo-iris") {
      df <- select(iris, Species, everything())
      tr_name <- colnames(df)[1]
      rename(df, "Treatment" = all_of(tr_name)) %>%
        mutate(Treatment = as.factor(Treatment))
    } else if (input$data_source == "demo-tooth") {
      df <- select(ToothGrowth, supp, everything())
      tr_name <- colnames(df)[1]
      rename(df, "Treatment" = all_of(tr_name)) %>%
        mutate(Treatment = as.factor(Treatment))
    } else {
      if (is.null(input$df_upload_file)) {
        return("")
      } else {
        df <- read_csv(input$df_upload_file$datapath)
        tr_name <- colnames(df)[1]
        rename(df, "Treatment" = all_of(tr_name)) %>%
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
  # 0. check data column by column by
  #    Shapiro-Wilk Normality Test
  #    return p value
  #    This reactive function is called
  #    rct_df_sw_test_pv
  #    return: tibble
  #    render: DT
  ########################################

  rct_df_sw_test_pv <- reactive({
    rct_df_data()[, -1] %>%
      map_dfc(function(col_dat) {
        p_v <- shapiro.test(col_dat) %>%
          .[["p.value"]]
      })
  })

  ########################################
  # 1. detect the distribution by sw_pv
  #    This reactive function is called
  #    rct_df_data
  #    return: tibble
  #    render: DT
  ########################################

  rct_dist_detect <- reactive({
    rct_df_sw_test_pv() %>%
      map_dfc(~ if_else(.x >= as.numeric(input$sw_signif_level),
        "normal",
        "non-normal"
      ))
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
  #    This section is seperated into
  #    two reactive functions.
  #    rct_condition prepare conditions
  #    rct_analysis_method generate tibble
  #    return: tibble
  #    render: DT
  ########################################
  rct_condition_ls <-
    reactive({
      # to determine the number of treatment
      # and whether the sample numbers of each treatment
      # is equal or not
      sum_df <-
        rct_df_data()$`Treatment` %>%
        table() %>%
        as.data.frame() %>%
        set_names("Variable", "Freq")

      list(
        Treatment_num = length(sum_df$Variable),
        is_equal = length(unique(sum_df$Freq)) == 1,
        to_levene = length(sum_df$Variable) == 2
      )
    })

  rct_analysis_method <- reactive({
    # determine the detailed test method
    map(rct_dist_detect(), function(x) {
      if (input$is_perm == "perm") {
        "Permutation test"
      } else {
        if (x == "normal") {
          if (rct_condition_ls()$Treatment_num == 2) {
            # Only the number of each group is equal can apply paired test
            if (rct_condition_ls()$is_equal && input$try_paired == "paired") {
              "Paired t test"
            } else {
              "t test"
            }
          } else {
            "one-way ANOVA"
          }
        } else {
          if (rct_condition_ls()$Treatment_num == 2) {
            if (rct_condition_ls()$is_equal && input$try_paired == "paired") {
              "Wilcoxon Signed rank test"
            } else {
              "Wilcoxon Rank Sum test"
            }
          } else {
            "Kruskal Wallis H test"
          }
        }
      }
    }) %>%
      as.data.frame() %>%
      as_tibble()
  })

  ########################################
  # 2.5. apply Levene’s test to check
  #    variance homogeneity
  #    This reactive function is called
  #    rct_levene_p
  #    return: tibble
  #    render: DT
  #    colnames: Levene's Test (p.value)
  ########################################

  rct_levene_p <- reactive({
    if (rct_condition_ls()$to_levene) {
      # rct_df_data[, -1] %>%
      rct_df_data()[, -1] %>%
        map_dfc(function(value_by_col) {
          # leveneTest(value_by_col, group = rct_df_data[, 1]) %>%
          leveneTest(value_by_col, group = rct_df_data()[, 1]) %>%
            tidy() %>%
            select(p.value)
        }) %>%
        # set_names(names(rct_analysis_method))
        set_names(names(rct_analysis_method()))
    }
  })

  ########################################
  # 3. we combine the tables of
  #    distribution and methods selection
  #    This reactive function is called
  #    rct_df_dist_n_method
  #    return: tibble
  #    render: DT
  #    colnames:
  ########################################
  rct_df_dist_n_method <- reactive({
    tb_sw_test_pv_long <-
      pivot_longer(rct_df_sw_test_pv(),
        everything(),
        names_to = "Varible",
        values_to = "p.value"
      ) %>%
      mutate(
        "p.value" =
          sprintf("%.4f", `p.value`)
      ) %>%
      rename(`Shapiro-Wilk (p.value)` = `p.value`)

    print(tb_sw_test_pv_long)

    tb_dist_detect_long <-
      pivot_longer(rct_dist_detect(),
        everything(),
        names_to = "Varible",
        values_to = "Distribution"
      )

    print(tb_dist_detect_long)

    tb_analysis_method <-
      pivot_longer(rct_analysis_method(),
        everything(),
        names_to = "Varible",
        values_to = "Method"
      )

    print(tb_analysis_method)

    final_dist_n_method <-
      if (rct_condition_ls()$to_levene) {
        tb_levene_p <-
          pivot_longer(rct_levene_p(),
            everything(),
            names_to = "Varible",
            values_to = "Levene test (p.value)"
          ) %>%
          mutate(`Levene test (p.value)` = as.numeric(`Levene test (p.value)`))

        print(tb_levene_p)

        list(
          tb_sw_test_pv_long,
          tb_dist_detect_long,
          tb_levene_p,
          tb_analysis_method
        ) %>%
          reduce(left_join, by = "Varible") %>%
          mutate(Method = if_else(Method == "t test",
            if_else(`Levene test (p.value)` <= .05,
              "t test (unequal variance)",
              "t test (equal variance)"
            ),
            Method
          ))
      } else {
        list(
          tb_sw_test_pv_long,
          tb_dist_detect_long,
          tb_analysis_method
        ) %>%
          reduce(left_join, by = "Varible")
      }
    print(final_dist_n_method)

    final_dist_n_method
  })



  ########################################
  # 4. we plot the histgram of each
  #    variable.
  #    This reactive function is called
  #    rct_ggplot_hist
  #    return: ggobject
  #    render: plot
  #    structure:
  #       wrap: name (Variable)
  #       axes: both x and y are free
  ########################################

  rct_ggplot_hist <- reactive({
    rct_df_data()[, -1] %>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = unique(name))) %>%
      ggplot(aes(value)) +
      geom_density(fill = "skyblue") +
      facet_wrap(vars(name),
        scales = "free"
      ) +
      theme_classic()
  })

  ########################################
  # 6. we plot the qqplot of each
  #    variable.
  #    This reactive function is called
  #    rct_ggplot_qq
  #    return: ggobject
  #    render: plot
  #    structure:
  #       wrap: name (Variable)
  #       axes: both x and y are free
  ########################################

  rct_ggplot_qq <- reactive({
    rct_df_data()[, -1] %>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = unique(name))) %>%
      ggplot(aes(sample = value)) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(vars(name),
        scales = "free"
      ) +
      theme_classic()
  })

  ########################################
  # 7. Provide options to determine
  #    what kind of statics
  #    would you like manually
  #    This reactive function is called
  #    rct_var_select_method_determine
  #    return: uiOutput
  ########################################
  rct_select_method_determine <- reactive({
    map2(rct_df_dist_n_method()$Varible, rct_df_dist_n_method()$Method, function(var, method) {
      alternative_method <-
        if (method %in% c(
          "Wilcoxon Rank Sum test",
          "t test (unequal variance)",
          "t test (equal variance)"
        )) {
          c(
            "Wilcoxon Rank Sum test",
            "t test (unequal variance)",
            "t test (equal variance)"
          )
        } else if (method %in% c(
          "Kruskal Wallis H test",
          "one-way ANOVA"
        )) {
          c(
            "Kruskal Wallis H test",
            "one-way ANOVA"
          )
        } else if (method %in% c(
          "Wilcoxon Signed rank test",
          "Paired t test"
        )) {
          c(
            "Wilcoxon Signed rank test",
            "Paired t test"
          )
        } else {
          "Permutation test"
        }

      column(
        width = 4,
        selectInput(
          inputId = paste0("var_", var),
          label = paste("Method for", var),
          choices = alternative_method,
          selected = method
        )
      )
    })
  })

  ########################################
  # Render
  #    panel: main - Distribution Determine
  ###    rct_dist_detect - DT
  ###    rct_analysis_method - DT
  #    rct_df_dist_n_method - DT
  #    rct_ggplot_hist - plot
  #    rct_ggplot_qq - plot
  ########################################
  output$df_dist_n_method <-
    renderDT(rct_df_dist_n_method())

  output$method_determine_select <-
    renderUI(rct_select_method_determine())

  output$ggplot_hist <-
    renderPlot(rct_ggplot_hist())

  output$ggplot_qq <-
    renderPlot(rct_ggplot_qq())
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
    map2(rct_df_dist_n_method()$Varible, rct_df_data()[, -1], function(var_name, var_data) {
      # get input from dynamic uiOutput
      # I need to use a new trick to access the values the input values.
      # So far we’ve always accessed the components of input with $, e.g. input$col1.
      # But here we have the input names in a character vector,
      # like var <- "col1". $ no longer works in this scenario,
      # so we need to swich to [[, i.e. input[[var]].

      var_method <-
        input[[paste0("var_", var_name)]]

      print(var_method)

      switch(var_method,
        "Permutation test" =
          independence_test(var_data ~ factor(rct_df_data()$Treatment),
            distribution = approximate(nresample = 9999)
          ) %>%
            tibble(
              statistic = statistic(.),
              p.value = pvalue(.) %>% as.numeric(),
              df = "",
              method = "Monte Carlo Permutation test"
            ),
        "t test (unequal variance)" =
        # for param which is not met the standard of one-way ANOVA
        # t test will be performed
          t.test(var_data ~ rct_df_data()$Treatment, na.action = na.omit, paired = FALSE, var.equal = FALSE) %>%
            broom::glance() %>%
            mutate(df = "") %>%
            select("statistic", "p.value", "df", "method"),
        "t test (equal variance)" =
        # for param which is not met the standard of one-way ANOVA
        # t test will be performed
          t.test(var_data ~ rct_df_data()$Treatment, na.action = na.omit, paired = FALSE, var.equal = TRUE) %>%
            broom::glance() %>%
            mutate(df = "") %>%
            select("statistic", "p.value", "df", "method"),
        "Paired t test" =
        # for param which is not met the standard of one-way ANOVA
        # t test will be performed
          t.test(var_data ~ rct_df_data()$Treatment, na.action = na.omit, paired = TRUE) %>%
            broom::glance() %>%
            mutate(df = "") %>%
            select("statistic", "p.value", "df", "method"),
        "Wilcoxon Signed rank test" =
          wilcox.test(var_data ~ rct_df_data()$Treatment, na.action = na.omit, paired = TRUE) %>%
            broom::glance() %>%
            mutate(df = "") %>%
            select("statistic", "p.value", "df", "method"),
        "Wilcoxon Rank Sum test" =
          wilcox.test(var_data ~ rct_df_data()$Treatment, na.action = na.omit, paired = FALSE) %>%
            broom::glance() %>%
            mutate(df = "") %>%
            select("statistic", "p.value", "df", "method"),
        "Kruskal Wallis H test" =
        # the fallback method is kruskal.test
          kruskal.test(var_data, rct_df_data()$Treatment, na.action = na.omit) %>%
            broom::glance() %>%
            select("statistic",
              "p.value",
              "df" = "parameter",
              "method"
            ) %>%
            mutate(df = as.character(df)),
        "one-way ANOVA" =
          aov(var_data ~ rct_df_data()$Treatment, na.action = na.omit) %>%
            broom::tidy() %>%
            filter(term != "Residuals") %>%
            select(
              "statistic",
              "p.value",
              "df"
            ) %>%
            mutate(
              method = "one-way ANOVA",
              df = as.character(df)
            )
        # TRUE ~ stop("Find an issue in `rct_compare_ls` section, please note chenhan28@gmail.com")
      )
    }) %>%
      bind_rows() %>%
      bind_cols(
        tibble(variable = colnames(rct_analysis_method())),
        .
      )
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
      df_tr_var <-
        bind_cols(
          Treatment =
            rct_df_data()[, 1],
          var = y
        )

      df_comp_ls <-
        df_tr_var %>%
        group_by(Treatment) %>%
        summarize(
          Mean = mean(var, na.rm = TRUE),
          SE = sd(var, na.rm = TRUE),
          Median = median(var, na.rm = TRUE),
          IQR = IQR(var, na.rm = TRUE)
        )

      ls_perm_res <-
        pairwisePermutationMatrix(var ~ Treatment,
          data = df_tr_var,
          method = input$p_adjust_method
        )

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
    reactive({{
    #   if (is.null(input$plot_x_lab)) {
    #     return("Please set label of X axis")
    # }

    ########################################
    # Tame data by Treatment,
    # It will be used in rct_df_data_by_tr
    # (distribution plot) and
    # rct_gg_post_hoc (post hoc tests).
    # therefore, we wrap it up as reactive
    # function
    ########################################
    rct_df_data_by_tr <- 
      rct_df_data() %>%
        pivot_longer(-Treatment) %>%
        ########################################
        # To make the orders of figure follow
        # the input table.
        # We convert the variable name into
        # factor and control the order of plot
        # by the level
        ########################################
        dplyr::mutate(
          name =
            factor(name,
              levels = colnames(rct_df_data()) %>%
                .[-1]
            )
        )

    post_hoc_data <-
      rct_df_data_by_tr %>%
      nest(-name) %>%
      mutate(data = map2(name, data, function(x, y) {
        bind_cols(name = x, y)
      }))

    method_by_select <-
      rct_df_dist_n_method()$Varible %>%
      map(~ input[[paste0("var_", .x)]])
    
    # get input from dynamic uiOutput
    # I need to use a new trick to access the values the input values.
    # So far we’ve always accessed the components of input with $, e.g. input$col1.
    # But here we have the input names in a character vector,
    # like var <- "col1". $ no longer works in this scenario,
    # so we need to swich to [[, i.e. input[[var]].

    list(
      method_by_select,
      post_hoc_data$data,
      post_hoc_data$name
    ) %>%
      pmap(function(x, y, z) {
        withProgress(expr = {
          setProgress(message = "Calculation in progress",
                      detail = "This may take a while...",
                      value = 1)
          if (x %in%
              c("Wilcoxon Signed rank test",
                "Wilcoxon Rank Sum test",
                "Kruskal Wallis H test")) {
            analysis_method <- "nonparametric"
          } else if (x == "t test (equal variance)") {
            # t test (equal variance) should be considered
            # a special name
            analysis_method <- "parametric_variance_equal"
          } else {
            # In ggstatsplot, parametric test will performed by
            # games howell post-hoc test
            # This is similar to tukey-karmer test
            analysis_method <- "parametric"
          }
          
          print(y)
          
          showtext_auto()
          ggbetweenstats(
            data = y,
            x = Treatment,
            y = value,
            # grouping.var = name,
            type = if_else(
              analysis_method == "parametric_variance_equal",
              "parametric",
              analysis_method
            ),
            nboot = 10,
            plot.type = "box",
            pairwise.comparisons = TRUE,
            results.subtitle = identical(input$show_statis, "show"),
            var.equal = identical(analysis_method, "parametric_variance_equal"),
            ggstatsplot.layer = FALSE,
            mean.plotting = FALSE,
            sample.size.label = FALSE,
            messages = TRUE,
            ggtheme = theme_classic(),
            ########################################
            # Customized parameter
            ########################################
            xlab = input$plot_x_lab,
            ylab = z,
            pairwise.display = input$pairwise_display,
            # Outdate and unavaible, Remove
            p.adjust.method = input$p_adjust_method,
            # ggplot theme
            ggplot.component = theme(text = element_text(family = "wqy-microhei"))
          )
        })
      }) %>%
      plot_grid(
        plotlist = .,
        ncol = input$figure_ncol,
        align = "h",
        labels = input$cow_lab
      ) }})
  
  event_rct_gg_post_hoc <- eventReactive(
    input$plot_figure,
    rct_gg_post_hoc()
  )

  output$compare_ls <-
    renderDT({
      rct_compare_ls()
    })
  output$dl_compare_ls <-
    downloadHandler(
      filename = "compare_list.csv",
      content = function(file) {
        write_csv(
          x = rct_compare_ls(),
          path = file
        )
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
        write_csv(
          x = rct_compare_table(),
          file = file
        )
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
