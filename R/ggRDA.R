## Define the ggRDA function
ggRDA <-
  function(rda_obj,
           sp_size = 4,
           arrow_txt_size = 4,
           envfit_df) {
    # ggplot doesn't support rda object directly, we use ggvegan::fortify function to convert the rda to data.frame
    fmod <- fortify(rda_obj)
    # to get the arrow of biplot, we plot rda by vegan::plot.rda function firstly.
    # The arrow attributes contain in the attributes(plot_obejct$biplot)$arrow.mul
    basplot <- plot(rda_obj)
    mult <- attributes(basplot$biplot)$arrow.mul
    
    # To check if envfit_df exists or not
    # If envfit_df exists, join the fortified rda_obj and envfit to mark which variable is significant.
    if (missingArg(envfit_df)) {
      bplt_df <- filter(fmod, Score == "biplot") %>%
        # If there is no requirement to mark significant variable
        # use the sytle of sinificant (black bolder solid arrow)
        # to paint the arrow
        mutate(bold = 'sig')
    } else {
      bplt_df <- filter(fmod, Score == "biplot") %>%
        left_join(envfit_df, by = c('Label' = 'factor')) %>%
        # To mark the significant variables as sig, not significant variables as ns
        # these information are stored in bold column
        mutate(bold = ifelse(str_detect(sig, fixed('*')), 'sig', 'ns'))
    }
    ggplot(fmod, aes(x = RDA1, y = RDA2)) +
      coord_fixed() +
      geom_segment(
        data = bplt_df,
        # mult and RDA1/RDA2 are from fortified RDA data.frame
        # they contain the direction and effects of every variabl
        # their products are the direction and length of arrows
        aes(
          x = 0,
          xend = mult * RDA1,
          y = 0,
          yend = mult * RDA2,
          # Use different arrow size to indicate the significant level
          size = bold,
          # Use different arrow color to indicate the significant level
          color = bold,
          # Use different arrow linetype to indicate the significant level
          linetype = bold
        ),
        #############################
        # Q:Why use three different attibution to control the significant levels?
        # It is redundancy, isn't it?
        # A: In fact, it's not easy to recongize the significant level by one kind attribution
        # Becasue it is not delicate to indicate it with supper bold and supper thin arrow,
        # by the same logic, high contrast colors are not delicate neither.
        # As for the line type, some arrow are really short, it's not easy to recognize
        # weather it is solid or dashed line at all.
        # To sum up, we use three different attributions
        # to indicate the same difference to avoid any misleading.
        #############################
        # to control the size of the header of arrow
        arrow = arrow(length = unit(0.25, "cm")),
      ) +
      # Add the text of variable name at the end of arrow
      geom_text(
        data =  subset(fmod, Score == "biplot"),
        aes(
          x = (mult + mult / 10) * RDA1,
          #we add 10% to the text to push it slightly out from arrows
          y = (mult + mult / 10) * RDA2,
          label = Label
        ),
        size = arrow_txt_size,
        #otherwise you could use hjust and vjust. I prefer this option
        hjust = 0.5
      ) +
      # Add the text of species
      geom_text(
        data = subset(fmod, Score == "species"),
        aes(colour = "species", label = Label),
        size = sp_size
      )
  }