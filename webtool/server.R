
# Define server function

shinyServer <- function(input, output, session) {
  
  #------------------------Move from landing page to additional tabs--------------  
  observe({
    if(input$button_one == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab1)
    })
  })
  
  observe({
    if(input$button_two == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab2)
    })
  })
  
  observe({
    if(input$button_three == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab3)
    })
  })
  
  #------------------------DATA PREP---------------------------------------
  
  #-------------------
  # ROLLS DATA
  #-------------------
  
  rolls_data <- rolls_prep(rolls_raw)
  
  #-------------------
  # DAMAGE AND HEALING
  #-------------------
  
  damage_clean <- cleaner(damage) %>%
    rename(damage = value)
  
  healing_clean <- cleaner(healing) %>%
    rename(healing = value)
  
  dam_heals <- damage_clean %>%
    left_join(healing_clean, by = c("episode" = "episode", "character" = "character"))
  
  # Aggregate to episode sums
  
  raw_data <- dam_heals %>%
    gather(key = variable, value = value, c(damage, healing)) %>%
    mutate(variable = str_to_sentence(variable)) %>%
    group_by(episode, variable) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(variable) %>%
    mutate(n = n(), sd = sd(value)) %>%
    ungroup()
  
  #------------------------ANALYSIS TAB------------------------------------
  
  # Barplot
  
  output$bar_plot <- renderPlot({
    
    bar_plot <- bar_rolls_data %>%
      filter(total_value == input$bar_input) %>%
      mutate(character = as.factor(character)) %>%
      ggplot(aes(x = reorder(character, counter), y = counter)) +
      geom_bar(fill = "#FD62AD", stat = "identity") +
      xlab("Character") +
      ylab("Count") +
      theme_bw() +
      coord_flip() +
      theme(panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(bar_plot)
    
  },
  bg = "transparent")
  
  # Heatmap
  
  output$tile_plot <- renderPlot({
    
    heat_data <- heatmap_prep(rolls_data)
    
    p <- heat_data %>%
      ggplot(aes(x = total_value, y = character, fill = props)) +
      geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
      geom_text(aes(x = total_value, y = character,
                    label = paste0(props,"%")), colour = "white") +
      labs(x = "Total Roll Value",
           y = NULL,
           fill = "% Total Rolls") +
      scale_fill_gradient(low = "#05445E", high = "#FD62AD",
                          label = function(x) paste0(x,"%")) +
      theme_bw() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(p)
    
  },
  bg = "transparent")
  
  # Density plot
  
  output$ridge_dens <- renderPlotly({
    
    # Values for inputs
    
    max_eps <- max(rolls_data$episode)
    
    the_dens_data <- rolls_data %>%
      filter(character == input$dist_char_selector) %>%
      filter(total_value < 100)
    
    molly_exception <- rolls_data %>%
      filter(episode >= 30) %>%
      filter(character == "Molly")
    
    if(input$dist_ep_selector == "All"){
    
    the_dens <- density_plotter(the_dens_data)
    
    } else if(input$dist_ep_selector == "Levels under 6"){
      
      the_dens_init <- the_dens_data %>%
        filter(episode < 30)
      
      the_dens <- density_plotter(the_dens_init)
      
    } else if(input$dist_ep_selector == "Levels over 6" & input$dist_char_selector == "Molly"){
      
      validate(
        need(try(length(molly_exception) > 0, "No data available.")
        )
      )
      
      the_dens_init <- the_dens_data %>%
        filter(episode >= 30)
      
      the_dens <- density_plotter(the_dens_init)
      
    } else{
      
      the_dens_init <- the_dens_data %>%
        filter(episode >= 30)
      
      the_dens <- density_plotter(the_dens_init)
      
    }
    
  })
  
  # GAM plot
  
  output$lm_plot <- renderPlot({
    
    healing_max <- max(dam_heals$healing) # For locking cartesian coordinates in the plot
    
    if(input$gam_zero_selector == "Yes"){
    
    dam_heal_plot <- dam_heals %>%
      mutate(character = case_when(
        character == "Nott_veth"  ~ "Veth/Nott",
        character == "Mollymauk"  ~ "Molly",
        character == "Beauregard" ~ "Beau",
        TRUE                      ~ character)) %>%
      drop_na() %>%
      filter(character == input$dist_char_selector) %>%
      group_by(episode, character) %>%
      summarise(damage = sum(damage),
                healing = sum(healing)) %>%
      ungroup() %>%
      ggplot(aes(x = damage, y = healing)) +
      geom_smooth(formula = y ~ s(x), method = "gam", fill = "#FEB06A", colour = "#FEB06A") +
      geom_point(size = 4, colour = "#05445E") +
      labs(x = "Damage Dealt",
           y = "Healing Given") +
      coord_cartesian(ylim = c(0, healing_max))  + # Constrains geom_smooth to 0-max range
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(dam_heal_plot)
    
    } else{
      
      dam_heal_plot <- dam_heals %>%
        mutate(character = case_when(
          character == "Nott_veth"  ~ "Veth/Nott",
          character == "Mollymauk"  ~ "Molly",
          character == "Beauregard" ~ "Beau",
          TRUE                      ~ character)) %>%
        drop_na() %>%
        filter(character == input$dist_char_selector) %>%
        filter(damage != 0 & healing != 0) %>%
        group_by(episode, character) %>%
        summarise(damage = sum(damage),
                  healing = sum(healing)) %>%
        ungroup() %>%
        ggplot(aes(x = damage, y = healing)) +
        geom_smooth(formula = y ~ x, method = "lm", fill = "#FEB06A", colour = "#FEB06A") +
        geom_point(size = 4, colour = "#05445E") +
        labs(x = "Damage Dealt",
             y = "Healing Given") +
        coord_cartesian(ylim = c(0, healing_max))  + # Constrains geom_smooth to 0-max range
        theme_bw() +
        theme(panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = alpha("white", 0.2)),
              plot.background = element_rect(fill = alpha("white", 0.2)),
              legend.background = element_rect(fill = alpha("white", 0.2)),
              axis.title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"))
      print(dam_heal_plot)
      
    }
    
  },
  bg = "transparent")
  
  # Cluster analysis
  
  output$cluster_plot <- renderPlotly({
    
    # Prep
    
    if(input$cluster_zero_selector == "No"){
      clus_data <- dam_heals %>%
        mutate(character = case_when(
          character == "Nott_veth"  ~ "Veth/Nott",
          character == "Mollymauk"  ~ "Molly",
          character == "Beauregard" ~ "Beau",
          TRUE                      ~ character)) %>%
        filter(damage != 0, healing != 0) %>%
        drop_na() %>%
        mutate(damage = scale(damage),
               healing = scale(healing))
    } else{
      clus_data <- dam_heals %>%
        mutate(character = case_when(
          character == "Nott_veth"  ~ "Veth/Nott",
          character == "Mollymauk"  ~ "Molly",
          character == "Beauregard" ~ "Beau",
          TRUE                      ~ character)) %>%
        drop_na() %>%
        mutate(damage = scale(damage),
               healing = scale(healing))
    }
    
    fit <- kmeans(clus_data[,3:4], input$num_clus)
    str(fit)
    clus_data$grouping <- as.factor(fit$cluster)
    
    # Set up palette
    
    if(input$num_clus == 1){
      cluster_palette <- c("#FD62AD")
    } else if(input$num_clus == 2){
      cluster_palette <- c("#FD62AD", "#A0E7E5")
    } else if(input$num_clus == 3){
      cluster_palette <- c("#FD62AD", "#A0E7E5", "#189AB4")
    } else if(input$num_clus == 4){
      cluster_palette <- c("#FD62AD", "#A0E7E5", "#189AB4", "#9571AB")
    } else if(input$num_clus == 5){
      cluster_palette <- c("#FD62AD", "#A0E7E5", "#189AB4", "#9571AB", "#F7C9B6")
    } else{
      cluster_palette <- c("#FD62AD", "#A0E7E5", "#189AB4", "#9571AB", "#F7C9B6", "#E7625F")
    }
    
    # Plot
    
    cluster_plot <- clus_data %>%
      ggplot(aes(x = damage, y = healing, colour = grouping,
                 text = paste('<b>Character:</b>', character,
                              '<br><b>Cluster:</b>', grouping,
                              '<b>Episode:</b>', episode))) +
      geom_point(size = 2) +
      labs(x = "Damage Dealt (Scaled)",
           y = "Healing Given (Scaled)") +
      theme_bw() +
      scale_colour_manual(values = cluster_palette) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    
    ggplotly(cluster_plot, tooltip = c("text")) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  #------------------------STATE SPACE TAB---------------------------------
  
  output$ss_model <- renderPlot({
    
    p <- raw_data %>%
      ggplot(aes(x = episode)) +
      geom_ribbon(data = full_models, 
                  aes(x = episode, ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
      geom_line(data = full_models, 
                aes(x = episode, y = mean, colour = variable), size = 1.1) +
      geom_point(data = raw_data, aes(x = episode, y = value), size = 2, colour = "black") +
      labs(x = "Episode",
           y = "Episode Sum Value") +
      theme_bw() +
      scale_colour_manual(values = ss_palette) +
      scale_fill_manual(values = ss_palette) +
      guides(fill = FALSE) +
      theme(legend.position = "none",
            panel.grid.minor =  element_blank(),
            strip.background = element_rect(fill = "#E7F2F8"),
            strip.text = element_text(face = "bold", colour = "black"),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold")) +
      facet_grid(variable ~.)
    print(p)
  },
  bg = "transparent")
  
  #------------------------MONEY TAB---------------------------------------
  
  output$waterfall <- renderPlot({
    
    water_palette <- c("Positive" = "#A0E7E5",
                       "Negative" = "#FD62AD",
                       "Zero" = "#189AB4")
    
    water <- water_data %>%
      filter(episode == input$money_episodes) %>%
      ggplot(aes(x = coin, y = value, fill = direction)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(x = "Coin Type",
           y = "Value (Scaled to value in gold)") +
      scale_fill_manual(values = water_palette) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(water)
    
    #ggplotly(water, tooltip = c("text")) %>%
    #  layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
    #         paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
    #  config(displayModeBar = F)
    
  },
  bg = "transparent")
  
  #------------------------SPELLCASTING TAB--------------------------------
  
  
  
}
