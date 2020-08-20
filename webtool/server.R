
# Define server function

shinyServer <- function(input, output, session) {
  
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
  
  #------------------------MONEY TAB---------------------------------------
  
  output$waterfall <- renderPlot({
    
    water_data_1 <- water_data %>%
      filter(episode == input$money_episodes)
    
    water <- water_data_1 %>%
      ggplot(aes(x = coin, y = value, fill = type)) +
      geom_bar(data = subset(water_data_1, type == "Gained"), stat = "identity") + 
      geom_bar(data = subset(water_data_1, type == "Paid"), stat = "identity") +
      theme_bw() +
      labs(x = "Coin Type",
           y = "Value (Scaled to value in gold)",
           fill = NULL) +
      coord_flip() +
      scale_fill_manual(values = c("#A0E7E5", "#FD62AD")) +
      theme(legend.position = "bottom",
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
  
  # Barplot
  
  output$spell_bar <- renderHighchart({
    
    spell_filt_agg <- spell_data %>%
      group_by(spell_level) %>%
      summarise(casts = sum(casts)) %>%
      ungroup()
    
    hc <- spell_filt_agg %>%
      hchart(
        "treemap", 
        hcaes(x = spell_level, value = casts, color = casts)
      ) %>%
      hc_colorAxis(stops = color_stops(n = 6, colors = c("#05445E", "#189AB4", "#A0E7E5", 
                                                         "#75E6DA", "#F7C9B6", "#FD62AD")))
    
    return(hc)
    
  })
  
  # Treemap
  
  output$spell_tree <- renderHighchart({
    
    spell_filt <- spell_data %>%
      filter(spell_level == input$spell_selector)
    
    hc1 <- spell_filt %>%
      hchart(
        "treemap", 
        hcaes(x = spell, value = casts, color = casts)
      ) %>%
      hc_colorAxis(stops = color_stops(n = 6, colors = c("#05445E", "#189AB4", "#A0E7E5", 
                                                         "#75E6DA", "#F7C9B6", "#FD62AD")))
    
    return(hc1)
    
  })
  
  # Network diagram
  
  output$sankey_plot <- renderSimpleNetwork({
    
    filtered_ch_spells <- all_ch_spells %>%
      filter(character == input$network_character)
    
    p <- simpleNetwork(filtered_ch_spells,        
                       Source = 1,                 # column number
                       Target = 3,                 # column number
                       linkDistance = 75,          # distance between nodes
                       charge = -15,                # big negative = further away
                       fontSize = 12,              
                       fontFamily = "serif",       
                       linkColour = "#189AB4",     # colour of edges
                       nodeColour = "#FD62AD",     # colour of nodes
                       opacity = 0.9,              # opacity of nodes
                       zoom = T)
    p
    
  })
  
  output$ch_spell_sankey <- renderSankeyNetwork({
    
    links <- all_ch_spells %>%
      filter(character == input$network_character) %>%
      mutate(indicator = case_when(
             character == "Caleb" & spell == "Smiles" ~ "Delete",
             TRUE                                     ~ "Keep")) %>%
      filter(indicator == "Keep") %>%
      group_by(spell, level) %>%
      summarise(number_times_cast = sum(number_times_cast)) %>%
      ungroup() %>%
      rename(source = level,
             target = spell,
             value = number_times_cast)
    
    nodes <- data.frame(
      name = c(as.character(links$source), 
               as.character(links$target)) %>% 
        unique())
    
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    colour_scale_1 <- data.frame(
      domain = c("Cantrip", "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Unknown"),
      range = c("#A0E7E5", "#189AB4", "#75E6DA", "#9571AB", "#05445E", "#FD62AD", "#F7C9B6", "#E7625F"),
      stringsAsFactors = FALSE)
    
    colour_scale <- links %>%
      inner_join(colour_scale_1, by = c("source" = "domain")) %>%
      rename(domain = source) %>%
      dplyr::select(c(domain, range))
    
    p <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget", units = "casts",
                       Value = "value", NodeID = "name", LinkGroup = "source",
                       fontSize = 14, fontFamily = "serif")#, 
                       #colourScale = JS(
                      #   sprintf(
                      #     'd3.scaleOrdinal() .domain(%s) .range(%s)',
                      #     jsonlite::toJSON(colour_scale$domain),
                      #     jsonlite::toJSON(colour_scale$range))))
    p
    
  })
  
  #------------------------POTIONS TAB-----------------------------------
  
  output$potion_sankey <- renderSankeyNetwork({
    
    links <- potion_prep(potions) %>%
      rename(source = administered_by,
             target = administered_to,
             value = healing) %>%
      group_by(source, target) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    nodes <- data.frame(
      name = c(as.character(links$source), 
             as.character(links$target)) %>% 
        unique())
    
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    sankey_palette <- 'd3.scaleOrdinal() .domain(["Beau", "Caduceus", "Fjord", "Nott", "Yasha", "Caleb", "Shakäste", "Jester", "Pumat Sol", "Molly"]) .range(["#A0E7E5", "#189AB4", "#9571AB", "#75E6DA", "#05445E", "#FD62AD", "#F7C9B6", "#E7625F", "#FEB06A", "#A91B60"])'
    
    p <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", 
                       fontSize = 20, LinkGroup = "source",
                       colourScale = sankey_palette)
    p
  
  })
  
}
