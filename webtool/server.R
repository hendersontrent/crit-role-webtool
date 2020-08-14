
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
      theme(legend.position = "bottom",
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(p)
    
  },
  bg = "transparent")
  
  # Ridgeplot
  
  output$ridge_dens <- renderPlot({
    
    the_dens <- rolls_data %>%
      filter(character %in% the_nein) %>%
      mutate(character = case_when(
        character == "Nott" ~ "Veth/Nott",
        character == "Veth" ~ "Veth/Nott",
        TRUE                ~ character)) %>%
      filter(total_value < 100) %>%
      ggplot(aes(x = total_value, y = character, fill = ..x..)) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      labs(x = "Total Roll Value",
           y = NULL,
           fill = "Roll value") +
      theme_bw() +
      scale_x_continuous(limits = c(0,50),
                         breaks = c(0,10,20,30,40,50)) +
      scale_fill_gradient(low = "#A0E7E5", high = "#FD62AD") +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(the_dens)
    
  },
  bg = "transparent")
  
  # Time series density
  
  output$ts_dens <- renderPlot({
    
    nat_20s <- rolls_data %>%
      filter(total_value == 120) %>%
      filter(character %in% the_nein) %>%
      mutate(character = case_when(
        character == "Nott" ~ "Veth/Nott",
        character == "Veth" ~ "Veth/Nott",
        TRUE                ~ character))
    
    ts_dens <- nat_20s %>%
      ggplot(aes(episode, after_stat(count), fill = character)) +
      geom_density(position = "fill") +
      labs(x = "Episode",
           y = "Roll Count Density",
           fill = NULL) +
      theme_bw() +
      scale_fill_manual(values = the_palette) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    print(ts_dens)
    
  },
  bg = "transparent")
  
  # Multinomial model
  
  output$multinom_plot <- renderPlot({
    
    mod_dat <- multinom_prep(rolls_data)
    
    theme_set(theme_sjplot())
    
    the_mod <- plot_model(mod_dat, sort.est = TRUE, transform = "plogis", show.values = TRUE, value.offset = .3,
                          title = "", colors = c("#FD62AD"))
    print(the_mod)
    
  },
  bg = "transparent")
  
  #------------------------STATE SPACE TAB---------------------------------
  
  full_models <- eventReactive(input$go_ss, {
    
    the_vars <- unique(raw_data$variable)
    
    some_list <- list()
    
    for(i in the_vars){
      
      shorter <- raw_data %>%
        filter(variable == i)
      
      d1 <- list(
        mu_start = first(shorter$value),
        n_eps = nrow(shorter),
        y_values = shorter$value,
        sigma = unique(shorter$sd)
      )
      
      mod <- stan(file = "R/ss-model.stan", data = d1, iter = 4000, control = list(max_treedepth = 20))
      
      ex <- as.data.frame(rstan::extract(mod, "mu"))
      
      outs <- ex %>%
        gather(key = episode, value = value, 1:105) %>%
        mutate(episode = gsub("mu.", "\\1", episode)) %>%
        mutate(episode = as.numeric(episode)) %>%
        group_by(episode) %>%
        summarise(mean = mean(value),
                  upper = quantile(value, 0.975),
                  lower = quantile(value, 0.025)) %>%
        ungroup() %>%
        mutate(variable = i)
      
      some_list[[i]] <- outs
      
    }
    
    full_models <- rbindlist(some_list, use.names = TRUE)
    
    return(full_models)
  })
  
  output$ss_model <- renderPlot({
    
    validate(
      need(full_models(), "Press 'Run Model'."
      )
    )
    
    p <- raw_data %>%
      ggplot(aes(x = episode)) +
      geom_ribbon(data = full_models(), 
                  aes(x = episode, ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
      geom_line(data = full_models(), 
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
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold")) +
      facet_grid(variable ~.)
    print(p)
  },
  bg = "transparent")
  
}
