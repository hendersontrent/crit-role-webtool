#---------------------------------------
# This script sets out to produce a 
# series of reusable functions for the
# web app
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 14 August 2020
#----------------------------------------

#----------------- GENERAL --------------------------------

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#----------------- ROLLS DATA -----------------------------

# General

rolls_prep <- function(data){
  
  # Remove unnecessary first worksheet
  
  d1 <- data
  
  d1 = d1[-1]
  
  # Retain only columns I need as worksheets are uneven
  
  keep <- c("Episode", "Character", "Type of Roll", "Total Value", "Natural Value")
  
  d2 <- lapply(d1, function(x) subset(x, select = intersect(keep, colnames(x))))
  
  varnames <- names(d2[[1]]) # variable names
  vattr <- purrr::map_chr(varnames, ~class(d2[[1]][[.x]])) # variable attributes
  
  for (i in seq_along(d2)) {
    # assign the same attributes of list 1 to the rest of the lists
    for (j in seq_along(varnames)) {
      if (varnames[[j]]  %in% names(d2[[i]])) {
        class(d2[[i]][[varnames[[j]]]]) <- vattr[[j]]
      } 
    }
  }
  
  df_merged <- rbindlist(d2, fill = TRUE, use.names = TRUE) %>%
    clean_names()
  
  clean <- df_merged %>%
    drop_na() %>%
    mutate(total_value = case_when(
      total_value == "Nat20" ~ "120",
      total_value == "Nat19" ~ "119",
      total_value == "Nat18" ~ "118",
      total_value == "Nat17" ~ "117",
      total_value == "Nat16" ~ "116",
      total_value == "Nat15" ~ "115",
      total_value == "Nat14" ~ "114",
      total_value == "Nat13" ~ "113",
      total_value == "Nat12" ~ "112",
      total_value == "Nat11" ~ "111",
      total_value == "Nat10" ~ "110",
      total_value == "Nat9"  ~ "109",
      total_value == "Nat8"  ~ "108",
      total_value == "Nat7"  ~ "107",
      total_value == "Nat6"  ~ "106",
      total_value == "Nat5"  ~ "105",
      total_value == "Nat4"  ~ "104",
      total_value == "Nat3"  ~ "103",
      total_value == "Nat2"  ~ "102",
      total_value == "Nat1"  ~ "101",
      TRUE                   ~ total_value)) %>%
    filter(total_value != "Unknown") %>%
    mutate(total_value = as.numeric(total_value)) %>%
    filter(total_value > 0) %>%
    mutate(episode = gsub("C2E", "", episode)) %>%
    mutate(episode = case_when(
      episode == "1519084800" ~ "020",
      episode == "1519171200" ~ "021",
      episode == "1519257600" ~ "022",
      episode == "1519344000" ~ "023",
      episode == "1519430400" ~ "024",
      episode == "1519516800" ~ "025",
      episode == "1519603200" ~ "026",
      episode == "1519689600" ~ "027",
      episode == "1519776000" ~ "028",
      TRUE                    ~ episode)) %>%
    mutate(episode = as.numeric(episode))
  
  return(clean)
  
}

# Heatmap prepper

heatmap_prep <- function(data){
  
  heat_data <- data %>%
    filter(character %in% the_nein) %>%
    mutate(character = case_when(
      character == "Nott" ~ "Veth/Nott",
      character == "Veth" ~ "Veth/Nott",
      TRUE                ~ character)) %>%
    mutate(total_value = case_when(
      total_value > 100 & total_value != 101 & total_value != 120 ~ "Other Nats",  
      total_value == 101                                          ~ "Nat1",
      total_value == 120                                          ~ "Nat20",
      total_value < 5                                             ~ "<5",
      total_value >= 5 & total_value < 10                         ~ "5-10",
      total_value >= 10 & total_value < 15                        ~ "10-15",
      total_value >= 15 & total_value < 20                        ~ "15-20",
      total_value >= 20 & total_value < 25                        ~ "20-25",
      total_value >= 25 & total_value < 30                        ~ "25-30",
      total_value >= 30 & total_value < 35                        ~ "30-35",
      total_value >= 35 & total_value < 40                        ~ "35-40",
      total_value >= 40 & total_value < 45                        ~ "40-45",
      total_value >= 45 & total_value < 50                        ~ "45-50")) %>%
    mutate(total_value = factor(total_value, levels = c("Nat1", "Nat20", "Other Nats",
                                                        "<5", "5-10", "10-15", "15-20",
                                                        "20-25", "25-30", "30-35", "35-40",
                                                        "40-45", "45-50"))) %>%
    group_by(character, total_value) %>%
    summarise(counts = n()) %>%
    group_by(character) %>%
    mutate(props = (counts / sum(counts))*100) %>%
    mutate(props = round(props, digits = 1)) %>%
    ungroup()
  
  return(heat_data)
  
}

# Multinomial logistic modeller

multinom_prep <- function(data){
  
  model_data <- data %>%
    filter(character %in% the_nein) %>%
    mutate(character = case_when(
      character == "Nott" ~ "Veth/Nott",
      character == "Veth" ~ "Veth/Nott",
      TRUE                ~ character)) %>%
    mutate(total_value = case_when(
      total_value == 101                                          ~ "Nat1",
      total_value == 120                                          ~ "Nat20",
      TRUE                                                        ~ "Other Totals")) %>%
    mutate(total_value = factor(total_value, levels = c("Other Totals", "Nat1", "Nat20")))
  
  # Split data into test and train sets
  
  sample_size <- floor(0.8 * nrow(model_data))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(model_data)), size = sample_size)
  train <- model_data[train_ind,]
  test <- model_data[-train_ind,]
  
  model <- nnet::multinom(total_value ~ character, data = train)
  
  return(model)
  
}

#----------------- DAMAGE AND HEALING ---------------------

cleaner <- function(data){
  data <- data[-c(1,2), ]
  
  data <- data %>%
    clean_names() %>%
    rename(episode = 1) %>%
    gather(key = character, value = value, 2:9) %>%
    mutate(episode = case_when(
      episode == "43132.0" ~ "02-01",
      episode == "43133.0" ~ "02-02",
      episode == "43134.0" ~ "02-03",
      episode == "43135.0" ~ "02-04",
      episode == "43136.0" ~ "02-05",
      episode == "43137.0" ~ "02-06",
      episode == "43138.0" ~ "02-07",
      episode == "43139.0" ~ "02-08",
      episode == "43140.0" ~ "02-09",
      episode == "43141.0" ~ "02-10",
      episode == "43142.0" ~ "02-11",
      episode == "43143.0" ~ "02-12",
      episode == "43144.0" ~ "02-13",
      episode == "43145.0" ~ "02-14",
      episode == "43146.0" ~ "02-15",
      episode == "43147.0" ~ "02-16",
      episode == "43148.0" ~ "02-17",
      episode == "43149.0" ~ "02-18",
      episode == "43150.0" ~ "02-19",
      episode == "43151.0" ~ "02-20",
      episode == "43152.0" ~ "02-21",
      episode == "43153.0" ~ "02-22",
      episode == "43154.0" ~ "02-23",
      episode == "43155.0" ~ "02-24",
      episode == "43156.0" ~ "02-25",
      episode == "43157.0" ~ "02-26",
      episode == "43158.0" ~ "02-27",
      episode == "43159.0" ~ "02-28",
      TRUE                 ~ episode)) %>%
    mutate(episode = gsub(".*-", "", episode)) %>%
    mutate(episode = as.numeric(episode)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(character = str_to_title(character))
  
  data[data == "N/A"] <- NA
  
  return(data)
}
