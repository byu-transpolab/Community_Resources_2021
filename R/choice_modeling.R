#' Construct an estimation dataset
#' 
#' @param flows tibble with flows from block groups to destination zones
#' @param lsums tibble with multimodal times and logsums between origins and destinations. see calculate_logsums()
#' @param ludata tibble with land use data
#' @param n_obs Number of simulated agents
#' @param n_alts Number of non-chosen alternatives
#' 
#' @return A tibble with simulated choice makers and their chosen alternative
#' 
#' 
make_estdata <- function(flows, lsums, ludata, acsdata, n_obs = 50, n_alts = 5,
                         day = "0: All Days (M-Su)", time = "All Day (12am-12am)") {
  
  
  # Get a list of chosen destinations  ----
  mydata <- flows %>%
    ungroup() %>%
    filter(dest %in% lsums$resource, geoid %in% lsums$blockgroup) %>%
    filter(time %in% time, day %in% day) %>%
    mutate(weight = flow / sum(flow)) %>%
    sample_n(n_obs, replace = TRUE, weight = weight) %>%
    transmute(obs_id = as.character(row_number()), geoid, dest,
              validation = sample(c(TRUE,FALSE), n(), TRUE, prob = c(0.2, 0.8))) %>%
    rename(alt_0 = dest) %>%
    filter(alt_0 %in% ludata$id)
    
              
  
  # Get a list of non-chosen alternatives ---------
  sampled_dests <- lapply(1:nrow(mydata), function(i){
    sample(ludata$id[which(mydata$obs_id[i] != ludata$id)], n_alts)
  }) %>%
    unlist() %>%
    matrix(ncol = n_alts) %>%
    as_tibble(.name_repair = ~ str_c("alt", 1:n_alts, sep = "_"))

  # Create dataset ----
  attributes <- ludata %>% st_set_geometry(NULL)  %>% as_tibble()
    
  
  logitdata <- mydata %>%
    bind_cols(sampled_dests) %>%
    gather(key = "alt", value = "dest", -obs_id, -geoid, -validation) %>%
    mutate(chosen = alt == "alt_0") %>%
    arrange(obs_id, alt) %>%
    
    # append attributes
    left_join(attributes, by = c("dest" = "id")) %>%
  
    # append distances
    left_join(lsums, by = c("geoid" = "blockgroup", "dest" = "resource")) %>%
    
    # append block group attributes
    left_join(acsdata, by = c("geoid")
              
    )
  
  
  logitdata  
}


#' Estimate grocery store choice models
#' 
#' @param groceries_estdata
#' 
#' mlogit()
#' 
estimate_grocerymodels <- function(groceries_estdata){
  
  
  ld <- dfidx(groceries_estdata, idx = list("obs_id", "alt"), shape = "long",  
        choice = "chosen", idnames = "id", drop.index = FALSE)
  
  
  models <- list(
    "Car" = mlogit(chosen ~ duration_CAR | -1, data = ld),
    "MCLS" = mlogit(chosen ~ mclogsum | -1 , data = ld),
    "Attributes" = mlogit(chosen ~ type + pharmacy + ethnic + merch | -1 , data = ld),
    "Size" = mlogit(chosen ~ type + pharmacy + ethnic + merch + registers + selfchecko| -1 , data = ld),
    "All - Car" = mlogit(chosen ~ duration_CAR  + type + pharmacy + ethnic + merch + registers + selfchecko | -1, data = ld),
    "All - Logsum" = mlogit(chosen ~ mclogsum + type + pharmacy + ethnic + merch + registers + selfchecko | -1, data = ld)
  )
  
  
  models
}


modeltable <- function(models){
  modelsummary(models, estimate = "{estimate}({statistic}){stars}", stars = c('*' = .05, '**' = .01),
               statistic = NULL, note = "t-statistics in parentheses. * p < 0.5, ** < 0.1")
}


calculate_grocery_access <- function(grocery_times, groceries, grocery_models) {
  
  coef <- grocery_models$Size$coefficients
  
  # construct prediction frame
  grocery_times %>%
    spread(mode, duration) %>%
    filter(!is.na(CAR)) %>%
    left_join(groceries %>% st_set_geometry(NULL), by = c("resource" = "id"))  %>%
    
    # calculate utilities
    mutate(
      other = ifelse(type == "Other (Write in a description)", TRUE, FALSE),
      convenience = ifelse(type == "Convenience Store", TRUE, FALSE),
      u = 
        CAR * coef["CAR"] + 
        convenience * coef["typeConvenience Store"] + 
        other * coef["typeOther (Write in a description)"]  +
        pharmacy * coef["pharmacyTRUE"] + 
        ethnic * coef["ethnicTRUE"] + 
        merch * coef["merchTRUE"] + 
        registers * coef["registers"] + 
        selfchecko * coef["selfchecko"] ,
      eU = exp(u)
    )  %>%
    group_by(blockgroup) %>%
    summarise(
      logsum = log(sum(eU))
    )
  
}


estimate_librarymodels <- function(libraries_estdata){
  
}

estimate_parkmodels <- function(parks_estdata){
  ld <- dfidx(parks_estdata, idx = list("obs_id", "alt"), shape = "long",  
              choice = "chosen", idnames = "id", drop.index = FALSE)
  
  models <- list(
    "Car" = mlogit(chosen ~ duration_CAR | -1, data = ld),
    "MCLS" = mlogit(chosen ~ mclogsum | -1 , data = ld),
    "Attributes" = mlogit(chosen ~  playground + volleyball + basketball + tennis | -1 , data = ld),
    "All - Car" = mlogit(chosen ~  duration_CAR + playground + volleyball + basketball + tennis | -1 , data = ld),
    "All - Logsum" = mlogit(chosen ~  mclogsum + playground + volleyball + basketball + tennis | -1 , data = ld)
    
  )
  
  models
}
