#' Construct an estimation dataset
#' 
#' @param flows tibble with flows from block groups to destination zones
#' @param times  tibble with multimodal times between origins and destinations
#' @param ludata tibble with land use data
#' @param n_obs Number of simulated agents
#' @param n_alts Number of non-chosen alternatives
#' 
#' @return A tibble with simulated choice makers and their chosen alternative
#' 
#' 
make_estdata <- function(flows, times, ludata, acsdata, n_obs = 50, n_alts = 5,
                         day = "0: All Days (M-Su)", time = "All Day (12am-12am)") {
  
  
  # Get a list of chosen destinations  ----
  mydata <- flows %>%
    ungroup() %>%
    filter(time %in% time, day %in% day) %>%
    mutate(weight = flow / sum(flow)) %>%
    sample_n(n_obs, replace = TRUE, weight = weight) %>%
    transmute(obs_id = as.character(row_number()), geoid, dest = str_c("UT-", dest, sep = ""),
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
  mytimes <- times %>%
    pivot_wider(names_from = mode, values_from = duration)
    
  
  logitdata <- mydata %>%
    bind_cols(sampled_dests) %>%
    gather(key = "alt", value = "dest", -obs_id, -geoid, -validation) %>%
    mutate(chosen = alt == "alt_0") %>%
    arrange(obs_id, alt) %>%
    
    # append attributes
    left_join(attributes, by = c("dest" = "id")) %>%
  
    # append distances
    left_join(mytimes, by = c("geoid" = "blockgroup", "dest" = "resource")) %>%
    
    # append block group attributes
    left_join(acsdata, by = c("geoid")
              
    )
  
  # TODO: fix CAR times 
  # until then, we need to remove observations that have missing CAR times for their
  # chosen alternative
  nocar <- logitdata %>% filter(is.na(CAR)) %>% pull(obs_id)
  
  
  logitdata  %>% filter(!obs_id %in% nocar)
}


#' Estimate grocery store choice models
#' 
#' @param groceries_estdata
#' 
#' 
estimate_grocerymodels <- function(groceries_estdata){
  
  
  ld <- dfidx(groceries_estdata, idx = list("obs_id", "alt"), shape = "long",  
        choice = "chosen", idnames = "id", drop.index = FALSE)
  
  models <- list(
    "Base" = mlogit(chosen ~ CAR + type | -1 , data = ld),
    "Attributes" = mlogit(chosen ~ CAR + type  + pharmacy + ethnic + merch | -1 , data = ld),
    "Size" = mlogit(chosen ~ CAR + type  + pharmacy + ethnic + merch + registers + selfchecko| -1 , data = ld)
  )
  
  
  models
}

