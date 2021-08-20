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
  
  # id-name lookup
  nameid <- ludata %>%
    select(Name, id) %>% st_set_geometry(NULL) %>% as_tibble() %>%
    mutate(id = as.character(id))
  
  # Get a list of chosen destinations  ----
  mydata <- flows %>%
    ungroup() %>%
    filter(time %in% time, day %in% day) %>%
    mutate(weight = flow / sum(flow)) %>%
    sample_n(n_obs, replace = TRUE, weight = weight) %>%
    transmute(obs_id = as.character(row_number()), geoid, Name = dest,
              validation = sample(c(TRUE,FALSE), n(), TRUE, prob = c(0.2, 0.8))) %>%
    left_join(nameid, by = "Name") %>%
    select(-Name) %>%
    rename(alt_0 = id)
    
              
  
  # Get a list of non-chosen alternatives ---------
  sampled_dests <- lapply(1:n_obs, function(i){
    sample(ludata$id[which(mydata$obs_id[i] != ludata$id)], n_alts)
  }) %>%
    unlist() %>%
    matrix(ncol = n_alts) %>%
    as_tibble(.name_repair = ~ str_c("alt", 1:n_alts, sep = "_"))

  # Create dataset ----
  attributes <- ludata %>% st_set_geometry(NULL) %>%
    mutate(id = as.character(id))
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
    left_join(mytimes, by = c("geoid" = "destination", "dest" = "origin")) %>%
    
    
    # append block group attributes
    left_join(acsdata, by = c("geoid")
              
    )
  
  
  
}