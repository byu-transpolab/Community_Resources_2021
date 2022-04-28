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
  
  
  # TODO: fix CAR times 
  # until then, we need to remove observations that have missing CAR times for their
  # chosen alternative
  nocar <- logitdata %>% filter(is.na(duration_CAR)) %>% pull(obs_id)
  
  
  logitdata  %>% filter(!obs_id %in% nocar)
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




estimate_librarymodels <- function(libraries_estdata){
  ld <- dfidx(libraries_estdata, idx = list("obs_id", "alt"), shape = "long",  
              choice = "chosen", idnames = "id", drop.index = FALSE)
  
  models <- list(
    "Car" = mlogit(chosen ~ duration_CAR | -1, data = ld),
    "MCLS" = mlogit(chosen ~ mclogsum | -1 , data = ld),
    "Attributes" = mlogit(chosen ~ classes +  genealogy + area | -1 , data = ld),
    "All - Car" = mlogit(chosen ~  duration_CAR + classes + genealogy + area| -1 , data = ld),
    "All - Logsum" = mlogit(chosen ~  mclogsum + classes + genealogy + area | -1 , data = ld)
  )
  
  models
  
}



estimate_parkmodels <- function(parks_estdata){
  ld <- dfidx(parks_estdata, idx = list("obs_id", "alt"), shape = "long",  
              choice = "chosen", idnames = "id", drop.index = FALSE)
  
  models <- list(
    "Car" = mlogit(chosen ~ duration_CAR | -1, data = ld),
    "MCLS" = mlogit(chosen ~ mclogsum | -1 , data = ld),
    "Attributes" = mlogit(chosen ~  yj_acres               + playground + volleyball + basketball + tennis + trail | -1 , data = ld),
    "All - Car" = mlogit(chosen ~  duration_CAR + yj_acres + playground + volleyball + basketball + tennis + trail | -1 , data = ld),
    "All - Logsum" = mlogit(chosen ~  mclogsum + yj_acres  + playground + volleyball + basketball + tennis + trail | -1 , data = ld)
  )
  models
}

#' Calculate the accessibility logsums implied by a destination choice model
#' 
#' @param m An MNL model
#' @param estadata The dataset on which the model was estimated
#' 
#' @return A tibble with the origin block group, and the total model logsum
#' 
calculate_access <- function(m, estdata){
  
  # Calculate utilities of the model, which get stored in .fitted
  aug <- my_augment(m)
  
  # The estimation data has lots of repeated possible origin and destination 
  # zones. We really only need to calculate one utility between each blockgroup 
  # and each grocery store. So, let's join the model output to the estimation 
  # datasets (because it has the blockgroup and destination ID's on it) 
  left_join(
    estdata %>% select(obs_id, alt, dest, geoid), 
    aug %>% select(id, alternative, .probability, .fitted, .resid), 
    by = c("obs_id" = "id", "alt" = "alternative")
  ) %>%
    #  now just get origin BG geoid, destination, id, and the model predicted utility
    select(geoid, dest, .fitted) %>%
    #  only need one of these for each pair
    group_by(geoid, dest) %>% slice(1) %>%
    #  Calculate the logsum of all destination options from each origin
    #  block group.
    group_by(geoid) %>%  summarise( access = logsum(.fitted) )
  
}

#' Compute whether any destination is within a given 
#' threshold
#' 
#' @param lsum Table of times and logsums
#' @param tholdvar Variable to use for threshold
#' @param thold Value to use for threshold
get_binary_access <- function(lsum, tholdvar, thold = 10){
  
  lsum %>%
    group_by(blockgroup) %>%
    summarise( minv = min({{tholdvar}}, na.rm = TRUE)) %>%
    mutate(within = minv < thold)
  
}


accessbin_data <- function(bgcentroid, gbin_access, lbin_access, pbin_access){
  bgcentroid %>%
    left_join(gbin_access %>% rename(grocery_min = minv, grocery = within),
              by = c("id" = "blockgroup")) %>%
    left_join(lbin_access %>% rename(library_min = minv, library = within),
              by = c("id" = "blockgroup")) %>%
    left_join(gbin_access %>% rename(park_min = minv, park = within),
              by = c("id" = "blockgroup")) 
  
}


#' Gather all of the logsum accessibilities into one tibble
#' 
#' @param bgcentroid
#' @param grocery_access
#' @param library_access
#' @param park_access
#'  
#' @return An sf object with a point for every block group centroid
#' 
accessls_data <- function(bgcentroid, grocery_access, library_access, park_access){
   
  bgcentroid %>%
    left_join(grocery_access %>% rename(grocery = access), by = c("id" = "geoid")) %>%
    left_join(library_access %>% rename(library = access), by = c("id" = "geoid")) %>%
    left_join(park_access %>% rename(park = access), by = c("id" = "geoid")) %>%
    mutate( total = rowSums(across(grocery:park), na.rm = TRUE) )
  
}

my_augment <- function (x, data = x$model, ...) {
  idx <- x$model$idx
  reg <- x$model %>% broom:::as_augment_tibble() %>% dplyr::select(-idx) %>% 
    dplyr::rename(chosen = 1, .probability = probabilities, 
                  .fitted = linpred) %>% 
    dplyr::mutate(id = idx[, 1], alternative = idx[, 2], .resid = as.vector(x$residuals)) %>% 
    dplyr::select(id, alternative, chosen, everything())
  reg
}


#' Horowitz likelihood ratio test of nested specifications
#' 
#' @param m1 an mlogit model
#' @param m2 an mlogit model
#' @return P-value of the horowitz test.
#' 
#' 
horowitz_lrtest <- function(m1, m2){
  g1 <- glance(m1)
  g2 <- glance(m2)
  
  ll0 <- g1$nobs * log(1 / length(m1$freq))
  
  rho21 <- if(is.nan(g1$rho2)) g1$rho20 else g1$rho2
  rho22 <- if(is.nan(g2$rho2)) g2$rho20 else g2$rho2
  

  t1 <- tidy(m1)
  t2 <- tidy(m2)
  
  k1 <- nrow(t1)
  k2 <- nrow(t2)
  
  q <- -1 * sqrt( -2 * (rho21 - rho22) * ll0  + (k1 - k2) )
  
  # p normal
  pval <- pnorm(q, lower.tail = TRUE)
  message("Test statistic value: ", q)
  
  pval
  
}
