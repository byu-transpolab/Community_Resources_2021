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
  ld <- dfidx(libraries_estdata, idx = list("obs_id", "alt"), shape = "long",  
              choice = "chosen", idnames = "id", drop.index = FALSE)
  
  models <- list(
    "Car" = mlogit(chosen ~ duration_CAR | -1, data = ld),
    "MCLS" = mlogit(chosen ~ mclogsum | -1 , data = ld),
    "Attributes" = mlogit(chosen ~  classes + genealogy | -1 , data = ld),
    "All - Car" = mlogit(chosen ~  duration_CAR + classes + genealogy | -1 , data = ld),
    "All - Logsum" = mlogit(chosen ~  mclogsum + classes + genealogy | -1 , data = ld)
    
  )
  
  models
  
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


function (m1, m2, digits = getOption("digits"))  {
  m1y <- m1$model$chosen
  m2y <- m2$model$chosen
  m1n <- length(m1y)
  m2n <- length(m2y)
  if (m1n == 0 | m2n == 0) 
    stop("Could not extract dependent variables from models.")
  if (m1n != m2n) 
    stop(paste("Models appear to have different numbers of observations.\n", 
               "Model 1 has ", m1n, " observations.\n", "Model 2 has ", 
               m2n, " observations.\n", sep = ""))
  if (any(m1y != m2y)) {
    stop(paste("Models appear to have different values on dependent variables.\n"))
  }
  
  
  p1 <- augment(m1)
  p2 <- predprob(m2)
  if (!all(colnames(p1) == colnames(p2))) {
    stop("Models appear to have different values on dependent variables.\n")
  }
  whichCol <- match(m1y, colnames(p1))
  whichCol2 <- match(m2y, colnames(p2))
  if (!all(whichCol == whichCol2)) {
    stop("Models appear to have different values on dependent variables.\n")
  }
  m1p <- rep(NA, m1n)
  m2p <- rep(NA, m2n)
  for (i in 1:m1n) {
    m1p[i] <- p1[i, whichCol[i]]
    m2p[i] <- p2[i, whichCol[i]]
  }
  k1 <- length(coef(m1))
  k2 <- length(coef(m2))
  lm1p <- log(m1p)
  lm2p <- log(m2p)
  m <- lm1p - lm2p
  bad1 <- is.na(lm1p) | is.nan(lm1p) | is.infinite(lm1p)
  bad2 <- is.na(lm2p) | is.nan(lm2p) | is.infinite(lm2p)
  bad3 <- is.na(m) | is.nan(m) | is.infinite(m)
  bad <- bad1 | bad2 | bad3
  neff <- sum(!bad)
  if (any(bad)) {
    cat("NA or numerical zeros or ones encountered in fitted probabilities\n")
    cat(paste("dropping these", sum(bad), "cases, but proceed with caution\n"))
  }
  aic.factor <- (k1 - k2)/neff
  bic.factor <- (k1 - k2)/(2 * neff) * log(neff)
  v <- rep(NA, 3)
  arg1 <- matrix(m[!bad], nrow = neff, ncol = 3, byrow = FALSE)
  arg2 <- matrix(c(0, aic.factor, bic.factor), nrow = neff, 
                 ncol = 3, byrow = TRUE)
  num <- arg1 - arg2
  s <- apply(num, 2, sd)
  numsum <- apply(num, 2, sum)
  v <- numsum/(s * sqrt(neff))
  names(v) <- c("Raw", "AIC-corrected", "BIC-corrected")
  pval <- rep(NA, 3)
  msg <- rep("", 3)
  for (j in 1:3) {
    if (v[j] > 0) {
      pval[j] <- 1 - pnorm(v[j])
      msg[j] <- "model1 > model2"
    }
    else {
      pval[j] <- pnorm(v[j])
      msg[j] <- "model2 > model1"
    }
  }
  out <- data.frame(v, msg, format.pval(pval))
  names(out) <- c("Vuong z-statistic", "H_A", "p-value")
  cat(paste("Vuong Non-Nested Hypothesis Test-Statistic:", 
            "\n"))
  cat("(test-statistic is asymptotically distributed N(0,1) under the\n")
  cat(" null that the models are indistinguishible)\n")
  cat("-------------------------------------------------------------\n")
  print(out)
  return(invisible(NULL))
}