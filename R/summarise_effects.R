#' Summarise Effects
#'
#' Generates the datasets which are read by the Summary page of the shiny app. This script should be run twice every time new data is added, and the outputs saved to replace the previous outputs in inst/shiny_data. Setting summarise_type = "order" generates meta_analysis_outputs.rds; setting summarise_type = "all" generates all_orders_meta_analysis_outputs.rds.
#'
#' @param prior_data The data.frame prior_data from inst/shiny_data
#' @param current_data The data.frame current_data from inst/shiny_data
#' @param summarise_type "order" for order-level summary (inst/shiny_data/meta_analysis_outputs.rds) or "all" for the overall effect on insects (inst/shiny_data/all-orders_meta_analysis_outputs.rds)
#' 
#' @export

summarise_effects <- function(prior_data, current_data, summarise_type){
  
  current_data$Treatment_error <- as.numeric(current_data$Treatment_error) # was numeric
  current_data$Control_error <- as.numeric(current_data$Control_error) # was character
  
  # filter out anything with blank errors
  current_data <- current_data %>%
    dplyr::filter(Control_error != "") %>%
    dplyr::filter(Treatment_error != "") %>%
    dplyr::filter(Treatment_N != "") %>%
    dplyr::filter(Control_N != "") %>%
    dplyr::filter(!is.na(Control_error_type)) %>% # added
    dplyr::filter(Control_error_type != "") %>% # added
    dplyr::filter(Treatment_error_type != "") %>% # added
    dplyr::filter(!is.na(Treatment_error_type)) # added
  # did not filter anything out
  
  # Mutate treatment error types ci95 and 95% Confidence interval to CI95
  current_data$Treatment_error_type[current_data$Treatment_error_type == "ci95"] <- "CI95"
  current_data$Treatment_error_type[current_data$Treatment_error_type == "95% Confidence interval "] <- "CI95"
  current_data$Treatment_error_type[current_data$Treatment_error_type == "se"] <- "Standard error"
  current_data$Treatment_error_type[current_data$Treatment_error_type == "Standard error "] <- "Standard error"
  current_data$Treatment_error_type[current_data$Treatment_error_type == "sd"] <- "Standard deviation"
  current_data$Control_error_type[current_data$Control_error_type == "ci95"] <- "CI95"
  current_data$Control_error_type[current_data$Control_error_type == "95% Confidence interval "] <- "CI95"
  current_data$Control_error_type[current_data$Control_error_type == "se"] <- "Standard error"
  current_data$Control_error_type[current_data$Control_error_type == "Standard error "] <- "Standard error"
  current_data$Control_error_type[current_data$Control_error_type == "sd"] <- "Standard deviation"
  
  # convert the error types to standard deviation in the current meta-analyses
  current_data$Treatment_error[current_data$Treatment_error_type == "CI95"] <- (current_data$Treatment_error[current_data$Treatment_error_type == "CI95"]/3.92) * sqrt(current_data$Treatment_N[current_data$Treatment_error_type == "CI95"])
  current_data$Treatment_error[current_data$Treatment_error_type == "Standard error"] <- current_data$Treatment_error[current_data$Treatment_error_type == "Standard error"] * sqrt(current_data$Treatment_N[current_data$Treatment_error_type == "Standard error"])
  current_data$Control_error[current_data$Control_error_type == "CI95"] <- (current_data$Control_error[current_data$Control_error_type == "CI95"]/3.92) * sqrt(current_data$Control_N[current_data$Control_error_type == "CI95"])
  current_data$Control_error[current_data$Control_error_type == "Standard error"] <- current_data$Control_error[current_data$Control_error_type == "Standard error"] * sqrt(current_data$Control_N[current_data$Control_error_type == "Standard error"])
  # 95% confidence interval is 3.92 standard errors (95CI/3.92)
  
  # then convert the strings so they match up with the conversion
  current_data$Treatment_error_type[current_data$Treatment_error_type == "CI95"] <- "Standard deviation"
  current_data$Treatment_error_type[current_data$Treatment_error_type == "Standard error"] <- "Standard deviation"
  current_data$Control_error_type[current_data$Control_error_type == "CI95"] <- "Standard deviation"
  current_data$Control_error_type[current_data$Control_error_type == "Standard error"] <- "Standard deviation"
  
  # add small fraction of pooled standard deviation for each study to control and treatment columns
  weighted_sd <- NULL
  # extract pooled standard deviations
  for(paper in current_data$Paper_ID){
    # weighted averages of control and treatment standard deviations, weighted by sample size
    control_sd <- weighted.mean(current_data$Control_error[current_data$Paper_ID == paper], current_data$Control_N[current_data$Paper_ID == paper])
    treatment_sd <- weighted.mean(current_data$Treatment_error[current_data$Paper_ID == paper], current_data$Treatment_N[current_data$Paper_ID == paper])
    # combine them to one final weighted average, divide it by 14, and add it to a vector where each element corresponds to a row of current_data
    means <- c(control_sd, treatment_sd)
    N <- c(sum(current_data$Control_N[current_data$Paper_ID == paper]), sum(current_data$Treatment_N[current_data$Paper_ID == paper]))
    weighted_sd <- c(weighted_sd, (weighted.mean(means, N))/14) 
    # 14 is the value for which the mean standard deviation of log response ratios for rows containing zeroes originally is closest to the standard deviation of the unadjusted log response ratios for all the rows with no zeroes originally.
    # In Gemini's words, "We applied an adjustment that prevented the mathematical explosion of variance in zero-count studies, at the cost of a slightly conservative estimate of overall effect size."
  }
  # add the column to current_data
  current_data$weighted_sd <- as.numeric(weighted_sd)
  
  # add to the means
  current_data$Treatment_mean <- current_data$Treatment_mean + current_data$weighted_sd
  current_data$Control_mean <- current_data$Control_mean + current_data$weighted_sd
  
  
  current_data <- current_data %>%
    dplyr::filter(Treatment_error >= 0 & Control_error >= 0)
  
  # calculate effect sizes from number, mean, and SD - data needs to be in wide format
  # Adds yi and vi columns to data
  current_data <- metafor::escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
                                  n1i = current_data$Treatment_N,
                                  n2i = current_data$Control_N,
                                  m1i = current_data$Treatment_mean,
                                  m2i = current_data$Control_mean,
                                  sd1i = current_data$Treatment_error,
                                  sd2i = current_data$Control_error,
                                  data = current_data)
  
  ##### Prior data
  prior_data <- prior_data %>%
    dplyr::mutate(Country = trimws(Country, "both"))
  
  ##### Initiate extraction loop
  
  if(summarise_type == "order"){
    data <- data.frame()
    data <- rbind(data, rep(NA, 9))
    names(data) <- c("order", "threat", "threat3", "pval", "beta", "ci.ub", "ci.lb", "n_studies", "n_effect_sizes")
    for(threat in c("9 Pollution", "8 Invasive & other problematic species, genes & diseases", "2 Agriculture and Aquaculture", "1 Residential & commercial development", "7 Natural system modifications")){
      for(order in unique(prior_data$Order)){
        custom_model_data <- current_data %>%
          dplyr::filter(IUCN_threat_category_1 == threat) %>%
          dplyr::filter(Order == order) %>%
          dplyr::filter(Biodiversity_metric == "Abundance") # %>%
        #dplyr::filter(Effect_size_type == "LogRR") # If more data is added 
        # that has a different error structure, error structure should be 
        # assigned before this point such that this filter accurately selects 
        # only LogRR data
        
        prior_custom_model_data <- prior_data %>%
          dplyr::filter(IUCN_threat_category_1 == threat) %>%
          dplyr::filter(Order == order) %>%
          dplyr::filter(Effect_size_type == "LogRR") %>%
          dplyr::filter(Biodiversity_metric == "Abundance")
        
        # remove extra columns from current meta-analyses so will merge on
        custom_model_data_simp <- custom_model_data %>%
          dplyr::select(-Treatment_N, -Control_N, -Treatment_mean, -Control_mean, 
                        -Treatment_error, -Control_error, -Control_error_type, -Treatment_error_type, 
                        -Extracted_from, -URL, -Language, -Database, -Life_history_stage, -Control_quantity, -Control_quantity_unit) %>%
          dplyr::mutate(Effect_size_type = "LogRR")
        
        # combine in the prior meta-analyses
        prior_custom_model_data <- prior_custom_model_data %>%
          rename(yi = Effect_size) %>%
          rename(vi = Sample_variance) %>%
          dplyr::select(-Aggregated, -Sample_variance_type)
        
        # # Change column type to numeric (from char)
        custom_model_data_simp$Treatment_quantity <- as.numeric(custom_model_data_simp$Treatment_quantity)
        prior_custom_model_data$Search_date <- as.character(prior_custom_model_data$Search_date)
        
        # row bind the current and prior meta-analyses together
        custom_model_data <- dplyr::bind_rows(custom_model_data_simp, prior_custom_model_data)
        #note that this leaves a lot of NAs
        
        if(nrow(custom_model_data) > 1){
          
          custom_model_data$IUCN_threat_category_3[is.na(custom_model_data$IUCN_threat_category_3)] <- custom_model_data$IUCN_threat_category_2[is.na(custom_model_data$IUCN_threat_category_3)]
          
          threat3 <- paste(unique(custom_model_data$IUCN_threat_category_3), collapse = "/")
          
          # Run metafor model
          custom_meta_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
                                               random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
                                               data = custom_model_data)
          
          # Extract relevant info
          pval <- custom_meta_model$pval
          beta <- as.numeric(custom_meta_model$beta) # beta was double
          ci.ub <- custom_meta_model$ci.ub
          ci.lb <- custom_meta_model$ci.lb
          n_studies <- length(unique(custom_model_data$Paper_ID))
          n_effect_sizes <- length(unique(custom_model_data$Observation_ID))
          datum <- cbind(order, threat, threat3, pval, beta, ci.ub, ci.lb, n_studies, n_effect_sizes)
          data <- rbind(data, datum)
          
        } else {
          threat3 <- NA
          pval <- NA
          beta <- NA
          ci.ub <- NA
          ci.lb <- NA
          n_studies <- 0
          n_effect_sizes <- 0
          datum <- cbind(order, threat, threat3, pval, beta, ci.ub, ci.lb, n_studies, n_effect_sizes)
          data <- rbind(data, datum)
        }
        
      }
    }
    # this has several warning messages. Rows with NAs omitted are fine; these are 
    #   rows where for whatever reason the effect size is NA and the variance is 0.
    #   The single-level factor(s) warning is about rows with only one data point. 
    #   The model fits Paper_ID as a random effect so gives a warning message where 
    #   the data come from only one paper e.g. Zygentoma pollution. This does not 
    #   affect the results. The warnings about non-positive sampling variances and 
    #   non-positive definite V are about rows such as Yadamsuren_2020_79 which have 
    #   identical treatment and control means and standard error of 0 on both. I 
    #   tested removing this value and re-running Odonata agriculture and it changed 
    #   the result but only marginally, so I assume this value is still being 
    #   included in the calculations and not massively skewing them - therefore, 
    #   this warning is not an issue.
    
    data <- data[-1,]
    rownames(data) <- NULL
    return(data)
  } else if(summarise_type == "all"){
    data <- data.frame()
    data <- rbind(data, rep(NA, 7))
    names(data) <- c("threat", "pval", "beta", "ci.ub", "ci.lb", "n_studies", "n_effect_sizes")
    for(threat in c("9 Pollution", "8 Invasive & other problematic species, genes & diseases", "2 Agriculture and Aquaculture", "1 Residential & commercial development", "7 Natural system modifications")){
      custom_model_data <- current_data %>%
        dplyr::filter(IUCN_threat_category_1 == threat) %>%
        dplyr::filter(Biodiversity_metric == "Abundance") # %>%
      #dplyr::filter(Effect_size_type == "LogRR") # If more data is added 
      # that has a different error structure, error structure should be 
      # assigned before this point such that this filter accurately selects 
      # only LogRR data
      
      prior_custom_model_data <- prior_data %>%
        dplyr::filter(IUCN_threat_category_1 == threat) %>%
        dplyr::filter(Effect_size_type == "LogRR") %>%
        dplyr::filter(Biodiversity_metric == "Abundance")
      
      # remove extra columns from current meta-analyses so will merge on
      custom_model_data_simp <- custom_model_data %>%
        dplyr::select(-Treatment_N, -Control_N, -Treatment_mean, -Control_mean, 
                      -Treatment_error, -Control_error, -Control_error_type, -Treatment_error_type, 
                      -Extracted_from, -URL, -Language, -Database, -Life_history_stage, -Control_quantity, -Control_quantity_unit) %>%
        dplyr::mutate(Effect_size_type = "LogRR")
      
      # combine in the prior meta-analyses
      prior_custom_model_data <- prior_custom_model_data %>%
        rename(yi = Effect_size) %>%
        rename(vi = Sample_variance) %>%
        dplyr::select(-Aggregated, -Sample_variance_type)
      
      # # Change column type to numeric (from char)
      custom_model_data_simp$Treatment_quantity <- as.numeric(custom_model_data_simp$Treatment_quantity)
      prior_custom_model_data$Search_date <- as.character(prior_custom_model_data$Search_date)
      
      # row bind the current and prior meta-analyses together
      custom_model_data <- dplyr::bind_rows(custom_model_data_simp, prior_custom_model_data)
      #note that this leaves a lot of NAs
      
      if(nrow(custom_model_data) > 1){
        
        custom_model_data$IUCN_threat_category_3[is.na(custom_model_data$IUCN_threat_category_3)] <- custom_model_data$IUCN_threat_category_2[is.na(custom_model_data$IUCN_threat_category_3)]
        
        
        # Run metafor model
        custom_meta_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
                                             random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
                                             data = custom_model_data)
        
        # Extract relevant info
        pval <- custom_meta_model$pval
        beta <- as.numeric(custom_meta_model$beta) # beta was double
        ci.ub <- custom_meta_model$ci.ub
        ci.lb <- custom_meta_model$ci.lb
        n_studies <- length(unique(custom_model_data$Paper_ID))
        n_effect_sizes <- length(unique(custom_model_data$Observation_ID))
        datum <- cbind(threat, pval, beta, ci.ub, ci.lb, n_studies, n_effect_sizes)
        data <- rbind(data, datum)
        
      } else {
        threat3 <- NA
        pval <- NA
        beta <- NA
        ci.ub <- NA
        ci.lb <- NA
        n_studies <- 0
        n_effect_sizes <- 0
        datum <- cbind(threat, pval, beta, ci.ub, ci.lb, n_studies, n_effect_sizes)
        data <- rbind(data, datum)
      }
    }
    data <- data[-1,]
    rownames(data) <- NULL
    return(data)
  } else {
    stop("summarise_type must be either 'order' (to split the data by order and threat) or 'all' (to split only by threat).")
  }
  
}
