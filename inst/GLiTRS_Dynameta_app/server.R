# Dynameta shiny app server script

# ---------------------------------------------------------------------------------------------

# Load packages
library(dplyr) # for data manipulation, part of tidyverse ("%>%" filter arrange relocate distinct)
library(DT) # for interactive tables
library(leaflet) # for mapping (renderLeaflet leaflet addTiles addCircleMarkers)
library(metafor) # for running meta-analytic models (escalc rma.mv forest)
library(shiny) # Required to run any Shiny app
library(shinyjs) # for enabling and disabling download button (enable disable show)
library(shinyWidgets) # for including a 'select all' option for filters (pickerInput)
library(tidyr) # for tidying messy data, part of tidyverse (drop_na)
library(readr) # for reading in csv files uploaded to the app
library(mapview) # for downloading the leaflet map

# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------



# Define the server function
server <- function(input, output) {

  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Load data that will be use throughout server.R file
  
  # =================================================================================================================
  # =================================================================================================================
  
  # -------------------------------------------------------------------------------------
  
  # observe event upload button for main meta-analysis
  shiny::observeEvent(input$Upload, {
    
    # read in file found on the file path chosen
    data() <- readr::read_csv(input$upload_data_to_analyse$datapath) %>% rbind(data)
    
    shinyjs::disable("Upload")
    
  })
  
  # observe event prior upload button
  shiny::observeEvent(input$prior_upload, {
    
    # read in file found on the file path chosen
    prior_data <- readr::read_csv(input$prior_data_to_analyse$datapath) %>% rbind(prior_data)
    
    shinyjs::disable("prior_upload")
    
  })
  

  

  
  # observe event upload button triggers checks
  shiny::observeEvent(input$confirm_bugs, {
    if(input$confirm_bugs == "Yes")
    shinyjs::enable("Upload")
    else if (input$confirm_bugs == "No") 
    shinyjs::disable("Upload")
    

  })
  
  # observe event upload button triggers checks
  shiny::observeEvent(input$prior_confirm_bugs, {
    if(input$prior_confirm_bugs == "Yes")
      shinyjs::enable("prior_upload")
    else if (input$prior_confirm_bugs == "No") 
      shinyjs::disable("prior_upload")
    
  })
  
  # Create reactive data object
  test_edit <- shiny::reactive({
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (!is.null(input$references_table_cell_edit)) {
      shinyjs::enable("Save")
    }
      
      # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
      if (is.null(input$references_table_cell_edit)) {
        shinyjs::disable("Save")
      }

  })
  
  # when either edits or deletions are made enable the delete and save buttons
  observe({
    
    # react function for whether there are edits, activates the save button
    test_edit()
    })
  
  # when either edits or deletions are made enable the delete and save buttons
  observe({
    
    # react function for whether there are deletes, activates the save button
    test_delete()
  })
  
  # Create reactive data object to check whether rows have been selected
  test_delete <- shiny::reactive({
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (!is.null(input$references_table_rows_selected)) {
      shinyjs::enable("Delete_rows")
    }
      
      # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
      if (is.null(input$references_table_rows_selected)) {
        shinyjs::disable("Delete_rows")
      }
  })
  
  # Create reactive data object
  test_react <- shiny::reactive({
    
    # When testing the package with shinytest, need to load the data in from the data_for_shinytest directory
    # or app can't find data like it does when user has installed the package
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (is.null(input$upload_data_to_analyse)) {
      shinyjs::hide("confirm_bugs")
      shinyjs::disable("Upload")
    }
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (is.null(input$prior_data_to_analyse)) {
      shinyjs::hide("prior_confirm_bugs")
      shinyjs::disable("prior_upload")
    }
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (!is.null(input$upload_data_to_analyse)) {
      
      # read in data to validate
      validation_test <- readr::read_csv(input$upload_data_to_analyse$datapath)
      
      # when data path is not null show info
      shinyjs::show("confirm_bugs")
      
      # Get the file encoding - returns encodings 'guesses' and associated confidence values
      encoding_and_confidence <- readr::guess_encoding(input$upload_data_to_analyse$datapath)
      
      # List the columns that must be included in the dataset
      necessary_columns <- c("Paper_ID", "Included", "Observation_ID", "Year", "Title", "Journal", "DOI", "Latitude",
                             "Longitude", "Country", "Taxa_level", "Taxa_name", "Order", "Experimental_year_start", "Experimental_year_end", "Biodiversity_metric", "IUCN_threat_category_1",
                             "Treatment", "Control", "Evidence_type", "Treatment_N", "Treatment_mean", "Treatment_error", "Treatment_error_type", "Control_N",
                             "Control_mean", "Control_error", "Control_error_type", "Contributor_name", "Search_date")
      
      # whole list of columns to catch anything that's extra and will make postgres bug (for glitrs meta-analyses)
      all_columns <- c("Paper_ID", "Included", "Observation_ID", "Author", "Year", "Title", "Journal", "DOI", "URL", "Language", "Database", "Latitude",
        "Longitude", "Country", "Taxa_level", "Taxa_name", "Order", "Family", "Genus", "Binomial", "Life_history_stage",
        "Experimental_year_start", "Experimental_year_end", "Sampling_method", "Sampling_intensity_unit", "Sampling_intensity",
        "Biodiversity_metric", "Unit", "IUCN_threat_category_1", "IUCN_threat_category_2", "IUCN_threat_category_3", "Treatment", "Treatment_quantity",
        "Treatment_quantity_unit", "Control", "Control_quantity", "Control_quantity_unit", "Extracted_from", "Evidence_type", "Treatment_N", "Treatment_mean", "Treatment_error", "Treatment_error_type", "Control_N",
        "Control_mean", "Control_error", "Control_error_type", "Contributor_name", "Search_date", "Notes")
      
      # check difference between sets of columns to catch those that need to be removed
      missing_columns <- setdiff(colnames(validation_test), all_columns)
      
      # Checks on data 1
      shiny::validate(
        
        # 1. Make sure file is a csv
        shiny::need(tools::file_ext(input$upload_data_to_analyse$name) == "csv", "File must be a .csv."),
        
        # 2. Make sure file encoding is either UTF-8 or ASCII and it is at least 90% confident
        shiny::need(encoding_and_confidence[1, 1] == "UTF-8" | encoding_and_confidence[1, 1] == "ASCII" & encoding_and_confidence[1, 2] >= 0.9,
                    paste0("The encoding of the file you are trying to upload (", encoding_and_confidence[1, 1], ") is not compatible with Dynameta. Ensure the file has UTF-8 or ASCII encoding.")),
        
        # 3. Make sure file has certain columns - these are the columns in the sample data that are needed for Dynameta (other columns can vary)
        # Tell user which columns in their dataframe are missing (if any)
        shiny::need(base::all(necessary_columns %in% colnames(readr::read_csv(input$upload_data_to_analyse$datapath))),
                    paste("Missing columns that must be present: ",
                          paste(setdiff(necessary_columns, colnames(readr::read_csv(input$upload_data_to_analyse$datapath))), collapse = ", "))),
        
        # 4. make sure that there are not any extra columns on top of those that possible for postgres tables
        need(length(missing_columns) == 0,
             paste("Additional columns that must be removed:",
                   if(length(missing_columns) > 0) paste(missing_columns, collapse = ", ")
             )),
        
        # 5. Make sure that the all effect size values are numeric, first checking whether the column is there
        if("Treatment_mean" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Treatment_mean) == TRUE, "Treatment_mean is non numeric")
          },
        
        if("Control_mean" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Control_mean) == TRUE, "Control_mean is non numeric")
          },
          
        if("Treatment_N" %in% colnames(validation_test)){
        shiny::need(is.numeric(validation_test$Treatment_N) == TRUE, "Treatment_N is non numeric")
          },
            
        if("Control_N" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Control_N) == TRUE, "Control_N is non numeric")
          },
              
        if("Treatment_error" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Treatment_error) == TRUE, "Treatment_error is non numeric")
          },
                
        if("Control_error" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Control_error) == TRUE, "Control_error is non numeric")
          },
                  
        # 6. Check that the coordinates are decimal degrees and of correct range
        if("Latitude" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Latitude) == TRUE, "Latitude is non numeric")
        },
          
        if("Longitude" %in% colnames(validation_test)){
          shiny::need(is.numeric(validation_test$Longitude) == TRUE, "Longitude is non numeric")
        },
         
        if(c("Latitude") %in% colnames(validation_test)){ 
          shiny::need(nrow(validation_test %>% filter(Latitude < 90 & Latitude > -90)) - nrow(validation_test) == 0, 
                      paste0("Latitude is out of range ", 
                             "(see row number ",
                             validation_test %>% 
                               mutate(row_no = row_number()) %>% 
                               filter(Latitude > 90 | Latitude < -90) %>%
                               dplyr::select(row_no), ")"))},
        
        if(c("Longitude") %in% colnames(validation_test)){ 
          shiny::need(nrow(validation_test %>% filter(Longitude < 180 & Longitude > -180)) - nrow(validation_test) == 0, 
                      paste0("Longitude is out of range ", 
                             "(see row number ",
                             validation_test %>% 
                               mutate(row_no = row_number()) %>% 
                               filter(Longitude > 180 | Longitude < -180) %>%
                               dplyr::select(row_no), ")"))
        },
        
        tryCatch({
          # 7. check that year end and start date are the correct way around, if the arithmetic doesn't work flag likely not numeric
          shiny::need(nrow(validation_test %>% 
                             mutate(year_diff = Experimental_year_start - Experimental_year_end) %>%
                             filter(year_diff > 0)) == 0, 
                      paste0("Experimental_year_start is greater than Experimental_year_end ", 
                             "(see row number ",
                             validation_test %>% 
                               mutate(row_no = row_number()) %>% 
                               mutate(year_diff = Experimental_year_start - Experimental_year_end) %>%
                               filter(year_diff > 0) %>%
                               pull(row_no), ")"))
        }, error = function(x) print("Experimental_year_start or Experimental_year_end are non numeric (if not missing)")),
        
        #8. check for duplicted observation IDs
        shiny::need(sum(duplicated(validation_test$Observation_ID)) == 0, "Observation_ID is duplicated")
        
      )
      
    }
  })
    
  test_react_prior <-  shiny::reactive({
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (!is.null(input$prior_data_to_analyse)) {
      
      # read in data to validate
      validation_test <- readr::read_csv(input$prior_data_to_analyse$datapath)
      
      # when data path is not null show info
      shinyjs::show("prior_confirm_bugs")
      
      # Get the file encoding - returns encodings 'guesses' and associated confidence values
      encoding_and_confidence <- readr::guess_encoding(input$prior_data_to_analyse$datapath)
      
      # List the columns that must be included in the dataset
      necessary_columns_prior <- c("Paper_ID", "Observation_ID", "Year", "Title", "Journal", "DOI", "Latitude",
                                   "Longitude", "Country", "Taxa_level", "Taxa_name", "Order", "Experimental_year_start", "Experimental_year_end", "Biodiversity_metric", "IUCN_threat_category_1",
                                   "Treatment", "Control", "Evidence_type", "Effect_size", "Effect_size_type", "Sample_variance", "Sample_variance_type", "Aggregated", "Contributor_name", "Search_date")
      
      # whole list of columns to catch anything that's extra and will make postgres bug (for glitrs meta-analyses)
      all_columns <- c("Paper_ID", "Included", "Observation_ID", "Author", "Year", "Title", "Journal", "DOI", "URL", "Language", "Database", "Latitude",
                       "Longitude", "Country", "Taxa_level", "Taxa_name", "Order", "Family", "Genus", "Binomial", "Life_history_stage",
                       "Experimental_year_start", "Experimental_year_end", "Sampling_method", "Sampling_intensity_unit", "Sampling_intensity",
                       "Biodiversity_metric", "Unit", "IUCN_threat_category_1", "IUCN_threat_category_2", "IUCN_threat_category_3", "Treatment", "Treatment_quantity",
                       "Treatment_quantity_unit", "Control", "Control_quantity", "Control_quantity_unit", "Extracted_from", "Evidence_type", "Effect_size", "Effect_size_type", "Sample_variance", "Sample_variance_type", "Aggregated",
                       "Contributor_name", "Search_date", "Notes")
      
      # check difference between sets of columns to catch those that need to be removed
      missing_columns <- setdiff(colnames(validation_test), all_columns)
      
       # Checks on data 1
      shiny::validate(
        
        # 1. Make sure file is a csv
        shiny::need(tools::file_ext(input$prior_data_to_analyse$name) == "csv", "File must be a .csv."),
        
        # 2. Make sure file encoding is either UTF-8 or ASCII and it is at least 90% confident
        shiny::need(encoding_and_confidence[1, 1] == "UTF-8" | encoding_and_confidence[1, 1] == "ASCII" & encoding_and_confidence[1, 2] >= 0.9,
                    paste0("The encoding of the file you are trying to upload (", encoding_and_confidence[1, 1], ") is not compatible with Dynameta. Ensure the file has UTF-8 or ASCII encoding.")),
        
        # 3. Make sure file has certain columns - these are the columns in the sample data that are needed for Dynameta (other columns can vary)
        # Tell user which columns in their dataframe are missing (if any)
        shiny::need(base::all(necessary_columns_prior %in% colnames(readr::read_csv(input$prior_data_to_analyse$datapath))),
                    paste("Missing columns that must be present: ",
                          paste(setdiff(necessary_columns_prior, colnames(readr::read_csv(input$prior_data_to_analyse$datapath))), collapse = ", "))),
        
        # 4. make sure that there are not any extra columns on top of those that possible for postgres tables
        need(length(missing_columns) == 0,
             paste("Additional columns that must be removed:",
                   if(length(missing_columns) > 0) paste(missing_columns, collapse = ", ")
             )),
        
        # 5. Make sure that the all effect size values are numeric
        shiny::need(is.numeric(validation_test$Effect_size) == TRUE, "Effect_size is non numeric"),
        shiny::need(is.numeric(validation_test$Sample_variance ) == TRUE, "Sample_variance is non numeric"),
        
        # 6. Check that the coordinates are decimal degrees and of correct range
        shiny::need(is.numeric(validation_test$Latitude) == TRUE, "Latitude is non numeric"),
        shiny::need(is.numeric(validation_test$Longitude) == TRUE, "Longitude is non numeric"),
        shiny::need(nrow(validation_test %>% filter(Latitude < 90 & Latitude > -90)) - nrow(validation_test) == 0, 
                    paste0("Latitude is out of range ", 
                           "(see row number ",
                           validation_test %>% 
                             mutate(row_no = row_number()) %>% 
                             filter(Latitude > 90 | Latitude < -90) %>%
                             dplyr::select(row_no), ")")),
        shiny::need(nrow(validation_test %>% filter(Longitude < 180 & Longitude > -180)) - nrow(validation_test) == 0, 
                    paste0("Longitude is out of range ", 
                           "(see row number ",
                           validation_test %>% 
                             mutate(row_no = row_number()) %>% 
                             filter(Longitude > 180 | Longitude < -180) %>%
                             dplyr::select(row_no), ")")),
        
        tryCatch({
          # 7. check that year end and start date are the correct way around, if the arithmetic doesn't work flag likely not numeric
          shiny::need(nrow(validation_test %>% 
                             mutate(year_diff = Experimental_year_start - Experimental_year_end) %>%
                             filter(year_diff > 0)) == 0, 
                      paste0("Experimental_year_start is greater than Experimental_year_end ", 
                             "(see row number ",
                             validation_test %>% 
                               mutate(row_no = row_number()) %>% 
                               mutate(year_diff = Experimental_year_start - Experimental_year_end) %>%
                               filter(year_diff > 0) %>%
                               pull(row_no), ")"))
        }, error = function(x) print("Experimental_year_start or Experimental_year_end are non numeric")),
        
        #8. check for duplicted observation IDs
        shiny::need(sum(duplicated(validation_test$Observation_ID)) == 0, "Observation_ID is duplicated")
        
      )
      
    }
    
  })
  
  # Create reactive data object
  data <- shiny::reactive({
    
    # Sample data in csv files for prior meta-analyses
    current_data <- readRDS("../shiny_data/current_data.rds")
    
    ###### Convert treatment and control errors to numeric (currently character)
    current_data$Treatment_error <- as.numeric(current_data$Treatment_error)
    current_data$Control_error <- as.numeric(current_data$Control_error)
    
    # filter out anything with blank errors
    current_data <- current_data %>%
      filter(Control_error != "") %>%
      filter(Treatment_error != "") %>%
      filter(Treatment_N != "") %>%
      filter(Control_N != "") %>%
      filter(!is.na(Control_error_type)) %>% # added
      filter(Control_error_type != "") %>% # added
      filter(Treatment_error_type != "") %>% # added
      filter(!is.na(Treatment_error_type)) # added
    
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
    
    # set up data_edit object for saving edits and disable save and delete options
    data_edit <<- current_data
    
  })
  
  # Create reactive data object
  prior_data <- shiny::reactive({

    # Sample data in csv files for prior meta-analyses
    prior_data <- readRDS("../shiny_data/prior_data.rds") %>%
      mutate(Country = trimws(Country, "both"))
    
    # set up data_edit object for saving edits and disable save and delete options
    prior_data_edit <<- prior_data
    
  })
  
  # -------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Intro tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ---------------------------------------------------------------------------------------------------------------
  
  ### Table for overview of papers included
  
  # Add table legend
  output$table_legend_overview <- shiny::renderText({
    
    # Calculate number of rows and columns
    nrow <- nrow(data())
    ncol <- ncol(data())
    
    # Calculate number of rows and columns for prior meta analysis
    prior_nrow <- nrow(prior_data())
    prior_ncol <- ncol(prior_data())
    
    # Get list of column names
    colnames <- paste(colnames(data()), collapse = "<br>")
    
    
    base::paste0("<b>Table 1.</b>", " Total number of papers (Paper_ID) and data points available to investigate each threat category. ", "The insect biodiversity meta-analytic database currently contains ", nrow + prior_nrow, " effect sizes.")
  })
  
  # Add sample size table
  output$sample_sizes_overview <- shiny::renderTable({
    
    # Initialise empty data frame
    new_row <- list()

    # Calculate statistics for each paper
    for (i in unique(c(data()$IUCN_threat_category_1, prior_data()$IUCN_threat_category_1))) { # data is whole spreadsheet

      threat_subset <- data() %>%
        dplyr::filter(IUCN_threat_category_1 %in% i)

      prior_threat_subset <- prior_data() %>%
        dplyr::filter(IUCN_threat_category_1 %in% i)

      # make new row which will be added to sample_sizes_table
      new_row[[i]] <- base::data.frame(
        "Threat_category" = i,
        "Number_of_papers" = length(unique(c(threat_subset$Paper_ID, prior_threat_subset$Paper_ID))), # number of unique papers per threat
        "Total_effect_sizes" = nrow(threat_subset) + nrow(prior_threat_subset)
        ) # number of instances
    }

    # Put table in alphabetical order based on paper_ID
    sample_sizes_table <- data.table::rbindlist(new_row) %>%
      arrange(Threat_category) %>%
      rename("Threat category"= "Threat_category") %>%
      rename("Total papers"= "Number_of_papers") %>%
      rename("Total effect sizes"= "Total_effect_sizes")
    
  },
  
  striped = TRUE
  
  )
  
  # ---------------------------------------------------------------------------------------------------------------
  
  ### Text for data summary
  
  # Add data summary text
  output$data_errors <- renderText({
    test_react()
    
  })
  
  # Add data summary text
  output$prior_data_errors <- renderText({
    test_react_prior()
    
  })
  
  
  shiny::observeEvent(input$necessary_columns_info_btn, {
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Columns that must be included in your GLiTRS meta-analysis. Any in bold need to be populated:",
        size = "l",
        HTML("<b>Paper_ID</b> - The paper from which the comparison was drawn (categorical)<br>
            <b>Included</b> - The exclusion or inclusion status of that paper. Included papers should be recorded here as “Y” for yes, and “N” if the paper was excluded during screening according to the exclusion criteria. Rows with “N” in Included should be blank for all other columns of that row (categorical)<br>
            <b>Observation_ID</b> - The comparison of that row, which should be unique to each row (categorical)<br>
            Author - The full list of authors of the paper (character)<br>
            <b>Year</b> - The year of publication of the paper (numeric)<br>
            <b>Title</b> - The paper title (character)<br>
            <b>Journal</b> - The journal of the paper (character)<br>
            <b>DOI</b> - The DOI of the paper (character)<br>
            URL - The URL of the paper (character)<br>
            Language - The language of the Paper_ID in that row (categorical)<br>
            Database - The database from which the Paper_ID in that row was found (categorical)<br>
            <b>Latitude</b> - The latitude coordinate of the observation in that row (numeric)<br>
            <b>Longitude</b> - The longitude coordinate of the observation in that row (numeric)<br>
            <b>Country</b> - The country in which the observation in that row is located (categorical)<br>
            <b>Taxa_level</b> - The taxonomic level that is common to all of the biodiversity recorded in that row (e.g. if both the treatment and control measures consider only taxa in one family, then the Taxa_level would be “Family”). If the ‘Taxa_level’ recorded is family (or higher), both ‘Genus’ and ‘Binomial’ should be left blank. (categorical)<br>
            <b>Taxa_name</b> - The name of the taxonomic level that is common to all of the biodiversity recorded in that row (e.g. if both the treatment and control measures consider only taxa in the Apidae, then the Taxa_name 12ouldd be “Apidae”) (categorical)<br>
            <b>Order</b> - The taxonomic order of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Family - The taxonomic family of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Genus - The taxonomic genus of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Binomial - The Latin binomial of the species in that row, if it has been recorded (Latin binomials should be of the format “Species genus” (e.g. Apis mellifera) (categorical)<br>
            Life_history_stage</b> - The life-history stage of the taxa in that row. Life-history stage should be one of “Egg”, “Larval”,or “Adult” (categorical)<br>
            <b>Experimental_year_start</b> - The year in which data collection started (numeric)<br>
            <b>Experimental_year_end</b> - The year in which data collection ended (numeric)<br>
            Sampling_method - The sampling method used to sample the biodiversity measure (character)<br>
            Sampling_intensity_unit - The unit of sampling effort or intensity (if applicable for quasi experimental effect sizes) (character)<br>
            Sampling_intensity - The measure of sampling effort or intensity (if applicable for quasi experimental effect sizes) (numeric)<br>
            <b>Biodiversity_metric</b> - The metric of biodiversity measured. Biodiversity_metric should typically be one of “Abundance”, “Richness”, or “Biomass” (categorical)<br>
            Unit - The unit of measure for the biodiversity metric recorded in that row (if applicable) (character)<br>
            <b>IUCN_threat_category_1</b> - Broadest level of IUCN threat (categorical)<br>
            IUCN_threat_category_2 - Secondary level of IUCN threat (e.g. 2.1 Annual & perennial non-timber crops) (categorical)<br>
            IUCN_threat_category_3 - Tertiary level of IUCN threat (e.g. 2.1.2 Small-holder farming) (categorical)<br>
            <b>Treatment</b> - Name of the threat measured in that row (e.g. insecticide) (categorical)<br>
            Treatment_quantity - The quantity of treatment reported by the authors for that observation (numeric)<br>
            Treatment_quantity_unit - The unit of measure for the numeric value in Treatment_quantity (character)<br>
            <b>Control</b> - Name of the control measured in that row (e.g. no insecticide) (categorical)<br>
            Control_quantity - The quantity of threat in the control reported by the authors for that observation (i.e. in some comparisons the control won’t be a true zero, but rather some lower level of threat) (numeric)<br>
            Control_quantity_unit - The unit of measure for the numeric value in Control_quantity (character)<br>
            Extracted_from - The figure or table from which the data in that row was extracted (character)<br>
            <b>Evidence_type</b> - The type of evidence for the data in that row. Type of evidence should be either “Experimental” or “Quasi-experimental”. Experimental refers to a deliberate treatment manipulation by the authors, whereas quasi-experimental is where there was no deliberate manipulation (see ‘Background’ for more details) (categorical)<br>
            <b>Treatment_N</b> - The number of treatment sites from which the mean and error values were drawn (numeric)<br>
            <b>Treatment_mean</b> - The mean biodiversity value across all treatment sites for that comparison (numeric)<br>
            <b>Treatment_error</b> - The raw treatment error value reported by the authors for that observation (numeric)<br>
            <b>Treatment_error_type</b> - The type of error (or uncertainty) recorded in Treatment_error. Typically this will be one of Standard error, Standard deviation, or 95% confidence interval (character)<br>
            <b>Control_N</b> - The number of control sites from which the mean and error values were drawn (numeric)<br>
            <b>Control_mean</b> - The mean biodiversity value across all control sites for that comparison (numeric)<br>
            <b>Control_error</b> - The raw control error value reported by the authors for that observation (numeric)<br>
            <b>Control_error_type</b> - The type of error recorded on Control_error. Typically this will be one of “Standard error”, “Standard deviation”, or “95% confidence interval” (character)<br>
            <b>Contributor_name</b> - The name of the person that collected that effect size (character)<br>
            <b>Search_date</b> - The date of the literature search on which this record was found (date)<br>
            Notes - This field should be used for any relevant descriptions that don’t fall within core data, or can’t be categorised into clearly defined factors in the associated metadata, or to provide explanations for the associated metadata (character)<br>"),
        easyClose = TRUE
      )
    )
  })
  
  shiny::observeEvent(input$prior_necessary_columns_info_btn, {
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Columns that must be included in your prior meta-analysis. Any in bold need to be populated:",
        size = "l",
        HTML("<b>Paper_ID</b> - The paper from which the comparison was drawn (categorical)<br>
            <b>Observation_ID</b> - The comparison of that row, which should be unique to each row (categorical)<br>
            Author - The full list of authors of the paper (character)<br>
            <b>Year</b> - The year of publication of the paper (numeric)<br>
            <b>Title</b> - The paper title (character)<br>
            <b>Journal</b> - The journal of the paper (character)<br>
            <b>DOI</b> - The DOI of the paper (character)<br>
            URL - The URL of the paper (character)<br>
            Language - The language of the Paper_ID in that row (categorical)<br>
            Database - The database from which the Paper_ID in that row was found (categorical)<br>
            <b>Latitude</b> - The latitude coordinate of the observation in that row (numeric)<br>
            <b>Longitude</b> - The longitude coordinate of the observation in that row (numeric)<br>
            <b>Country</b> - The country in which the observation in that row is located (categorical)<br>
            <b>Taxa_level</b> - The taxonomic level that is common to all of the biodiversity recorded in that row (e.g. if both the treatment and control measures consider only taxa in one family, then the Taxa_level would be “Family”). If the ‘Taxa_level’ recorded is family (or higher), both ‘Genus’ and ‘Binomial’ should be left blank. (categorical)<br>
            <b>Taxa_name</b> - The name of the taxonomic level that is common to all of the biodiversity recorded in that row (e.g. if both the treatment and control measures consider only taxa in the Apidae, then the Taxa_name 12ouldd be “Apidae”) (categorical)<br>
            <b>Order</b> - The taxonomic order of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Family - The taxonomic family of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Genus - The taxonomic genus of the biodiversity measured in that row, for both the treatment and control (categorical)<br>
            Binomial - The Latin binomial of the species in that row, if it has been recorded (Latin binomials should be of the format “Species genus” (e.g. Apis mellifera) (categorical)<br>
            Life_history_stage</b> - The life-history stage of the taxa in that row. Life-history stage should be one of “Egg”, “Larval”,or “Adult” (categorical)<br>
            <b>Experimental_year_start</b> - The year in which data collection started (numeric)<br>
            <b>Experimental_year_end</b> - The year in which data collection ended (numeric)<br>
            Sampling_method - The sampling method used to sample the biodiversity measure (character)<br>
            Sampling_intensity_unit - The unit of sampling effort or intensity (if applicable for quasi experimental effect sizes) (character)<br>
            Sampling_intensity - The measure of sampling effort or intensity (if applicable for quasi experimental effect sizes) (numeric)<br>
            <b>Biodiversity_metric</b> - The metric of biodiversity measured. Biodiversity_metric should typically be one of “Abundance”, “Richness”, or “Biomass” (categorical)<br>
            Unit - The unit of measure for the biodiversity metric recorded in that row (if applicable) (character)<br>
            <b>IUCN_threat_category_1</b> - Broadest level of IUCN threat (categorical)<br>
            IUCN_threat_category_2 - Secondary level of IUCN threat (e.g. 2.1 Annual & perennial non-timber crops) (categorical)<br>
            IUCN_threat_category_3 - Tertiary level of IUCN threat (e.g. 2.1.2 Small-holder farming) (categorical)<br>
            <b>Treatment</b> - Name of the threat measured in that row (e.g. insecticide) (categorical)<br>
            Treatment_quantity - The quantity of treatment reported by the authors for that observation (numeric)<br>
            Treatment_quantity_unit - The unit of measure for the numeric value in Treatment_quantity (character)<br>
            <b>Control</b> - Name of the control measured in that row (e.g. no insecticide) (categorical)<br>
            Control_quantity - The quantity of threat in the control reported by the authors for that observation (i.e. in some comparisons the control won’t be a true zero, but rather some lower level of threat) (numeric)<br>
            Control_quantity_unit - The unit of measure for the numeric value in Control_quantity (character)<br>
            Extracted_from - The figure or table from which the data in that row was extracted (character)<br>
            <b>Evidence_type</b> - The type of evidence for the data in that row. Type of evidence should be either “Experimental” or “Quasi-experimental”. Experimental refers to a deliberate treatment manipulation by the authors, whereas quasi-experimental is where there was no deliberate manipulation (see ‘Background’ for more details) (categorical)<br>
            <b>Effect_size</b> - The effect size for the difference between the treatment and control in that comparison (numeric)<br>
            <b>Effect_size_type</b> - The type of effect size for the comparison in that row (e.g. Hedge’s D, log response ratio (character)<br>
            <b>Sample_variance</b> - The sample variance for the difference between the treatment and control in that comparison (numeric)<br>
            <b>Sample_variance_type</b> - The type of sample variance for the comparison in that row (character)<br>
            <b>Aggregated</b> - Whether the effect size in that row isfor a single comparison, or an aggregation of comparisons (Y for aggregated; N for not aggregated). As far as possible always try to include the non-aggregated effect size for each comparison (categorical)<br>
            <b>Contributor_name</b> - The name of the person that uploaded that prior effect size (character)<br>
            <b>Search_date</b> - The date on which the search of the literature was carried out for that effect size (date)<br>
            Notes - This field should be used for any relevant descriptions that don’t fall within core data, or can’t be categorised into clearly defined factors in the associated metadata, or to provide explanations for the associated metadata (character)<br>"),
        easyClose = TRUE
      )
    )
  })
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # Map of where data comes from
  # Make reactive object:
  leaflet_map <- reactive({
    
    # Filter the data to include data points which have long and lat available
    coord_data <- data() %>%
      mutate(Longitude = as.numeric(Longitude)) %>%
      mutate(Latitude = as.numeric(Latitude)) %>%
      tidyr::drop_na(Longitude, Latitude) %>%
      dplyr::filter(Longitude != "." & Latitude != ".") %>%
      dplyr::select(Latitude, Longitude, Title, Paper_ID, Observation_ID, IUCN_threat_category_1)
    
    # filter the data to include also prior collected effect sizes
    prior_coord_data <- prior_data() %>%    
      mutate(Longitude = as.numeric(Longitude)) %>%
      mutate(Latitude = as.numeric(Latitude)) %>%
      tidyr::drop_na(Longitude, Latitude) %>%
      dplyr::filter(Longitude != "." & Latitude != ".") %>%
      dplyr::select(Latitude, Longitude, Title, Paper_ID, Observation_ID, IUCN_threat_category_1)
    
    # Make leaflet map
    leaflet_map <- leaflet::leaflet(data = rbind(coord_data, prior_coord_data)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
                                popup = ~paste("<b>Title</b>: ", Title, "<b><br>Paper_ID: </b>", Paper_ID, "<b><br>Observation_ID: </b>", Observation_ID, "<b><br>Threat category: </b>", IUCN_threat_category_1), # Click on point to get clickable link to paper if available
                                clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
    
  })
  
  output$map <- leaflet::renderLeaflet({
    
    req(leaflet_map())
    
    map <- leaflet_map()
    
  })
  
  
  # Calculate total number of data points
  total_data_points <- shiny::reactive({
    
    return(nrow(data()) + nrow(prior_data()))
    
    
  })
  
  # Calculate number of data points with coordinates available
  data_with_coords <- shiny::reactive({
    
    prior_coords <- prior_data() %>% dplyr::select(Latitude, Longitude) %>%
      filter(!is.na(Latitude)) %>%
      filter(Latitude != "") %>%
      filter(!is.na(Longitude)) %>% 
      filter(Longitude != "") %>%
      tally() %>% pull(n)
    
    current_coords <- data() %>% dplyr::select(Latitude, Longitude) %>%
      filter(!is.na(Latitude)) %>%
      filter(Latitude != "") %>%
      filter(!is.na(Longitude)) %>% 
      filter(Longitude != "") %>%
      tally() %>% pull(n)
    
    return(prior_coords + current_coords)
    
  })
  
  # Add map figure legend
  output$map_figure_legend <- shiny::renderText({
    base::paste("<b>Figure 1.</b>", "Map showing location of effect sizes, for both current GLiTRS and prior meta-analyses. Currently", data_with_coords(), "out of", total_data_points(),
                "data points have associated latitude and longitude coordinates. You can zoom in on, and click on, data clusters to explore the map. 
                Clicking on an individual point provides a link to the details of that effect size.")
  })
  
  # Download map button
  output$download_map <- downloadHandler(
    
    filename = function() {
      paste0("map_from_Dynameta_", Sys.Date(), ".png", sep="")
    },
    
    content = function(file) {
      mapview::mapshot(x = leaflet_map(),
                       file = file,
                       cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
                       selfcontained = FALSE) # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    }
  )
  
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Run models tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ----------------------------------------------------------------------------------------------------
  
  # Make reactive IUCN threat category choices
  output$reactive_iucn_threat_category <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "iucn_threat_category",
                              label = "IUCN Threat:",
                              choices = unique(c(data()$IUCN_threat_category_1, prior_data()$IUCN_threat_category_1)),
                              selected = NULL,
                              multiple = FALSE) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive location choices
  output$reactive_location <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "location",
                              label = "Location(s):",
                              choices = unique(c(data()$Country, prior_data()$Country)),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive taxa order choices
  output$reactive_taxa_order <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "taxa_order",
                              label = "Taxonomic order(s):",
                              choices = unique(c(data()$Order, prior_data()$Order)),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE))
  })
  
  # Make reactive biodiversity metric choices
  output$reactive_biodiversity_metric_category <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "biodiversity_metric_category",
                              label = "Biodiversity metric(s):",
                              choices = unique(c(data()$Biodiversity_metric, prior_data()$Biodiversity_metric)),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE))
    
  })
    
    # Make reactive biodiversity metric choices
    output$reactive_effect_size_category <- shiny::renderUI({
      shinyWidgets::pickerInput(inputId = "effect_size_category",
                                label = "Effect size type:",
                                choices = unique(c(data()$Effect_size_type, prior_data()$Effect_size_type)),
                                selected = NULL,
                                multiple = FALSE,
                                options = list(`actions-box` = TRUE))
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  ### Run model
  
  # Filter the data based on user input and run model once the run model button has been pressed
  custom_model <- shiny::eventReactive(input$run_custom_model, {
    
    shiny::validate(
      shiny::need(input$iucn_threat_category != "", "Please select at least one threat category."),
      shiny::need(input$location != "", "Please select at least one location."),
      shiny::need(input$taxa_order != "", "Please select at least one taxonomic order."),
      shiny::need(input$biodiversity_metric_category != "", "Please select at least one biodiveristy metric category.")
    )
    
    # Filter the data based on the studies the user wants to run the model on
    custom_model_data <- data() %>%
      dplyr::filter(IUCN_threat_category_1 %in% input$iucn_threat_category) %>%
      dplyr::filter(Country %in% input$location) %>%
      dplyr::filter(Order %in% input$taxa_order) %>%
      dplyr::filter(Biodiversity_metric %in% input$biodiversity_metric_category)
    
    # filter the data also for the prior meta-analysis
    prior_custom_model_data <- prior_data() %>%
      dplyr::filter(IUCN_threat_category_1 %in% input$iucn_threat_category) %>%
      dplyr::filter(Country %in% input$location) %>%
      dplyr::filter(Order %in% input$taxa_order) %>%
      dplyr::filter(Biodiversity_metric %in% input$biodiversity_metric_category) %>%
      dplyr::filter(Effect_size_type %in% input$Effect_size_type)
    
    # Try to run the model on the currently selected subset of data. If doesn't work, tell user to include more data or view error message.
    base::tryCatch(
      expr = {
        
        if(nrow(custom_model_data) > 0){
        
          # add small value to control and treatment columns
          custom_model_data$Treatment_mean <- custom_model_data$Treatment_mean + 0.1
          custom_model_data$Control_mean <- custom_model_data$Control_mean + 0.1
          
          custom_model_data <- custom_model_data %>%
            filter(Treatment_error >= 0 & Control_error >= 0)
          
          # calculate effect sizes from number, mean, and SD - data needs to be in wide format
          # Adds yi and vi columns to data
          custom_model_data <- metafor::escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
                                               n1i = custom_model_data$Treatment_N,
                                               n2i = custom_model_data$Control_N,
                                               m1i = custom_model_data$Treatment_mean,
                                               m2i = custom_model_data$Control_mean,
                                               sd1i = custom_model_data$Treatment_error,
                                               sd2i = custom_model_data$Control_error,
                                               slab = paste(Paper_ID), # slab adds study labels which will help when we make forest plot
                                               data = custom_model_data)
        }
        
        # remove extra columns from current meta-analyses so will merge on
        custom_model_data_simp <- custom_model_data %>%
          dplyr::select(-Treatment_N, -Control_N, -Treatment_mean, -Control_mean, 
                 -Treatment_error, -Control_error, -Control_error_type, -Treatment_error_type, 
                 -Extracted_from, -URL, -Language, -Database, -Life_history_stage, -Control_quantity, -Control_quantity_unit) %>%
          mutate(Effect_size_type = "LogRR")
        
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
        
        # Run metafor model
        custom_meta_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
                                             random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
                                             data = custom_model_data)
        
        # If model successfully runs, enable the download results buttons
        shinyjs::enable("download_custom_model_output")
        shinyjs::enable("download_custom_model_object")
        shinyjs::enable("download_forest_plot")
        
        ### Assign additional attributes to the model object (so if user downloads the rds model object,
        ### they would be able to see exactly what they did last time, and repeat it).
        ### Access attributes with attributes() function
        
        # Date and time model ran
        base::attr(custom_meta_model, "date_and_time") <- base::Sys.time()
        # Data filters
        base::attr(custom_meta_model, "data_filters_IUCN_threat") <- c("IUCN threat category: ", input$iucn_threat_category)
        base::attr(custom_meta_model, "data_filters_locations") <- c("Location(s): ", input$location)
        base::attr(custom_meta_model, "data_filters_taxonomic_orders") <- c("Taxonomic order(s): ", input$taxa_order)
        base::attr(custom_meta_model, "data_filters_biodiversity_metric") <- c("Biodiversity_metric: ", input$biodiversity_metric_category)
        # Session info
        base::attr(custom_meta_model, "session_info") <- utils::sessionInfo()
        
        
        custom_model <- custom_meta_model
        
      }, error = function(e) {
        
        # If model does not successfully run, make sure download results buttons are disabled
        shinyjs::disable("download_custom_model_output")
        shinyjs::disable("download_custom_model_object")
        shinyjs::disable("download_forest_plot")
        
        # Then stop the process, and return this error message
        base::stop(shiny::safeError(paste0("This model failed to run. This may be due to insufficient data for this model to run, but please see the R error message: ", e)))
      })
    
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  # Custom model summary
  
  custom_model_summary <- shiny::reactive({
    
    shiny::req(custom_model())
    
    custom_model_summary <- utils::capture.output(base::summary(custom_model())) # capture.output allows it to be put into a txt file that the user can download
    
  })
  
  # ---------------------------------------------------------------------------------------------------
  
  ### Plotting custom model graph
  
  # Make plot a reactive object
  figure <- reactive({
    
    shiny::req(custom_model())
    
    figure <- metafor::forest(custom_model(),
                              xlim = c(-12, 8), # horizontal limits of the plot region
                              ilab = base::cbind(Treatment), # add in info on treatment used
                              ilab.xpos = -8, # position treatment labels
                              order = Treatment, # Order results by treatment
                              cex = 1.5,
                              col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                              mlab = "RE Model for All Studies",
                              header = "Author(s) and Year")
    
  })
  
  # Render the plot in Dynameta
  output$custom_model_figure <- shiny::renderPlot({
    
    shiny::req(figure())
    
    custom_model_figure <- figure()
    
  })
  
  # Produce figure legend
  output$custom_model_figure_legend <- shiny::renderText({
    
    shiny::req(custom_model())
    
    # Convert LRR overall effect size to percentage
    percentage_change <- round(100 * (exp(stats::coef(custom_model())) - 1), digits = 2)
    
    # Calculate confidence interval lower bound in percentage
    ci_lb <- round(100 * (exp(custom_model()$ci.lb) - 1), digits = 2)
    
    # Calculate confidence interval upper bound in percentage
    ci_ub <- round(100 * (exp(custom_model()$ci.ub) - 1), digits = 2)
    
    # Calculate I2 statistic. Code adapted from http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate - Multilevel Models section
    W <- diag(1/custom_model()$vi)
    X <- metafor::model.matrix.rma(custom_model())
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    i2 <- round(100 * sum(custom_model()$sigma2) / (sum(custom_model()$sigma2) + (custom_model()$k-custom_model()$p)/sum(diag(P))), digits = 2)
    
    # Add these stats to the paste() below.
    
    paste("<b>Figure 2. </b>", "Forest plot showing the effect sizes for each data point and the overall effect size of ",
          paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on insect biodiversity. The overall effect size is indicated by the diamond -
          the centre of the diamond on the x-axis represents the point estimate,
          with its width representing the 95% confidence interval. The specific ",
          paste(shiny::isolate(input$iucn_threat_category)), " type is listed next to each data point. <br><br>",
          "The overall effect size of ", paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity for ",
          paste(shiny::isolate(input$taxa_order), collapse = ", "), " in ", paste(shiny::isolate(input$location), collapse = ", "), " measured with ",
          paste(shiny::isolate(input$biodiversity_metric_category), collapse = ", "), " as the biodiversity metric is ", round(stats::coef(custom_model()), digits = 2),
          ". This equates to a percentage change of ", percentage_change, "%", " [", ci_lb, "%, ", ci_ub, "%]. <br><br>",
          "The", "<i> I² </i>", "statistic for the meta-analysis is ", i2, "%. This describes the percentage of total variance that is due to heterogeneity (variability among studies), and not due to chance. <br><br>",
          sep = "")
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Downloading the custom R model output, object, and forest plot
  
  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_custom_model_output")
  shinyjs::disable("download_custom_model_object")
  shinyjs::disable("download_forest_plot")
  
  # Disable the download buttons if the iucn_threat_category choice changes
  observeEvent(input$iucn_threat_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the location choice changes
  shiny::observeEvent(input$location, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the taxa_order choice changes
  shiny::observeEvent(input$taxa_order, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the biodiversity_metric_category choice changes
  shiny::observeEvent(input$biodiversity_metric_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Download custom model output (txt file) button
  output$download_custom_model_output <- shiny::downloadHandler(
    filename = function() {
      paste0("custom_model_output", base::Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      utils::write.table(custom_model_summary(), file)
    }
  )
  
  # Download custom model object (rds file) button
  output$download_custom_model_object <- shiny::downloadHandler(
    filename = function() {
      paste0("custom_model_object", base::Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      base::saveRDS(custom_model(), file)
    }
  )
  
  # Download forest plot button
  output$download_forest_plot <- shiny::downloadHandler(
    
    filename = function() {
      paste0("forest_plot", base::Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      grDevices::png(file, width = 1500, height = 1000)
      metafor::forest(custom_model(),
                      xlim = c(-12, 8), # horizontal limits of the plot region
                      ilab = base::cbind(Treatment), # add in info on treatment used
                      ilab.xpos = -8, # position treatment labels
                      order = Treatment, # Order results by treatment
                      cex = 1.5,
                      col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                      mlab = "RE Model for All Studies",
                      header = "Author(s) and Year")
      grDevices::dev.off()
    }
    
  )
  
  # ******* Add code chunk server1 here for subgroup-analysis *****************************************************************************
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### References tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  ### References table
  
  # Add table legend
  output$references_table_legend <- shiny::renderText({
    paste("Table for all data currently in GLiTRS Dynameta. The 'GLiTRS' tab refers to any effect sizes collected under the GLiTRS protocol, and 'Prior' refers to any effect sizes collated from previously published meta-analyses. Please see our publication in Diversity and Distributions, entitled 'A multi-threat meta-analytic database for understanding insect biodiversity change', for more details.")
  })
  
  # Render the references table in the shiny app
  output$references_table <- DT::renderDT({
    
    datatable(data(), editable = FALSE,  selection = "none", options = list(
      scrollX = TRUE, # allow scrolling if too wide to fit all columns on one page
      autoWidth = TRUE, # use smart column width handling
      pageLength = 10, # show 5 entries per page
      
      rownames = FALSE # stops it adding column for row numbers
      
    ))
    
  })
  
  # Render the references table in the shiny app
  output$prior_references_table <- DT::renderDT({
    
    datatable(prior_data(), editable = FALSE,  selection = "none", options = list(
       scrollX = TRUE, # allow scrolling if too wide to fit all columns on one page
       autoWidth = TRUE, # use smart column width handling
       pageLength = 10, # show 5 entries per page
       
       rownames = FALSE # stops it adding column for row numbers
      
    ))
    
  })

  
  # Download full data (.csv) button
  output$download_references_table <- shiny::downloadHandler(
    filename = function() {
      paste0("GLiTRS_table_", base::Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Download full data (.csv) button
  output$download_prior_references_table <- shiny::downloadHandler(
    filename = function() {
      paste0("Prior_table_", base::Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(prior_data(), file, row.names = FALSE)
    }
  )
  
  # =================================================================================================================
  # =================================================================================================================
  
}