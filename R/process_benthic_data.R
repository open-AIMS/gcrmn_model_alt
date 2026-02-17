
process_benthic_data <- function() {
  targets <- list(
                                        # Target: Load processing libraries
    tar_target(
      processing_libraries_,
      {
        ## ---- spatial libraries
        library(tidyverse)      # for data manipulation and visualisation
        library(sf)             # for spatial data handling and visualisation
        library(lwgeom)         # for spatial data handling
        ## ----end
      }
    ),
    
    tar_target(
      processing_global_parameters_,
      {
        ## ---- processing global parameters
        assign(x = "data_path", value = "../data/", envir = .GlobalEnv)
        assign(x = "primary_path", value = "../data/primary/", envir = .GlobalEnv)
        assign(x = "gis_path", value = "../data/primary/GIS/", envir = .GlobalEnv)
        assign(x = "output_path", value = "../output/", envir = .GlobalEnv)
        paths <- list(
          data_path = data_path,
          primary_path = primary_path,
          gis_path = gis_path,
          output_path = output_path,
          fig_path = paste0(output_path, "figures/")
        )
        lapply(paths, function(x) {
          if (!dir.exists(x)) {
            dir.create(x)
          }
        })
        ## ----end
        paths
      }
    ),
    
    ## Import data ====================================================
    ## Benthic data ---------------------------------------------------
    tar_target(benthic_data_file_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- benthic data file
      benthic_data_file <- paste0(
        primary_path,
        "data_benthic_prepared.RData"
      )
      ## ----end
      benthic_data_file
    },
    format = "file"
    ),
    tar_target(read_benthic_data_, {
      primary_path <- spatial_global_parameters_$primary_path
      benthic_data_file <- benthic_data_file_
      ## ---- read benthic data
      benthic_data <- get(load(file = benthic_data_file)) 
      ## ----end
      benthic_data
    }),

    ## Benthic prediction data ----------------------------------------
    ## tar_target(read_benthic_prediction_data_, {
    ##   primary_path <- spatial_global_parameters_$primary_path
    ##   ## ---- read benthic prediction data
    ##   benthic_pred <- get(load(file = paste0(
    ##     primary_path,
    ##     "data_predictors_pred_murray.RData"
    ##   )))
    ##   ## ----end
    ##   benthic_pred
    ## }),

    ## Process data ===================================================
    ## Process benthic data (adjust PERSGA) ---------------------------
    ## THIS IS NO LONGER REQUIRED
    tar_target(process_benthic_data_adjust_PERSGA_, {
      benthic_data <- read_benthic_data_
      ## ---- process benthic data adjust PERSGA 
      benthic_data <- benthic_data |> 
        mutate(ecoregion = ifelse(subregion == "PERSGA 1", "Northern Red Sea",
          ifelse(subregion == "PERSGA 2", "Central Red Sea",
            as.character(ecoregion)))) |>
        mutate(ecoregion = factor(ecoregion))
      ## ----end
      benthic_data
    }),

    ## Process benthic data (adjust GBR) ------------------------------
    tar_target(process_benthic_data_adjust_GBR_, {
      ## benthic_data <- process_benthic_data_adjust_PERSGA_
      benthic_data <- read_benthic_data_
      ## ---- process benthic data adjust GBR 
      benthic_data <-
        benthic_data |>
        filter(!(ecoregion == "Central and Southern Great Barrier Reef" & year < 1993)) |>
        filter(!(ecoregion == "Houtman" & datasetID == "d4992")) |>  ## for testing purposes
        filter(!(ecoregion == "West Caroline Islands" & datasetID == "d4992" & year < 2000)) |>  ## for testing purposes
        filter(!(ecoregion == "Vanuatu" & datasetID == "d4992" & year < 2005)) |>  ## for testing purposes
        filter(!(ecoregion == "Gilbert/Ellis Islands" & datasetID == "d4992" & year < 2012)) |>  ## for testing purposes
        filter(!(ecoregion == "Samoa Islands" & datasetID == "d4992" & year < 2000)) |>  ## for testing purposes
        droplevels()
      ## ----end
      benthic_data
    }),

    ## Process benthic data (aggregate transect) ------------------------------
    tar_target(process_benthic_data_aggregate_transect_, {
      benthic_data <- process_benthic_data_adjust_GBR_
      ## ---- process benthic data aggregate transect 
      benthic_data <- 
        benthic_data |> 
        group_by(region, subregion, ecoregion, 
          decimalLatitude, decimalLongitude, 
          parentEventID, datasetID, year, category) |>
        summarise(measurementValue = mean(measurementValue, na.rm = TRUE),
          .groups = "keep") 
      ## ----end
      benthic_data
    }),

    ## Process benthic data (fill in missing zeros) ------------------
    tar_target(process_benthic_data_fill_zeros_, {
      benthic_data <- process_benthic_data_aggregate_transect_
      ## ---- process benthic data fill zeros
      ## Determine which datasetID are ineligable for zero replacement
      ## - those datasetsID's that never observed the category
       ineligable_datasets <- 
        benthic_data |>
        ungroup() |> 
        group_by(datasetID) |>
        complete(category, fill = list(value = NA)) |> 
        group_by(category, .add = TRUE) |> 
        summarise(value = sum(measurementValue), .groups = "keep") |> 
        filter(is.na(value)) |>
        dplyr::select(-value)

      ## Fill all missing zeros
      filled <-
        benthic_data |> 
        pivot_wider(names_from = "category",
          values_from = "measurementValue",
          values_fill = 0) |>
        pivot_longer(cols = c("Algae", "Macroalgae", `Hard coral`,
          `Other fauna`, `Coralline algae`, `Turf algae`),
          names_to = "category", values_to = "measurementValue") |>
        ungroup()

      ## Remove the ineligable cases
      benthic_data <- filled |>
        anti_join(ineligable_datasets, by = join_by(datasetID, category))
      ## ----end
      benthic_data
    }),

    ## Process benthic data (Part 1. add ecoregions) -------------------
    tar_target(process_benthic_data_1_, {
      benthic_data <- process_benthic_data_fill_zeros_
      ecoregions <- process_ecoregions_
      grid_reef <- process_grid_reef_
      ## ---- process benthic data Part 1. add ecoregions
      sf_use_s2(FALSE)
      benthic_data <-
        benthic_data |>
        st_as_sf(coords =  c("decimalLongitude", "decimalLatitude"),
                 crs =  st_crs(grid_reef),
                 remove =  FALSE) |>
        st_intersection(ecoregions)
        sf_use_s2(TRUE)
      ## ----end
      benthic_data
    }),

    ## Process benthic data (Part 2. add grid data) -------------------
    tar_target(process_benthic_data_2_, {
      benthic_data <- process_benthic_data_1_     
      grid_reef <- process_grid_reef_
      ## ---- process benthic data Part 2. add grid data
      sf_use_s2(FALSE)
      benthic_data <- benthic_data |>
        st_join(grid_reef, join =  st_nearest_feature)
      sf_use_s2(TRUE)
      ## ----end
      benthic_data
    }),
    
    ## Process benthic data (Part 3. add factors) ---------------------
    tar_target(process_benthic_data_3_, {
      benthic_data <- process_benthic_data_2_     
      ## ---- process benthic data Part 3. add factors
      benthic_data <- benthic_data |>
        st_drop_geometry() |> 
        mutate(
          grid_id = factor(grid_id),
          ## Site =  factor(paste0(decimalLatitude, decimalLongitude, sep =  "_")),
          Site =  factor(paste0(
            round(decimalLatitude, 3),
            round(decimalLongitude, 3),
            sep =  "_")),
          Transect =  factor(paste0(Site, parentEventID, sep =  "_")),
          datasetID =  factor(datasetID),
          fyear =  factor(year),
          value =  measurementValue / 100,
          value_trim = case_when(
            value < 0.01 ~ 0.01,
            value > 0.99 ~ 0.99,
            TRUE ~ value
          )
        )
      ## ----end
      benthic_data
    })
    )
}
