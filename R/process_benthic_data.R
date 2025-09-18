
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
    tar_target(read_benthic_data_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- read benthic data
      benthic_data <- get(load(file = paste0(
        primary_path,
        "data_benthic_prepared_murray.RData"
      )))
      ## ----end
      benthic_data
    }),
    ## Benthic prediction data ----------------------------------------
    tar_target(read_benthic_prediction_data_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- read benthic prediction data
      benthic_pred <- get(load(file = paste0(
        primary_path,
        "data_predictors_pred_murray.RData"
      )))
      ## ----end
      benthic_pred
    }),

    ## Process data ===================================================
    ## Process benthic data (Part 1. add ecoregions) ------------------
    tar_target(process_benthic_data_1_, {
      benthic_data <- read_benthic_data_
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
          Site =  factor(paste0(decimalLatitude, decimalLongitude, sep =  "_")),
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
