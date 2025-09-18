
process_spatial <- function() {
  targets <- list(
    # Target: Load spatial libraries
    tar_target(
      spatial_libraries_,
      {
        ## ---- spatial libraries
        library(tidyverse)      # for data manipulation and visualisation
        library(sf)             # for spatial data handling and visualisation
        library(lwgeom)         # for spatial data handling
        library(rnaturalearth)  #for world mapping layers
        ## ----end
      }
    ),
    tar_target(
      spatial_global_parameters_,
      {
        ## ---- spatial global parameters
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
    ## World map layers -----------------------------------------------
    tar_target(
      ne_countries_,
      {
        ## ---- ne countries
        world_sf <- rnaturalearth::ne_countries(scale =  "small", returnclass =  "sf")
        ## ----end
      }
    ),
    ## Reef grid ------------------------------------------------------
    tar_target(grid_, {
      gis_path <- spatial_global_parameters_$gis_path
      ## ---- reef grid
      grid <- read_sf(paste0(gis_path, "reef_grid.shp"))
      ## ----end
      grid
    }),
    ## MEOWs (ecoregions) ---------------------------------------------
    tar_target(meow_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- ecoregions
      load(paste0(primary_path, "meow.RData"))
      ## ----end
      meow
    }),
    ## Ecoregions lookups ---------------------------------------------
    tar_target(ecoregion_lookup_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- ecoregion lookup
      load(paste0(primary_path, "ecoregion.lookup.RData"))
      ## ----end
      ecoregion.lookup
    }),

    ## Process data ===================================================
    ## Reef grid ------------------------------------------------------
    tar_target(process_grid_reef_, {
      grid <- grid_
      ## reduce grid to reef
      ## ---- process grid reef
      grid_reef <- grid |>
        mutate(grid_id =  seq_len(n())) |>
        filter(sum > 0)
      ## ----end
      grid_reef
    }),
    ## Join meows to the ecoregion lookup -----------------------------
    tar_target(process_ecoregions_, {
      grid_reef <- process_grid_reef_
      meow <- meow_
      ecoregion_lookup <- ecoregion_lookup_
      ## ---- join meow and ecoregion_lookup
      ecoregions <- meow |>
        st_transform(crs = st_crs(grid_reef)) |>
        full_join(ecoregion_lookup)
      ## ----end
      ecoregions 
    }),

    ## Process spatial weights ----------------------------------------
    tar_target(process_spatial_weights_, {
      grid_reef <- process_grid_reef_
      ecoregions <- process_ecoregions_
      ## ---- process patial weights
      sf_use_s2(FALSE)
      grid_wts <- grid_reef |>
        st_intersection(ecoregions)

      wts <- grid_wts |>
        st_drop_geometry() |>
        group_by(GCRMN_region, GCRMN_subregion, ECOREGION) |>
        summarise(area =  sum(sum)) |>
        ungroup() |>
        group_by(GCRMN_region, GCRMN_subregion) |>
        mutate(
          subregion_area =  sum(area),
          wt =  area / subregion_area
        ) |>
        ungroup()
      sf_use_s2(TRUE)
      ## ----end
      wts
    })
  )
  return(targets)
}
