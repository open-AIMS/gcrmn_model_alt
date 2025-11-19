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
    ## GCRMN subregions object (from Jeremy) --------------------------
    tar_target(gcrmn_subregions_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- gcrmn subregions
      gcrmn_subregions <- read_sf(paste0(primary_path, "gcrmn_subregions/",
        "gcrmn_subregions.shp"))
      ## ----end
      gcrmn_subregions
    }),
    ## Global 2024 reefs (from Jeremy) --------------------------------
    tar_target(global_2024_reefs_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- global 2024 reefs
      global_2024_reefs <- read_sf(paste0(primary_path, "coral_reef_distribution/",
        "global_2024_reefs.shp"))
      ## ----end
      global_2024_reefs
    }),
    ## Make a units=m version -----------------------------------------
    tar_target(global_2024_reefs_m_, {
      global_2024_reefs <- global_2024_reefs_
      ## ---- global 2024 reefs m
      crs_string <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
      global_2024_reefs_m <- global_2024_reefs |>
        st_transform(crs_string)
      ## ----end
      global_2024_reefs_m
    }),
    ## Make grid ------------------------------------------------------
    tar_target(make_grid_, {
      global_2024_reefs_m <- global_2024_reefs_m_
      ## ---- make grid
      grid.10 <- global_2024_reefs_m |>
        st_make_grid(cellsize = 10000) |>
        (\(x) st_sf(x, grid_id = seq_len(length(x))))()
      ## ----end
      grid.10
    }),
    ## Make reef grid ------------------------------------------------------
    tar_target(make_reef_grid_, {
      grid.10 <- make_grid_
      global_2024_reefs_m <- global_2024_reefs_m_
      ## ---- make reef grid
      sf_use_s2(FALSE)
      reefs_grid_10 <- st_intersection(global_2024_reefs_m, grid.10)
      reefs_grid_10 <- reefs_grid_10 |>
        (\(x) mutate(x, Area = st_area(x)))() |>
        group_by(grid_id) |>
        summarise(Area = sum(Area))
      sf_use_s2(TRUE)
      ## ----end
      reefs_grid_10
    }),
    ## grid_10 ------------------------------------------------------
    tar_target(grid_10_, {
      grid.10 <- make_grid_
      reefs_grid_10 <- make_reef_grid_
      ## ---- grid_10
      grid_10 <- grid.10 |>
        left_join(reefs_grid_10 |>
                    st_drop_geometry())
      ## ----end
      grid_10
    }),

    ## ## Reef grid ------------------------------------------------------
    ## tar_target(grid_, {
    ##   gis_path <- spatial_global_parameters_$gis_path
    ##   ## ---- reef grid
    ##   grid <- read_sf(paste0(gis_path, "reef_grid.shp"))
    ##   ## ----end
    ##   grid
    ## }),
    ## MEOWs (ecoregions) ---------------------------------------------
    tar_target(meow_, {
      primary_path <- spatial_global_parameters_$primary_path
      ## ---- ecoregions
      load(paste0(primary_path, "meow.RData"))
      ## ----end
      meow
    }),
    ## ## Ecoregions lookups ---------------------------------------------
    ## tar_target(ecoregion_lookup_, {
    ##   primary_path <- spatial_global_parameters_$primary_path
    ##   ## ---- ecoregion lookup
    ##   load(paste0(primary_path, "ecoregion.lookup.RData"))
    ##   ## ----end
    ##   ecoregion.lookup
    ## }),
    ## Ecoregions lookups ---------------------------------------------
    tar_target(ecoregion_lookup_, {
      meow <- meow_
      gcrmn_subregions <- gcrmn_subregions_
      ## ---- ecoregion lookup
      meow <- meow |>
        dplyr::rename(ecoregion = ECOREGION)
      sf_use_s2(FALSE)
      ecoregion_lookup <-
        meow  |>
        st_buffer(dist = -0.01) |>
        st_intersection(gcrmn_subregions) |>
        filter(!is.na(subregion)) |>
        st_drop_geometry() |>
        dplyr::select(region, subregion, ecoregion) |>
        distinct() |>
        arrange(region, subregion, ecoregion)
      sf_use_s2(TRUE)
      ## ----end
      ecoregion_lookup
    }),
    ## Process data ===================================================
    ## Reef grid ------------------------------------------------------
    tar_target(process_grid_reef_, {
      reefs_grid_10 <- make_reef_grid_
      gcrmn_subregions <- gcrmn_subregions_
      ## ---- process grid reef
      grid_reef <- reefs_grid_10 |>
        st_transform(crs =  st_crs(gcrmn_subregions))
      ## grid_reef <- grid |>
      ##   mutate(grid_id =  seq_len(n())) |>
      ##   filter(sum > 0)
      ## ----end
      grid_reef
    }),
    ## Join meows to the ecoregion lookup -----------------------------
    tar_target(process_ecoregions_, {
      ## grid_reef <- process_grid_reef_
      meow <- meow_
      meow <- meow |>
        dplyr::rename(ecoregion = ECOREGION)
      ecoregion_lookup <- ecoregion_lookup_
      ## ---- join meow and ecoregion_lookup
      ecoregions <- meow |>
        ## st_transform(crs = st_crs(grid_reef)) |>
        full_join(ecoregion_lookup)
      ## ----end
      ecoregions
    }),

    ## Process spatial weights ----------------------------------------
    tar_target(process_spatial_weights_, {
      grid_reef <-  make_reef_grid_
      ecoregions <- process_ecoregions_
      ## ---- process patial weights
      sf_use_s2(FALSE)
      grid_wts <- grid_reef |>
        st_transform(crs = st_crs(ecoregions)) |>
        st_intersection(ecoregions)
      wts <- grid_wts |>
        dplyr::rename(GCRMN_region = region, GCRMN_subregion = subregion, ECOREGION = ecoregion) |>
        st_drop_geometry() |>
        group_by(GCRMN_region, GCRMN_subregion, ECOREGION) |>
        summarise(area =  sum(Area)) |>
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
