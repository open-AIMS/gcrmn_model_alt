aggregate_models <- function() {
  targets <- list(
                                        # Target: Load processing libraries
    tar_target(
      aggregate_libraries_,
      {
        ## ---- aggregate libraries
        library(tidyverse)      # for data manipulation and visualisation
        ## ----end
      }
    ),

    tar_target(aggregate_global_parameters_, {
        ## ---- aggregate global parameters
        assign(x = "data_path", value = "../data/", envir = .GlobalEnv)
        assign(x = "primary_path", value = "../data/primary/", envir = .GlobalEnv)
        assign(x = "output_path", value = "../output/", envir = .GlobalEnv)
        paths <- list(
          data_path = data_path,
          primary_path = primary_path,
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

    ## Retrieve the xgboost predictions (supplied by Jeremy)
    tar_target(xgboost_data_, {
      data_path <- fit_models_global_parameters_$data_path
      ## ---- aggregate xgboost data
      ## data_xgboost <- read_csv(paste0(data_path, "primary/data_xgboost.csv"))
      ## data_xgboost <- get(load("../data/primary/xgboost-trends_2025-09-26.RData"))
      data_xgboost <- get(load("../data/primary/data_xgboost.RData"))
      ## ----end
      data_xgboost
    }),

    ## Aggregate the posteriors for all ecoregions 
    tar_target(compile_posteriors_, {
      data_path <- aggregate_global_parameters_$data_path
      all_years <- get_all_years_
      ## ---- compile posteriors function
      compile_posteriors <- function(data, type = "cellmeans_years") {
        if (type == "cellmeans_years") {
          fl <- data$posteriors_V2
          dat <- map(.x = fl, #.y = wt,
            .f =  ~ {
              dat <- readRDS(.x) |>
                pivot_longer(cols = starts_with("cellmeans_years")) |>
                group_by(.chain, .draw, .iteration) |>
                mutate(Year = all_years) |>
                ungroup() |>
                dplyr::select(-name)
              dat
            })
        } else {
          fl <- data$posteriors
        dat <- map(.x = fl, #.y = wt,
          .f =  ~ {
            dat <- readRDS(.x) |>
              pivot_longer(cols = starts_with("Years")) |>
              group_by(.chain, .draw, .iteration) |>
              mutate(Year = all_years) |>
              ungroup() |>
              dplyr::select(-name)
            dat
          })
        }
        dat[[1]]
      }
      # ----end
      compile_posteriors
    }),
    tar_target(summarise_posteriors_, {
      ## ---- summarise_posteriors function
      summarise_posteriors <- function(.x) {
        .x |>
          group_by(Year) |>
          posterior::summarise_draws(
            median,
            HDInterval::hdi,
            ~ HDInterval::hdi(.x, credMass = c(0.8))
          ) |>
          rename(lower_80 = V4, upper_80 = V5)
      }
      ## ----end
      summarise_posteriors
    }),
    tar_target(aggregate_compile_, {
      ## benthic_models <- fit_models_stan_partial_plot_
      benthic_models <- fit_models_stan_predict_
      benthic_models <- benthic_models$benthic_models
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      compile_posteriors <- compile_posteriors_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_compile
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               region = str_replace(region, "East Asia", "EAS"),
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               subregion = str_replace(subregion, "East Asia", "EAS"),
               ecoregion = ECOREGION
               )

      benthic_posteriors <-
        benthic_models |>
        dplyr::select(region, subregion, ecoregion, category, posteriors, stan_data) |>
        unnest("posteriors") |>
        left_join(wts, by = c("region", "subregion", "ecoregion")) |>
        group_by(region, subregion, ecoregion, category, stan_data) |>
        nest() |>
        ungroup() |> 
        mutate(posteriors = list(
          parallel::mcmapply(FUN = compile_posteriors,
            data, "Years",
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(posteriors = posteriors[[1]]) |>
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, subregion, ecoregion, category)

      ## ----end
      benthic_posteriors
    }
    ),
    tar_target(aggregate_compile_V2_, {
      benthic_models <- fit_models_stan_predict_
      benthic_models <- benthic_models$benthic_models
      ## benthic_models <- fit_models_stan_partial_plot_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      compile_posteriors <- compile_posteriors_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_compile V2
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               region = str_replace(region, "East Asia", "EAS"),
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               subregion = str_replace(subregion, "East Asia", "EAS"),
               ecoregion = ECOREGION
               )

      benthic_posteriors_V2 <-
        benthic_models |>
        dplyr::select(region, subregion, ecoregion, category, posteriors_V2, stan_data) |>
        unnest("posteriors_V2") |>
        left_join(wts, by = c("region", "subregion", "ecoregion")) |>
        group_by(region, subregion, ecoregion, category, stan_data) |>
        nest() |>
        ungroup() |> 
        mutate(posteriors = list(
          parallel::mcmapply(FUN = compile_posteriors,
            data, "cellmeans_years",
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(posteriors = posteriors[[1]]) |>
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, subregion, ecoregion, category)

      ## ----end
      benthic_posteriors_V2
    }
    ),

    ## Aggregate the posteriors for all subregions 
    tar_target(wts_subregions_, {
      benthic_posteriors <- aggregate_compile_
      wts <- process_spatial_weights_
      ## ---- wts_subregions
      stan_model_ecoregions <- unique(benthic_posteriors$ecoregion)
      wts_ecoregions <- unique(wts$ECOREGION)
      wts_nodata <- setdiff(wts_ecoregions, stan_model_ecoregions)
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               region = str_replace(region, "East Asia", "EAS"),
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               subregion = str_replace(subregion, "East Asia", "EAS"),
               ecoregion = ECOREGION
               )
      ## Correct weights (remove ecoregions where there was no benthic data)
      wts <- wts |>
        filter(!(ecoregion %in% wts_nodata)) |>
        droplevels() |>
        group_by(region, subregion) |>
        mutate(
          subregion_area =  sum(area),
          wt =  area / subregion_area) |>
        ungroup()
      wts
      ## ----end
    }
    ),
    tar_target(aggregate_subregions_, {
      benthic_posteriors <- aggregate_compile_
      wts <- wts_subregions_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_subregions
      benthic_posteriors_subregions <-
        benthic_posteriors |>
        dplyr::select(-data) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, ecoregion, wt),
                  by = c("region", "subregion", "ecoregion")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_subregions <-
        data.table::setDT(benthic_posteriors_subregions)[, .(value =  sum(value_wt)),
          by = c("region", "subregion", "category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(region, subregion, category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, subregion, category) 
      
      benthic_posteriors_subregions
      ## ----end
    }
    ),
    tar_target(aggregate_subregions_V2_, {
      benthic_posteriors_V2 <- aggregate_compile_V2_
      wts <- wts_subregions_
      summarise_posteriors <- summarise_posteriors_
      library(data.table)
      ## ---- aggregate_subregions
      benthic_posteriors_subregions_V2 <-
        benthic_posteriors_V2 |>
        dplyr::select(-data) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, ecoregion, wt),
                  by = c("region", "subregion", "ecoregion")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_subregions_V2 <-
        data.table::setDT(benthic_posteriors_subregions_V2)[, .(value =  sum(value_wt)),
          by = c("region", "subregion", "category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(region, subregion, category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, subregion, category)
      
      benthic_posteriors_subregions_V2
      ## ----end
    }
    ),

    ## Aggregate the posteriors for all regions 
    tar_target(wts_regions_, {
      benthic_posteriors_subregions <- aggregate_subregions_
      wts <- process_spatial_weights_
      ## ---- wts_regions
      stan_model_subregions <- unique(benthic_posteriors_subregions$subregion)
      wts_subregions <- unique(wts$GCRMN_subregion)
      wts_nodata <- setdiff(wts_subregions, stan_model_subregions)
      wts <-
        wts |>
        filter(!(GCRMN_subregion %in% wts_nodata)) |>
        droplevels() |>
        mutate(region = GCRMN_region,
               region = str_replace(region, "East Asia", "EAS"),
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               subregion = str_replace(subregion, "East Asia", "EAS"),
               ecoregion = ECOREGION
               ) |>
        dplyr::select(region, subregion, subregion_area) |>
        distinct() |>
        group_by(region) |>
        mutate(wt = subregion_area / sum(subregion_area))
      wts
      ## ----end
    }
    ),
    tar_target(aggregate_regions_, {
      benthic_posteriors_subregions <- aggregate_subregions_
      wts <- wts_regions_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_regions
      benthic_posteriors_regions <-
        benthic_posteriors_subregions |>
        dplyr::select(-cellmeans) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, wt),
                  by = c("region", "subregion")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_regions <-
        data.table::setDT(benthic_posteriors_regions)[, .(value =  sum(value_wt)),
          by = c("region", "category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(region, category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, category)
      
      benthic_posteriors_regions
      ## ----end
    }
    ),
    tar_target(aggregate_regions_V2_, {
      benthic_posteriors_subregions_V2 <- aggregate_subregions_V2_
      wts <- wts_regions_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_regions V2
      benthic_posteriors_regions_V2 <-
        benthic_posteriors_subregions_V2 |>
        dplyr::select(-cellmeans) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, wt),
                  by = c("region", "subregion")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_regions_V2 <-
        data.table::setDT(benthic_posteriors_regions_V2)[, .(value =  sum(value_wt)),
          by = c("region", "category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(region, category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(region, category)
      
      benthic_posteriors_regions_V2
      ## ----end
    }
    ),

    ## Aggregate the posteriors for the globe 
    tar_target(wts_global_, {
      benthic_posteriors_regions <- aggregate_regions_
      wts <- process_spatial_weights_
      ## ---- wts_global
      stan_model_regions <- unique(benthic_posteriors_regions$region)
      wts_regions <- unique(wts$GCRMN_region)
      wts_nodata <- setdiff(wts_regions, stan_model_regions)
      wts <-
        wts |>
        filter(!(GCRMN_region %in% wts_nodata)) |>
        droplevels() |>
        mutate(region = GCRMN_region,
          region = str_replace(region, "East Asia", "EAS"),
          ) |>
        dplyr::select(region, subregion_area) |>
        distinct() |>
        group_by(region) |>
        summarise(region_area = sum(subregion_area)) |>
        mutate(wt = region_area / sum(region_area))
      wts
      ## ----end
    }
    ),
    tar_target(aggregate_global_, {
      benthic_posteriors_regions <- aggregate_regions_
      wts <- wts_global_
      summarise_posteriors <- summarise_posteriors_
      library(data.table)
      ## ---- aggregate_global
      print("Aggregate global")
      benthic_posteriors_global <-
        benthic_posteriors_regions |>
        dplyr::select(-cellmeans) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, wt),
                  by = c("region")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_global <-
        data.table::setDT(benthic_posteriors_global)[, .(value =  sum(value_wt)),
          by = c("category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(category)
      
      benthic_posteriors_global
      ## ----end
    }
    ),
    tar_target(aggregate_global_V2_, {
      benthic_posteriors_regions_V2 <- aggregate_regions_V2_
      wts <- wts_global_
      summarise_posteriors <- summarise_posteriors_
      ## ---- aggregate_global V2
      print("Aggregate global V2")
      benthic_posteriors_global_V2 <-
        benthic_posteriors_regions_V2 |>
        dplyr::select(-cellmeans) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, wt),
                  by = c("region")) |>
        ungroup() |>
        mutate(value_wt = as.double(value * wt)) 
      
      benthic_posteriors_global_V2 <-
        data.table::setDT(benthic_posteriors_global_V2)[, .(value =  sum(value_wt)),
          by = c("category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(category) |>
        nest(.key = "posteriors") |>
        ungroup() |> 
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors,
            posteriors,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(cellmeans = cellmeans[[1]]) |>
        group_by(category)
      
      benthic_posteriors_global_V2
      ## ----end
    }
    ),

    ## Contrasts
    tar_target(contrasts_global_, {
      benthic_posteriors_global <- aggregate_global_V2_
      ## ---- contrasts_global
      yrs <- benthic_posteriors_global$cellmeans[[1]]$Year
      a <- rep(0, len = length(yrs))
      a[which(yrs %in% c(2015, 2017))] <- c(1, -1)
      a

      
      contrasts_global <- benthic_posteriors_global |>
        mutate(contrast = map(.x = posteriors,
          .f = ~ {
            .x |> filter(Year %in% c(2015, 2017)) |>
              droplevels() |>
              group_by(.draw) |>
              summarise(value = diff(value * 100)) |>
              mutate(contrast = "2014 vs 2016")
          })) |>
        mutate(contrast_sum = map(.x = contrast,
          .f =  ~ {
           .x |>
             group_by(contrast) |> 
             posterior::summarise_draws(
               median,
               HDInterval::hdi,
               ~ HDInterval::hdi(.x, credMass = c(0.8)),
               Pl = ~ mean(.x < 0),
               Pg = ~ mean(.x > 0)
             ) |>
             rename(lower_80 = V4, upper_80 = V5)
          }
          ))
      ## ----end
    }
    )
    

  )
}

