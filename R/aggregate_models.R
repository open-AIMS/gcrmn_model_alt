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
    
    tar_target(
      aggregate_global_parameters_,
      {
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

    
    tar_target(aggregate_compile_, {
      benthic_models <- fit_models_stan_partial_plot_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      ## ---- aggregate_compile
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               ecoregion = ECOREGION
               ) 
      benthic_posteriors <-
        benthic_models |>
        ungroup() |>
        dplyr::select(region, subregion, ecoregion, category, posteriors) |>
        unnest("posteriors") |>
        left_join(wts, by = c("region", "subregion", "ecoregion")) |>
        group_by(region, subregion, ecoregion, category) |>
        nest() |>
        mutate(posteriors = pmap(.l = list(data, subregion),
                                .f = ~ {
                                  fl <- ..1$posteriors 
                                  ## wt <- ..1$wt
                                  dat <- map(.x = fl, #.y = wt,
                                              .f =  ~ {
                                                dat <- readRDS(.x) |>
                                                  pivot_longer(cols = starts_with("Years")) |>
                                                  group_by(.chain, .draw, .iteration) |>
                                                  mutate(Year = all_years) |>
                                                  ungroup() |>
                                                  dplyr::select(-name)
                                                  ## mutate(wt = .y,
                                                  ##        value_wt = value * wt
                                                  ##        )
                                                dat
                                              })
                                  dat[[1]]
                                }
                                )) |> 
        mutate(summ = map(.x = posteriors,
                          .f =  ~ {
                            .x |>
                              group_by(Year) |> 
                              posterior::summarise_draws(
                                           median,
                                           HDInterval::hdi,
                                           ~ HDInterval::hdi(.x, credMass = c(0.8))
                                         ) |>
                              rename(lower_80 = V4, upper_80 = V5) 
                          }
                          ))
      ## ----end
      benthic_posteriors
    }
    ),

    tar_target(aggregate_ecoregion_plots_, {
      benthic_posteriors <- aggregate_compile_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      benthic_models <- fit_models_stan_predict_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## ---- aggregate_ecoregion_plots
      benthic_posteriors_ecoregions <- 
        benthic_posteriors |>
        rename(other_data =  data) |> 
        left_join(benthic_models |>
                  dplyr::select(region, subregion, ecoregion, category,
                                data, name, stan_data),
                  by = c("region", "subregion", "ecoregion", "category")) |> 
        mutate(plot =
                 pmap(.l = list(summ, data, stan_data, name),
                      .f = ~ {
                        cellmeans <- ..1
                        dat <- ..2
                        stan_data <- ..3
                        title <- str_replace_all(..4, "_", " ")
                        ytitle <- str_replace(..4, ".*_(.*)", "\\1")
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        g2 <- stan_partial_plot(cellmeans, stan_data,
                                                data = dat,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = TRUE)

                        nm <- paste0(output_path, "figures/ecoregion_pdp_", "_", ..4, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        nm2 <- paste0(output_path, "figures/ecoregion_pdpraw_", "_", ..4, ".png")
                        ggsave(
                          filename = nm2,
                          g2,
                          width =  6, height =  4, dpi =  72
                        )
                        nm
                      }
                      ))
      #benthic_posteriors_ecoregions # ----end
      benthic_posteriors_ecoregions 
    }
    ),

    tar_target(aggregate_subregions_, {
      benthic_posteriors <- aggregate_compile_
      wts <- process_spatial_weights_
      ## ---- aggregate_subregions
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               ecoregion = ECOREGION
               ) 
      benthic_posteriors_subregions <-
        benthic_posteriors |>
        dplyr::select(-data) |> 
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, ecoregion, wt),
                  by = c("region", "subregion", "ecoregion")) |> 
        ungroup(ecoregion) |>
        group_by(.draw, Year, .add = TRUE) |>
        mutate(value_wt = value * wt) |>
        summarise(value = sum(value * wt, na.rm = TRUE)) |>
        ungroup() |>
        group_by(region, subregion, category) |>
        nest(.key = "posteriors") |>
        mutate(summ = map(.x = posteriors,
                          .f =  ~ {
                            .x |>
                              group_by(Year) |> 
                              posterior::summarise_draws(
                                           median,
                                           HDInterval::hdi,
                                           ~ HDInterval::hdi(.x, credMass = c(0.8))
                                         ) |>
                              rename(lower_80 = V4, upper_80 = V5) 
                            }
                          ))
      benthic_posteriors_subregions
      ## ----end
    }
    ),

    tar_target(aggregate_subregion_plots_, {
      benthic_posteriors_subregions <- aggregate_subregions_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      benthic_models <- fit_models_stan_predict_
      ## ---- aggregate_subregion_plots
      ## combine the stan_data
      stan_data  <- 
        benthic_models |>
        ungroup(ecoregion) |>
        dplyr::select(region, subregion, category, stan_data) |>
        ## filter(subregion %in% c("Caribbean 1", "Caribbean 2")) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
        
      benthic_posteriors_subregions <- 
        benthic_posteriors_subregions |>
        left_join(stan_data, by = c("region", "subregion", "category")) |>
        mutate(name = paste(subregion, category, sep = "_")) |> 
        mutate(plot =
                 pmap(.l = list(summ, stan_data, name),
                      .f = ~ {
                        cellmeans <- ..1
                        stan_data <- ..2
                        title <- str_replace_all(..3, "_", " ")
                        ytitle <- str_replace(..3, ".*_(.*)", "\\1")
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        nm <- paste0(output_path, "figures/subregion_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        nm
                      }
                      ))
      # ----end
      benthic_posteriors_subregions 
    }
    ),

    tar_target(aggregate_regions_, {
      benthic_posteriors_subregions <- aggregate_subregions_
      wts <- process_spatial_weights_
      ## ---- aggregate_regions
      wts <-
        wts |>
        mutate(region = GCRMN_region,
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               ecoregion = ECOREGION
               ) |>
        dplyr::select(region, subregion, subregion_area) |>
        distinct() |> 
        group_by(region) |>
        mutate(wt = subregion_area / sum(subregion_area))
      benthic_posteriors_regions <-
        benthic_posteriors_subregions |>
        dplyr::select(-summ) |> 
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, wt),
                  by = c("region", "subregion")) |> 
        ungroup(subregion) |>
        group_by(.draw, Year, .add = TRUE) |>
        mutate(value_wt = value * wt) |>
        summarise(value = sum(value * wt, na.rm = TRUE)) |>
        ungroup() |>
        group_by(region, category) |>
        nest(.key = "posteriors") |>
        mutate(summ = map(.x = posteriors,
                          .f =  ~ {
                            .x |>
                              group_by(Year) |> 
                              posterior::summarise_draws(
                                           median,
                                           HDInterval::hdi,
                                           ~ HDInterval::hdi(.x, credMass = c(0.8))
                                         ) |>
                              rename(lower_80 = V4, upper_80 = V5) 
                            }
                          ))
      benthic_posteriors_regions
      ## ----end
    }
    ),

    tar_target(aggregate_region_plots_, {
      benthic_posteriors_regions <- aggregate_regions_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      benthic_models <- fit_models_stan_predict_
      ## ---- aggregate_region_plots
      ## combine the stan_data
      stan_data  <- 
        benthic_models |>
        ungroup(subregion, ecoregion) |>
        dplyr::select(region, category, stan_data) |>
        ## filter(subregion %in% c("Caribbean 1", "Caribbean 2")) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
        
      benthic_posteriors_regions <- 
        benthic_posteriors_regions |>
        left_join(stan_data, by = c("region", "category")) |>
        mutate(name = paste(region, category, sep = "_")) |> 
        mutate(plot =
                 pmap(.l = list(summ, stan_data, name),
                      .f = ~ {
                        cellmeans <- ..1
                        stan_data <- ..2
                        title <- str_replace_all(..3, "_", " ")
                        ytitle <- str_replace(..3, ".*_(.*)", "\\1")
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        nm <- paste0(output_path, "figures/region_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        nm
                      }
                      ))
      # ----end
      benthic_posteriors_regions 
    }
    )
    
  )
}
