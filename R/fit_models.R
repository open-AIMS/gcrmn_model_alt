fit_models <- function() {
  targets <- list(
                                        # Target: Load processing libraries
    tar_target(
      fit_models_libraries_,
      {
        ## ---- fit models libraries
        library(tidyverse)      # for data manipulation and visualisation
        library(furrr)
        ## ----end
      }
    ),

    tar_target(
      fit_models_global_parameters_,
      {
        ## ---- fit models global parameters
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

    ## Get a list of the ecoregions
    tar_target(get_ecoregions_, {
      benthic_data <- process_benthic_data_3_
      ## ---- get ecoregions
      ecoregions <- benthic_data |>
        pull(ecoregion) |>
        unique()
      ## ----end
      ecoregions
    }
    ),

    ## Get all years
    tar_target(get_all_years_, {
      benthic_data <- process_benthic_data_3_
      ## ---- get all years
      all_years <- benthic_data |>
        tidyr::expand(year = tidyr::full_seq(year, period = 1)) |>
        pull(year)
      ## ----end
      all_years
    }
    ),
    ## Set up the nested data for modelling
    tar_target(fit_models_nest_, {
      benthic_data <- process_benthic_data_3_
      ecoregions <- get_ecoregions_
      ## ---- fit models nest
      benthic_models <- benthic_data |>
        group_by(region, subregion, ecoregion, category) |>
        nest() |>
        mutate(name = paste(ecoregion, category, sep = "_"),
               name = str_replace_all(name, "/", " and ")
               )
      ## ----end
      benthic_models
    }
    ),

    ## Refactor subsets
    tar_target(fit_models_refactor_, {
      benthic_models <- fit_models_nest_
      ## ---- fit models refactor
      benthic_models <- benthic_models |>
        mutate(data =
                 map(.x = data,
                     .f = ~ {
                       .x |>
                         droplevels() |>
                         mutate(cYear = factor(year),
                                Year = year,
                                Cover = value_trim,
                                grid_id = factor(grid_id),
                                cSite = factor(Site),
                                cReplicate = factor(Transect),
                                datasetID = factor(datasetID),
                                area = 1,
                                sum = 1
                                )
                     }
                     ))
      ## ----end
      benthic_models
    }
    ),

    ## Prepare data for STAN
    tar_target(fit_models_prepare_stan_data_, {
      benthic_models <- fit_models_refactor_
      all_years <- get_all_years_
      prepare_data_for_stan <- prepare_data_for_stan_
      ## ---- fit models prepare for stan
      benthic_models <- benthic_models |>
        mutate(stan_data =
                 map(.x = data,
                     .f =  ~ {
                       .x |> prepare_data_for_stan(yrs = all_years)
                     }
                     ))
      ## ----end
      benthic_models
    }
    ),

    ## Order nested tibble according to most observations
    tar_target(fit_models_reorder_, {
      benthic_models <- fit_models_prepare_stan_data_
      ## ---- fit models refactor
      benthic_models <- benthic_models |>
        mutate(n =
                 map(.x = data,
                     .f = ~ {
                       .x |> nrow()
                     }
                 )) |>
        unnest(n) |>
        ungroup() |>
        arrange(desc(n), region, subregion, ecoregion) |>
        mutate(region = factor(region, levels = unique(region)),
          subregion = factor(subregion, levels = unique(subregion)),
          ecoregion = factor(ecoregion, levels =  unique(ecoregion)),
          category = factor(category, levels = unique(category))
        ) |>
        group_by(region, subregion, ecoregion, category)
      ## ----end
      benthic_models
    }
    ),
    ## Fit the STAN model
    ## At this point, I would like to have a loop that can farm off
    ## to the HPC.
    ## For the moment, I will just do a single ecoregion (Eastern Caribbean)

    tar_target(fit_models_stan_, {
      benthic_models <- fit_models_reorder_
      data_path <- fit_models_global_parameters_$data_path
      ## ---- fit models stan
      print(unique(benthic_models$region))
      ## model_stan <- cmdstanr::cmdstan_model(stan_file =  "model2.stan")
      model_stan <- cmdstanr::cmdstan_model(stan_file =  "gcrmn_model_43.stan")
      ## benthic_models <- benthic_models |>
        ## filter(ecoregion == "Fiji Islands", category == "Hard coral") |>
        ## filter(ecoregion == "Eastern Caribbean", category == "Hard coral") |>
        ## filter(category %in% c("Hard coral", "Macroalgae"))

      options(future.globals.maxSize = 2 * 1024^3)
      old_plan <- future::plan(future::multisession, workers = 15)
      results <- furrr::future_map2(.x = benthic_models$stan_data,
        .y = benthic_models$name,
        .f = ~ {
          name <- .y
          nm <- paste0(data_path, "mod_", name, ".rds")
          if (file.exists(nm)) {
            cat(paste("\n", "Skipping", name, "as it already exists\n"))
            return(nm)
          }
          cat(paste("\n", "Running", name, "\n"))
          mod_stan <-
            model_stan$sample(
              data =  .x,
              seed =  123,
              iter_sampling =  5000,
              iter_warmup =  1000,
              ## init = init,
              thin =  5,
              chains =  3,
              parallel_chains =  3,
              adapt_delta =  0.99,
              output_dir =  data_path,
              )
          saveRDS(mod_stan,
            file = nm
          )
          nm
        },
        .progress = TRUE
      )
      future::plan(old_plan)
      benthic_models <- benthic_models |>
        ungroup() |>                                      ## need to ungroup for the following
        mutate(mod = results) |>
        group_by(region, subregion, ecoregion, category)  ## put the groups back


     ##    filter(category == "Hard coral") |>
     ##    ##filter(region=="WIO") |>
     ##    ## unfortunately, future_map2, does not work with grouped data
     ##    ## so I will have to make name a list column and ungroup the data
     ##    ## nest(name=name) |> ungroup() |>
     ##    mutate(mod =
     ##             ##furrr::future_map2(.x = stan_data,
     ## map2(.x=stan_data,
     ##                  .y = name,
     ##                  .f = ~ {
     ##                    nm <- paste0(data_path, "mod_", name, ".rds")
     ##                    if (file.exists(nm)) {
     ##                      cat(paste("\n", "Skipping", name, "as it already exists\n"))
     ##                      return(nm)
     ##                    }
     ##                    mod_stan <-
     ##                      model_stan$sample(
     ##                                   data =  .x,
     ##                                   seed =  123,
     ##                                   iter_sampling =  5000,
     ##                                   iter_warmup =  1000,
     ##                                   ## init = init,
     ##                                   thin =  5,
     ##                                   chains =  3,
     ##                                   parallel_chains =  3,
     ##                                   adapt_delta =  0.99,
     ##                                   output_dir =  data_path,
     ##                                   )
     ##                    saveRDS(mod_stan,
     ##                            file = nm
     ##                            )
     ##                    nm
     ##                  }#,
     ##        #.progress=TRUE
     ##                  ))
      ## ----end
      benthic_models
    }
    ),

    ## ## Stan posterior predictions
    tar_target(fit_models_stan_posterior_predict_, {
      benthic_models <- fit_models_stan_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      all_years <- get_all_years_
      print(paste0("The Number of available cores:", parallelly::availableCores()))
      ## ---- fit models stan posterior predictions

      ## options(future.globals.maxSize = 2 * 1024^3)
      ## old_plan <- future::plan(future::multisession, workers = 15)
      ## posteriors <- furrr::future_map(
      ##   .x = benthic_models$name,
      ##   .f =  ~ {
      ##     mod <- readRDS(paste0(data_path, "mod_", .x, ".rds"))
      ##     vars <- get_variables(mod)
      ##     posteriors <-
      ##       mod$draws(variables = "Years") |>
      ##       posterior::as_draws_df() |>
      ##       mutate(across(starts_with("Years"), plogis))
      ##     nm <- paste0(data_path, "posteriors_", .x, ".rds")
      ##     saveRDS(posteriors,
      ##       file = nm
      ##     )
      ##     nm
      ##   },
      ##   .progress = TRUE
      ## )
      ## future::plan(old_plan)
      ## gc()
      ## benthic_models <- benthic_models |>
      ##   ungroup() |>                                      ## need to ungroup for the following
      ##   mutate(posteriors = posteriors) |>
      ##   group_by(region, subregion, ecoregion, category)  ## put the groups back
      
      ## benthic_models <- benthic_models |>
      ##   mutate(posteriors = map2(.x = mod,
      ##                           .y = name,
      ##                           .f =  ~ {
      ##                             mod <- readRDS(paste0(data_path, "mod_", .y, ".rds"))
      ##                             vars <- get_variables(mod)
      ##                             posteriors <-
      ##                               mod$draws(variables = "Years") |>
      ##                               posterior::as_draws_df() |>
      ##                               mutate(across(starts_with("Years"), plogis))
      ##                             nm <- paste0(data_path, "posteriors_", .y, ".rds")
      ##                             saveRDS(posteriors,
      ##                                     file = nm
      ##                                     )
      ##                             nm
      ##                           }
      ##                           ))

      get_posteriors <- function(.x, .y) {
        mod <- readRDS(paste0(data_path, "mod_", .y, ".rds"))
        vars <- get_variables(mod)
        posteriors <-
          mod$draws(variables = "Years") |>
          posterior::as_draws_df() |>
          mutate(across(starts_with("Years"), plogis))
        nm <- paste0(data_path, "posteriors_", .y, ".rds")
        saveRDS(posteriors,
          file = nm
        )
        nm
      }

      benthic_models <- benthic_models |>
        ungroup() |> 
        mutate(posteriors = list(parallel::mcmapply(FUN = get_posteriors,
          mod, name, mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE))) |>
        mutate(posteriors = posteriors[[1]]) |>
        group_by(region, subregion, ecoregion, category)
      ## ----end
      benthic_models
    }
    ),

    ## ## Stan predictions summarised
    tar_target(fit_models_stan_predict_, {
      print(paste0("Summarise the posteriors"))
      benthic_models <- fit_models_stan_posterior_predict_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      all_years <- get_all_years_
      ## ---- fit models stan predictions
      benthic_models <- benthic_models |>
        mutate(cellmeans = map2(.x = posteriors,
                                .y = name,
                               .f =  ~ {
                                 posteriors <- readRDS(.x)
                                 ## vars <- get_variables(mod)
                                 cellmeans <-
                                   posteriors |>
                                   posterior::summarise_draws(
                                                median,
                                                HDInterval::hdi,
                                                ~ HDInterval::hdi(.x, credMass = c(0.8))
                                              ) |>
                                   rename(lower_80 = V4, upper_80 = V5) |>
                                   mutate(Year = all_years)
                                 nm <- paste0(data_path, "cellmeans_", .y, ".rds")
                                 saveRDS(cellmeans,
                                         file = nm
                                         )
                                 nm
                               }
                               ))
      ## ----end
      benthic_models
    }
    ),

    ## ## Stan partial plot
    tar_target(fit_models_stan_partial_plot_, {
      benthic_models <- fit_models_stan_predict_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## ---- fit models stan partial plot
      benthic_models <-
        benthic_models |>
        mutate(plot = pmap(.l = list(cellmeans, data, stan_data, name),
                           .f = ~ {
                             cellmeans <- readRDS(..1)
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

                             nm <- paste0(output_path, "figures/pdp_", "_", ..4, ".png")
                             ggsave(
                               filename = nm,
                               g1,
                               width =  6, height =  4, dpi =  72
                             )
                             nm2 <- paste0(output_path, "figures/pdpraw_", "_", ..4, ".png")
                             ggsave(
                               filename = nm2,
                               g2,
                               width =  6, height =  4, dpi =  72
                             )
                             nm
                           }
                           ))
      ## ----end
      benthic_models
    }
    ),

    tar_target(fit_models_raw_data_plot_, {
      benthic_models <- fit_models_stan_partial_plot_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      ## ---- fit models stan raw data plot
      benthic_models <- benthic_models |>
        mutate(raw_plot = pmap(.l = list(data, name),
                               .f =  ~ {
                                 dat <- ..1
                                 g1 <- dat |> ggplot(aes(x = Year, y = value)) +
                                   geom_point(color = "black", alpha = 0.3) +
                                   geom_line(aes(group = Transect),
                                             color = "black", alpha = 0.2) +
                                   theme_classic()
                                 nm <- paste0(output_path, "raw_pdp_", "_", ..2, ".png")
                                 ggsave(
                                   filename = nm,
                                   g1,
                                   width =  6, height =  4, dpi =  72
                                 )
                                 nm
                               }
                               ))
      ## ----end
      benthic_models
    }
    )
    ## ## tar_target(test, {
    ## ##   interpolate_values <- interpolate_values_
    ## ##   interpolate_values(1:5, 1, "mean")
    ## ## })

  )
}
