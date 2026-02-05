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
    tar_target(prepare_data_for_stan_, {
      ## ---- prepare data for stan function
      prepare_data_for_stan <- function(data, yrs) {
        data <- data |> st_drop_geometry()
        if (length(unique(data$cYear)) > 1) {

          X <- model.matrix(~ -1 + cYear, data =  data |> droplevels())
          ## X <- model.matrix(~ -1 + cYear, data = data)
        } else {

          X <- model.matrix(~ 1, data =  data)
        }
        ## DatasetID model matrix
        if (length(unique(data$datasetID)) > 1) {
          contrasts(data$datasetID) <- contr.sum
          Z <- model.matrix(~datasetID, data = data)
        } else {
          Z <- model.matrix(~ 1, data =  data)
        }
        ## all_years <- 1978:2020
        if (is.null(yrs)) {

          all_years <- as.numeric(as.character(levels(data$cYear)))
        } else {

          all_years <- yrs
        }
        data_years <- which(all_years %in% (data |> pull(Year) |> unique() |> sort()))
        no_data_years <- which(!(all_years %in% (data |> pull(Year) |> unique() |> sort())))
        gap_years <- no_data_years[no_data_years > data_years[1] & no_data_years < data_years[length(data_years)]]
        prior_years <- no_data_years[no_data_years < data_years[1]]
        init_year <- data |>
          filter(Year ==  min(Year)) |>
          pull(Year) |>
          unique()
        init_year <- which(init_year ==  all_years)
        post_years <- which(all_years > init_year)
        init_cover <- binomial()$linkfun(data |> filter(Year ==  min(Year)) |>
                                         droplevels() |>
                                         summarise(avCover =  mean(Cover)) |> as.numeric())
        non_init_year <- (1:length(all_years))[-init_year]
        between_years <- gap_years[gap_years < max(data_years)]
        after_years <- post_years[post_years > max(data_years)]
        N <- nrow(data)


        data_lookup <- gap_lookup <- numeric(length(all_years))
        data_lookup[data_years] <- seq_along(data_years)
        gap_lookup[gap_years] <- seq_along(gap_years)
        
        ## Xmat <- model.matrix(~ factor(all_years))
        Xmat <- model.matrix(~ -1 + factor(all_years))

        year_conversions <- data |>
          mutate(
            iYear =  as.numeric(factor(Year)),
            all_years =  as.numeric(factor(Year, levels =  all_years))
          ) |>
          select(iYear, all_years) |>
          distinct() |>
          arrange(iYear) |>
          pull(all_years)

        ## Get weights
        grid_wts1 <- data |>
          group_by(grid_id) |>
          ## summarise(area =  unique(sum)) |>
          summarise(area =  as.numeric(unique(Area))) |>
          ungroup() |>
          mutate(wt =  area / sum(area))

        wts <- data |>
          dplyr::select(-area) |> 
          ## mutate(grid_id = as.numeric(as.character(grid_id))) |>
          group_by(grid_id) |> 
          mutate(N = n()) |>
          ungroup() |>
          left_join(grid_wts1, by = "grid_id") |>
          mutate(wt = wt/N) |>
          dplyr::select(grid_id, wt, N) |>
          mutate(grid_id2 = as.numeric(factor(grid_id))) |>
          distinct() |>
          mutate(wt = ifelse(is.na(wt), 0, wt))

        stan_data <- list(
          N =  N,
          Y =  data$Cover,
          K =  ncol(X),
          X =  X,
          Z =  Z,
          P =  ncol(Xmat),
          Xmat =  Xmat,
          Z_0_1 =  rep(0, N),
          Z_1_1 =  rep(0, N),
          Z_2_1 =  rep(0, N),
          Z_3_1 =  rep(0, N),
          J_0 =  as.numeric(factor(data$datasetID)),  ## datasetID
          J_1 =  as.numeric(factor(data$grid_id)),    ## grid_id
          J_2 =  as.numeric(factor(data$cSite)),      ## cSite
          J_3 =  as.numeric(factor(data$cReplicate)), ## cReplicate
          N_0 =  length(unique(factor(data$datasetID))),
          M_0 =  1,
          N_1 =  length(unique(factor(data$grid_id))),
          M_1 =  1,
          NC_1 =   0,
          ## wt_1 =  grid_wts$wt,
          wt_1 =  wts$wt,
          N_2 =  length(unique(factor(data$cSite))),
          M_2 =  1,
          NC_2 =   0,
          N_3 =  length(unique(factor(data$cReplicate))),
          M_3 =  1,
          NC_3 =   0,
          n_all_years =  length(all_years),
          all_years =  all_years,
          n_prior_years =  length(prior_years),
          prior_years =  prior_years,
          n_post_years =  length(post_years),
          post_years =  post_years,
          init_year =  init_year,
          init_cover =  init_cover,
          n_gap_years =  length(gap_years),
          gap_years =  gap_years,
          n_after_years =  length(after_years),
          after_years =  after_years,
          n_data_years =  length(data_years),
          data_years =  data_years,
          year_conversions =  year_conversions,
          data_lookup =  data_lookup,
          gap_lookup = gap_lookup
        )
        stan_data
      }
      ## ----end
      prepare_data_for_stan
    }
    ),
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
          category = factor(category, levels = unique(category)),
          name = factor(name, levels = unique(name))
        ) |>
        group_by(region, subregion, ecoregion, category, name) |>
        filter(category %in% c("Hard coral", "Macroalgae", "Algae", "Turf algae")) |>
        droplevels()
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
      run_again <- TRUE
      if (!run_again) {
        benthic_models <- benthic_models |>
          mutate(mod = map2(.x = stan_data, .y = name,
            .f =  ~ {
              name <- .y
              nm <- paste0(data_path, "mod_", name, ".rds")
              nm
            })) 
      } else {
        model_stan <- cmdstanr::cmdstan_model(stan_file =  "gcrmn_model.stan")
        print(paste0("run again: ", run_again))
        options(future.globals.maxSize = 2 * 1024^3)
        old_plan <- future::plan(future::multisession, workers = 20)
        results <- furrr::future_map2(.x = benthic_models$stan_data,
          .y = benthic_models$name,
          .f = ~ {
            name <- .y
            nm <- paste0(data_path, "mod_", name, ".rds")
            mod_stan <- NULL
            if (file.exists(nm)) {
              cat(paste("\n", "Skipping", name, "as it already exists\n"))
              mod_stan <- readRDS(file = nm)
            } else {
              cat(paste("\n", "Running", name, "\n"))
              mod_stan <-
                model_stan$sample(
                  data =  .x,
                  seed =  123,
                  iter_sampling =  10000,
                  iter_warmup =  5000,
                  ## init = init,
                  thin =  10,
                  chains =  3,
                  parallel_chains =  3,
                  adapt_delta =  0.99,
                  output_dir =  data_path,
                  )
              saveRDS(mod_stan,
                file = nm
              )
            }
            sig <- digest::digest(mod_stan)
            list(path = nm, signature = sig)
          },
          .progress = FALSE,
          .options = furrr::furrr_options(scheduling = Inf)
        )
        future::plan(old_plan)
        benthic_models <- benthic_models |>
          ungroup() |>                                      ## need to ungroup for the following
          mutate(mod = sapply(results, FUN = \(x) x$path),
            signature = sapply(results, FUN = \(x) x$signature)) |>
          group_by(region, subregion, ecoregion, category)  ## put the groups back
        
      }
      ## ----end
      sig <- digest::digest(benthic_models)
      list(benthic_models =  benthic_models,
        signature = sig)
    }
    ),

    ## Stan posterior predictions
    tar_target(get_posteriors_, {
      ## ---- get_posteriors function
      get_posteriors <- function(.x, .y, type = "cellmeans_years", data_path) {
        mod <- readRDS(paste0(data_path, "mod_", .y, ".rds"))
        vars <- get_variables(mod)
        if (type == "cellmeans_years") {
          posteriors <-
            mod$draws(variables = "cellmeans_years") |>
            posterior::as_draws_df() 
          nm <- paste0(data_path, "posteriors_V2", .y, ".rds")
        } else {
          posteriors <-
            mod$draws(variables = "Years") |>
            posterior::as_draws_df() |>
            mutate(across(starts_with("Years"), plogis))
          nm <- paste0(data_path, "posteriors_", .y, ".rds")
        }
        saveRDS(posteriors,
          file = nm
        )
        nm
      }
      ## ----end
      get_posteriors
    }),

    tar_target(fit_models_stan_posterior_predict_, {
      benthic_models <- fit_models_stan_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      get_posteriors <- get_posteriors_
      all_years <- get_all_years_
      print(paste0("The Number of available cores:", parallelly::availableCores()))
      benthic_models <- benthic_models$benthic_models
      ## ---- fit models stan posterior predictions
      ## for Years (saved to DATA_PATH/posteriors_*)
      benthic_models <- benthic_models |>
        ungroup() |> 
        mutate(posteriors = list(
          parallel::mcmapply(FUN = get_posteriors,
            mod, name, "Years", data_path, mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(posteriors = posteriors[[1]]) |>
        group_by(region, subregion, ecoregion, category)

      ## for Cellmeans (saved to DATA_PATH/posteriors_V2_*)
      benthic_models <- benthic_models |>
        ungroup() |> 
        mutate(posteriors_V2 = list(
          parallel::mcmapply(FUN = get_posteriors,
            mod, name, "cellmeans_years", data_path, mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(posteriors_V2 = posteriors_V2[[1]]) |>
        group_by(region, subregion, ecoregion, category)
      ## ----end
      sig <- digest::digest(benthic_models)
      list(benthic_models = benthic_models,
        signature = sig)
    }
    ),

    ## Stan predictions summarised
    tar_target(summarise_posteriors_file_, {
      ## ---- summarise_posteriors file function
      summarise_posteriors_file <- function(.x, .y, all_years, type = "cellmeans_years", data_path) {
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
        if (type == "cellmeans") {
          nm <- paste0(data_path, "cellmeans_V2_", .y, ".rds")
        } else {
          nm <- paste0(data_path, "cellmeans_", .y, ".rds")
        }
        saveRDS(cellmeans,
          file = nm
        )
        sig <- digest::digest(cellmeans)
        list(path = nm, signature =  sig)
      }
      ## ----end
      summarise_posteriors_file
    }),
    tar_target(fit_models_stan_predict_, {
      print(paste0("Summarise the posteriors"))
      benthic_models <- fit_models_stan_posterior_predict_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      all_years <- get_all_years_
      benthic_models <- benthic_models$benthic_models
      summarise_posteriors_file <- summarise_posteriors_file_
      ## ---- fit models stan predictions
      print("STAN Posterior predictions")
      benthic_models <- benthic_models |>
        ungroup() |> 
        ## for Years (saved to DATA_PATH/cellmeans_*)
        mutate(cellmeans = list(
          parallel::mcmapply(FUN = summarise_posteriors_file,
            posteriors, name,
            MoreArgs = list(all_years, "Years", data_path),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(signature = sapply(cellmeans[[1]], FUN = \(x) x$signature)) |>
        mutate(cellmeans = sapply(cellmeans[[1]], FUN = \(x) x$path)) |>

        ## for Cellmeans (saved to DATA_PATH/cellmeans_V2_*)
        mutate(cellmeans_V2 = list(
          parallel::mcmapply(FUN = summarise_posteriors_file,
            posteriors_V2, name,
            MoreArgs = list(all_years, "cellmeans_years", data_path),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(signature_V2 = sapply(cellmeans_V2[[1]], FUN = \(x) x$signature)) |>
        mutate(cellmeans_V2 = sapply(cellmeans_V2[[1]], FUN = \(x) x$path)) |>
        group_by(region, subregion, ecoregion, category)

      ## ----end
      sig <- digest::digest(benthic_models)
      list(benthic_models = benthic_models,
        signature = sig)
    }
    ),

    ## Calculate simple and simple hierarachical means
    tar_target(fit_models_simple_hierarchical_means_, {
      benthic_models <- fit_models_refactor_ #fit_models_stan_partial_plot_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      ## benthic_models <- benthic_models$benthic_models
      ## ---- fit models simple hierarchical means
      benthic_models <- benthic_models |>
        mutate(simple_means = pmap(.l = list(data, name),
          .f =  ~ {
            dat <- ..1
            dat1 <- dat |>
              group_by(datasetID, grid_id, Site, Transect, cYear) |>
              summarise(mean = mean(measurementValue, na.rm = TRUE),
                median = median(measurementValue, na.rm = TRUE),
                .groups = "keep") |>
              ungroup() |>
              group_by(datasetID, grid_id, Site, cYear) |>
              summarise(mean = mean(mean, na.rm = TRUE),
                median = median(median, na.rm = TRUE),
                .groups = "keep") |>
              ungroup() |>
              group_by(datasetID, grid_id, cYear) |>
              summarise(mean = mean(mean, na.rm = TRUE),
                median = median(median, na.rm = TRUE),
                .groups = "keep") |>
              ungroup() |>
              group_by(datasetID, cYear) |>
              summarise(mean = mean(mean, na.rm = TRUE),
                median = median(median, na.rm = TRUE),
                .groups = "keep") |>
              ungroup() |>
              group_by(cYear) |>
              summarise(mean = mean(mean, na.rm = TRUE),
                median = median(median, na.rm = TRUE),
                .groups = "keep")
            dat2 <- dat |>
              group_by(cYear) |>
              summarise(smean = mean(measurementValue, na.rm = TRUE),
                smedian = median(measurementValue, na.rm = TRUE))
            dat1 |> full_join(dat2, by = "cYear")
          }
        ))

      ## ----end
      benthic_models
    }
    )

  )
}
