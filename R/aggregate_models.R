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
               region = str_replace(region, "East Asia", "EAS"),
               subregion = str_replace(GCRMN_subregion, "\\.", " "),
               subregion = str_replace(subregion, "East Asia", "EAS"),
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

    tar_target(xgboost_data_, {
      data_path <- fit_models_global_parameters_$data_path
      ## ---- aggregate xgboost data
      ## data_xgboost <- read_csv(paste0(data_path, "primary/data_xgboost.csv"))
      ## data_xgboost <- get(load("../data/primary/xgboost-trends_2025-09-26.RData"))
      data_xgboost <- get(load("../data/primary/data_xgboost.RData"))
      ## ----end
      data_xgboost
    }),

    tar_target(aggregate_ecoregion_plots_, {
      benthic_posteriors <- aggregate_compile_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      benthic_models <- fit_models_stan_predict_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      data_xgboost <- xgboost_data_
      ## ---- aggregate_ecoregion_plots
      data_xgboost <- data_xgboost |>
        filter(!is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      xgboost_models <- data_xgboost |> pull(ecoregion) |> unique() |> sort()
      stan_models <- benthic_models |> pull(ecoregion) |> unique() |> sort()
      benthic_posteriors_ecoregions <-
        benthic_posteriors |>
        rename(other_data =  data) |>
        left_join(benthic_models |>
                  dplyr::select(region, subregion, ecoregion, category,
                                data, name, stan_data),
                  by = c("region", "subregion", "ecoregion", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "subregion", "ecoregion", "category")) |>
        ## filter(ecoregion == "Central and Southern Great Barrier Reef") |> droplevels() |>
        mutate(plot =
                 pmap(.l = list(summ, data, stan_data, name, xgboost),
                      .f = ~ {
                        cellmeans <- ..1
                        dat <- ..2
                        stan_data <- ..3
                        name <- ..4
                        title <- str_replace_all(name, "_", " ")
                        ytitle <- str_replace(name, ".*_(.*)", "\\1")
                        xgboost <- ..5
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        g1 <- g1 +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        ## Now a version trimmed to minium data year
                        g1a <- stan_partial_plot(
                          cellmeans,
                          stan_data,
                          data = NULL,
                          title = title,
                          ytitle = ytitle,
                          include_raw = FALSE,
                          min_year = min(dat$Year))
                        ## if (!str_detect(..4, "Cocos Islands|Northern Gulf of Mexico|Northern Galapagos Islands|Western Galapagos Islands")) {
                        if (!any(str_detect(..4, setdiff(xgboost_models, stan_models))) &
                            !is.null(xgboost)) {
                          g1 <- g1 +
                            geom_ribbon(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                          ## Now a version trimmed to minium data year
                          g1a <- g1a +
                            geom_ribbon(data = xgboost |> filter(year >= (min(dat$Year))) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost |> filter(year >= (min(dat$Year))) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                        }
                        g2 <- stan_partial_plot(cellmeans, stan_data,
                                                data = dat,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = TRUE)
                        g2a <- stan_partial_plot(
                          cellmeans,
                          stan_data,
                          data = dat,
                          title = title,
                          ytitle = ytitle,
                          include_raw = TRUE,
                          min_year = min(dat$Year))

                        nm <- paste0(output_path, "figures/ecoregion_pdp_", "_", name, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        nma <- paste0(output_path, "figures/ecoregion_pdp_a_", "_", name, ".png")
                        ggsave(
                          filename = nma,
                          g1a,
                          width =  6, height =  4, dpi =  72
                        )
                        nm2 <- paste0(output_path, "figures/ecoregion_pdpraw_", "_", name, ".png")
                        ggsave(
                          filename = nm2,
                          g2,
                          width =  6, height =  4, dpi =  72
                        )

                        nm2a <- paste0(output_path, "figures/ecoregion_pdpraw_a_", "_", name, ".png")
                        ggsave(
                          filename = nm2a,
                          g2a,
                          width =  6, height =  4, dpi =  72
                        )
                        ## if (!str_detect(..4, "Cocos Islands|Northern Gulf of Mexico|Northern Galapagos Islands|Western Galapagos Islands")) {
                        if (!any(str_detect(..4, setdiff(xgboost_models, stan_models))) &
                        !is.null(xgboost)){
                          nm3 <- paste0(output_path,
                            "figures/ecoregion_pdprawxgboost_", "_", name, ".png")
                          g3 <- g2 +
                            geom_ribbon(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                          ggsave(
                            filename = nm3,
                            g3,
                            width =  6, height =  4, dpi =  72
                          )
                          nm3a <- paste0(output_path,
                            "figures/ecoregion_pdprawxgboost_a_", "_", name, ".png")
                          g3a <- g2a +
                            geom_ribbon(data = xgboost |> filter(year >= min(dat$Year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost |> filter(year >= min(dat$Year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                          ggsave(
                            filename = nm3a,
                            g3a,
                            width =  6, height =  4, dpi =  72
                          )
                        }
                        nm
                      }
                      ))
      #benthic_posteriors_ecoregions # ----end
      benthic_posteriors_ecoregions
    }
    ),

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
      library(data.table)
      ## ---- aggregate_subregions
      benthic_posteriors_subregions <-
        benthic_posteriors |>
        ## filter(subregion == "Australia 1") |>
        dplyr::select(-data) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, ecoregion, wt),
                  by = c("region", "subregion", "ecoregion")) |>
        ## ungroup(ecoregion) |>
        ## group_by(.draw, Year, .add = TRUE) |>
        ungroup() |>
        ## mutate(value_wt = value * wt) |>
        mutate(value_wt = as.double(value * wt)) 
        ## group_by(region, subregion, category, .draw, Year) |>
        ## ## summarise(value = sum(value * wt, na.rm = TRUE)) |>
        ## ## summarise(value = sum(value_wt, na.rm = TRUE)) |>
        ## ## summarise(value = .Primative("sum")(value_wt)) 
        ## summarise(value = sum(value_wt)) #|> 
        ## ## summarise(value = weighted.mean(value, w = wt, na.rm = TRUE)) |>
      
      benthic_posteriors_subregions <- setDT(benthic_posteriors_subregions)[, .(value =  sum(value_wt)), by = c("region", "subregion", "category", ".draw", "Year")] |>
        as_tibble() |> 
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
      data_xgboost <- xgboost_data_
      ## ---- aggregate_subregion_plots
      data_xgboost <- data_xgboost |>
        filter(!is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_models |>
        ungroup(ecoregion) |>
        ## group_by(region, subregion, category) |>
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
        left_join(data_xgboost,
                  by = c("region", "subregion", "category")) |>
        mutate(name = paste(subregion, category, sep = "_")) |>
        mutate(plot =
                 pmap(.l = list(summ, stan_data, name, xgboost),
                      .f = ~ {
                        cellmeans <- ..1
                        stan_data <- ..2
                        name <- ..3
                        title <- str_replace_all(name, "_", " ")
                        ytitle <- str_replace(name, ".*_(.*)", "\\1")
                        xgboost <- ..4
                        first_year <- stan_data$all_years[stan_data$data_years[1]]
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)

                        g1 <- g1 +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nm <- paste0(output_path, "figures/subregion_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        nm <- paste0(output_path, "figures/subregion_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        ## Now a version trimmed to minium data year
                        g1a <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE,
                                                min_year = first_year)

                        g1a <- g1a +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nma <- paste0(output_path, "figures/subregion_pdp_a_", "_", ..3, ".png")
                        ggsave(
                          filename = nma,
                          g1a,
                          width =  6, height =  4, dpi =  72
                        )

                        nm2 <- paste0(output_path,
                          "figures/subregion_pdprawxgboost_", "_", ..3, ".png")
                        nm2a <- paste0(output_path,
                          "figures/subregion_pdprawxgboost_a_", "_", ..3, ".png")
                        #print(xgboost)
                        if (!is.null(xgboost)) {
                          g2 <- g1 +
                            geom_ribbon(data = xgboost, inherit.aes = FALSE,
                                        aes(y = mean, x = year,
                                            ymin = lower_ci_95, ymax = upper_ci_95),
                                        color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost, inherit.aes = FALSE,
                                      aes(y = mean, x = year), colour = "blue")
                          g2a <- g1a +
                            geom_ribbon(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                        } else {
                          g2 <- g1
                          g2a <- g1a
                        }
                        ggsave(
                          filename = nm2,
                          g2,
                          width =  6, height =  4, dpi =  72
                        )
                        ggsave(
                          filename = nm2a,
                          g2a,
                          width =  6, height =  4, dpi =  72
                        )

                        nm
                      }
                      ))
      # ----end
      benthic_posteriors_subregions
    }
    ),

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
      library(data.table)
      ## ---- aggregate_regions
      benthic_posteriors_regions <-
        benthic_posteriors_subregions |>
        ## filter(region == "Australia", category == "Hard coral") |> droplevels() |> 
        ## filter(region == "Australia") |> droplevels() |> 
        dplyr::select(-summ) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, subregion, wt),
                  by = c("region", "subregion")) |>
        ungroup() |> 
        mutate(value_wt = as.double(value * wt))
        
      benthic_posteriors_regions <- setDT(benthic_posteriors_regions)[, .(value =  sum(value_wt)), by = c("region", "category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(region, category) |>
        nest(.key = "posteriors") |>
        ## group_by(region, category) |>
        ## nest(.key = "data") |>
        ## mutate(posteriors = map(.x = data,
        ##   .f =  ~ {
        ##     print("here")
        ##     .x |> group_by(.draw, Year) |>
        ##       summarise(value = sum(value_wt), .groups = "keep")
        ##   })) |>
        ## dplyr::select(-data) |> 
        ## ## unnest("values") |> 
        ## ## group_by(region, category, .draw, Year) |>
        ## ## summarise(value = sum(value_wt, na.rm = TRUE)) |>
        ## ## ungroup() |>
        ## ## group_by(region, category) |>
        ## ## nest(.key = "posteriors") |>
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
      data_xgboost <- xgboost_data_
      ## ---- aggregate_region_plots
      data_xgboost <- data_xgboost |>
        filter(!is.na(region), is.na(subregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, category) |>
        nest(.key = "xgboost")
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
        left_join(data_xgboost,
                  by = c("region", "category")) |>
        mutate(name = paste(region, category, sep = "_")) |>
        ## filter(region == "Australia", category == "Hard coral") |> droplevels() |> 
        mutate(plot =
                 pmap(.l = list(summ, stan_data, name, xgboost),
                      .f = ~ {
                        cellmeans <- ..1
                        stan_data <- ..2
                        title <- str_replace_all(..3, "_", " ")
                        ytitle <- str_replace(..3, ".*_(.*)", "\\1")
                        xgboost <- ..4
                        first_year <- stan_data$all_years[stan_data$data_years[1]]
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        g1 <- g1 +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nm <- paste0(output_path, "figures/region_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        ## Now a version trimmed to minium data year
                        g1a <- stan_partial_plot(cellmeans, stan_data,
                          data = NULL,
                          title = title,
                          ytitle = ytitle,
                          include_raw = FALSE,
                          min_year = first_year)
                        g1a <- g1a +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nma <- paste0(output_path, "figures/region_pdp_a_", "_", ..3, ".png")
                        ggsave(
                          filename = nma,
                          g1a,
                          width =  6, height =  4, dpi =  72
                        )
                        nm2 <- paste0(output_path,
                          "figures/region_pdprawxgboost_", "_", ..3, ".png")
                        nm2a <- paste0(output_path,
                          "figures/region_pdprawxgboost_a_", "_", ..3, ".png")
                        if (!is.null(xgboost)) {
                          g2 <- g1 +
                            geom_ribbon(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                          g2a <- g1a +
                            geom_ribbon(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                        } else {
                          g2 <- g1
                          g2a <- g1a
                        }
                        ggsave(
                          filename = nm2,
                          g2,
                          width =  6, height =  4, dpi =  72
                        )
                        ggsave(
                          filename = nm2a,
                          g2a,
                          width =  6, height =  4, dpi =  72
                        )

                        nm
                      }
                      ))
      # ----end
      benthic_posteriors_regions
    }
    ),

    
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
      library(data.table)
      ## ---- aggregate_global
      benthic_posteriors_global <-
        benthic_posteriors_regions |>
        dplyr::select(-summ) |>
        unnest(posteriors) |>
        left_join(wts |> dplyr::select(region, wt),
                  by = c("region")) |>
        ungroup() |> 
        mutate(value_wt = as.double(value * wt))
        
      benthic_posteriors_global <- setDT(benthic_posteriors_global)[, .(value =  sum(value_wt)), by = c("category", ".draw", "Year")] |>
        as_tibble() |> 
        ungroup() |>
        group_by(category) |>
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
      benthic_posteriors_global
      ## ----end
    }
    ),

    tar_target(aggregate_global_plots_, {
      benthic_posteriors_global <- aggregate_global_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      benthic_models <- fit_models_stan_predict_
      data_xgboost <- xgboost_data_
      ## ---- aggregate_global_plots
      data_xgboost <- data_xgboost |>
        filter(is.na(region), is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_models |>
        ungroup(region, subregion, ecoregion) |>
        dplyr::select(category, stan_data) |>
        ## filter(subregion %in% c("Caribbean 1", "Caribbean 2")) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))

      benthic_posteriors_global <-
        benthic_posteriors_global |>
        left_join(stan_data, by = c("category")) |>
        left_join(data_xgboost,
                  by = c("category")) |>
        mutate(name = paste(category, sep = "_")) |>
        mutate(plot =
                 pmap(.l = list(summ, stan_data, name, xgboost),
                      .f = ~ {
                        cellmeans <- ..1
                        stan_data <- ..2
                        title <- str_replace_all(..3, "_", " ")
                        ytitle <- str_replace(..3, ".*_(.*)", "\\1")
                        xgboost <- ..4
                        first_year <- stan_data$all_years[stan_data$data_years[1]]
                        ## plot without raw points
                        g1 <- stan_partial_plot(cellmeans, stan_data,
                                                data = NULL,
                                                title = title,
                                                ytitle = ytitle,
                                                include_raw = FALSE)
                        g1 <- g1 +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nm <- paste0(output_path, "figures/global_pdp_", "_", ..3, ".png")
                        ggsave(
                          filename = nm,
                          g1,
                          width =  6, height =  4, dpi =  72
                        )
                        ## Now a version trimmed to minium data year
                        g1a <- stan_partial_plot(cellmeans, stan_data,
                          data = NULL,
                          title = title,
                          ytitle = ytitle,
                          include_raw = FALSE,
                          min_year = first_year)
                        g1a <- g1a +
                          theme(panel.grid.major.y = element_line(),
                                panel.grid.minor.y = element_line(),
                                panel.grid.major.x = element_line()
                               )
                        nma <- paste0(output_path, "figures/global_pdp_a_", "_", ..3, ".png")
                        ggsave(
                          filename = nma,
                          g1a,
                          width =  6, height =  4, dpi =  72
                        )
                        nm2 <- paste0(output_path,
                          "figures/global_pdprawxgboost_", "_", ..3, ".png")
                        nm2a <- paste0(output_path,
                          "figures/global_pdprawxgboost_a_", "_", ..3, ".png")
                        if (!is.null(xgboost)) {
                          g2 <- g1 +
                            geom_ribbon(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost, inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                          g2a <- g1a +
                            geom_ribbon(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year,
                                ymin = lower_ci_95, ymax = upper_ci_95),
                              color = NA, fill = "blue", alpha = 0.5) +
                            geom_line(data = xgboost |> filter(year >= (first_year)) |> droplevels(),
                              inherit.aes = FALSE,
                              aes(y = mean, x = year), colour = "blue")
                        } else {
                          g2 <- g1
                          g2a <- g1a
                        }
                        ggsave(
                          filename = nm2,
                          g2,
                          width =  6, height =  4, dpi =  72
                        )
                        ggsave(
                          filename = nm2a,
                          g2a,
                          width =  6, height =  4, dpi =  72
                        )

                        nm
                      }
                      ))
      # ----end
      benthic_posteriors_global
    }
    )#,

  )
}
