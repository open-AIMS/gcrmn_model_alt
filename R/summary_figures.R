summary_figures <- function() {
  targets <- list(
    # Target: Load processing libraries
    tar_target(
      summary_figurese_libraries_,
      {
        ## ---- summary figures libraries
        library(tidyverse)      # for data manipulation and visualisation
        library(furrr)
        library(data.table)
        ## ----end
      }
    ),

    tar_target(summary_figures_global_parameters_, {
        ## ---- summary_figures global parameters
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
        other_paths <- list(
          raw_figs_path = paste0(output_path, "figures/raw/"),
          pdp_figs_path = paste0(output_path, "figures/pdp/"),
          ecoregion_figs_path = paste0(output_path, "figures/ecoregion/"),
          subregion_figs_path = paste0(output_path, "figures/subregion/"),
          region_figs_path = paste0(output_path, "figures/region/"),
          global_figs_path = paste0(output_path, "figures/global/")
        )
        lapply(other_paths, function(x) {
          if (!dir.exists(x)) {
            dir.create(x)
          }
        })
        ## ----end
        paths
      }
    ),


    ## Raw plots (output/figures/raw/raw_pdp_*)
    tar_target(summary_figures_raw_data_plot_, {
      benthic_models <- fit_models_simple_hierarchical_means_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      raw_plots <- raw_plots_
      ## ---- summary figures stan raw data plot
      summary_figures <- benthic_models |>
        ungroup() |> 
        ## for Years (saved to FIG_PATH/raw_pdp_*)
        mutate(plot = list(
          parallel::mcmapply(FUN = raw_plots,
            data, name, simple_means,
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |> 
        mutate(plot = plot[[1]]) |> 
        group_by(region, subregion, ecoregion, category)

      ## ----end
      summary_figures
    }
    ),

    ## Partial dependency plots (output/figures/pdp/pdp_*)
    tar_target(summary_figures_stan_partial_plot_, {
      benthic_models <- fit_models_stan_predict_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      partial_plots <- partial_plots_
      benthic_models <- benthic_models$benthic_models
      ## ---- summary figures stan partial plot
      summary_figures <- benthic_models |>
        ungroup() |> 
        ## for Years (saved to FIG_PATH/pdp_*)
        mutate(plot = list(
          parallel::mcmapply(FUN = partial_plots,
            cellmeans, data, stan_data, name, "Years",
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(signature = sapply(plot[[1]], \(x) x$signature)) |> 
        mutate(plot = sapply(plot[[1]], \(x) x$path)) |> 
        mutate(plot_V2 = list(
          parallel::mcmapply(FUN = partial_plots,
            cellmeans_V2, data, stan_data, name, "cellmeans_years",
            mc.cores = 40, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(signature_V2 = sapply(plot_V2[[1]], FUN = \(x) x$signature)) |> 
        mutate(plot_V2 = sapply(plot_V2[[1]], FUN = \(x) x$signature)) |>
        group_by(region, subregion, ecoregion, category)
        
      ## ----end
      sig <- digest::digest(summary_figures)
      list(summary_figures = summary_figures,
        signature = sig)
    }
    ),

    ## Function for producing the various pdp plots
    ## path: output/figures/<scale>/
    ## _pdp: Bayesian Hierachical trend (priors)
    ## _pdp_V2: Bayesian Hierachical trend (betas)
    ## _pdp_a: Trimmed Bayesian Hierachical trend (priors)
    ## _pdp_a_V2: Trimmed Bayesian Hierachical trend (betas)
    ## _pdpraw: Bayesian Hierachical trend (priors) + raw data
    ## _pdpraw_V2: Bayesian Hierachical trend (betas) + raw data
    ## _pdpraw_a: Trimmed Bayesian Hierachical trend (priors) + raw data
    ## _pdpraw_a_V2: Trimmed Bayesian Hierachical trend (betas) + raw data
    ## _pdprawxgboost: Bayesian Hierachical trend (priors) + raw data + xgboost
    ## _pdprawxgboost_V2: Bayesian Hierachical trend (betas) + raw data + xgboost
    ## _pdprawxgboost_a: Trimmed Bayesian Hierachical trend (priors) + raw data + xgboost
    ## _pdprawxgboost_a_V2: Trimmed Bayesian Hierachical trend (betas) + raw data + xgboost
    ## _pdprawxgboost_simple: Bayesian Hierachical trend (priors) + raw data + xgboost + simple means
    ## _pdprawxgboost_simple_V2: Bayesian Hierachical trend (betas) + raw data + xgboost + simple means
    ## _pdprawxgboost_a_simple: Trimmed Bayesian Hierachical trend (priors) + raw data + xgboost + simple means
    ## _pdprawxgboost_a_simple_V2: Trimmed Bayesian Hierachical trend (betas) + raw data + xgboost + simple means
    ## _pdp_simple: Bayesian Hierachical trend (priors) + simple means
    ## _pdp_simple_V2: Bayesian Hierachical trend (betas) + simple means
    ## _pdp_a_simple: Trimmed Bayesian Hierachical trend (priors) + simple means
    ## _pdp_a_simple_V2: Trimmed Bayesian Hierachical trend (betas) + simple means
    tar_target(pdp_plots_, {
      interpolate_values <- interpolate_values_
      output_path <- summary_figures_global_parameters_$output_path
      stan_partial_plot <- stan_partial_plot_
      ## ---- stan partial plot function
      pdp_plots <- function(cellmeans, dat, stan_data, name, xgboost,
                            simple_means =  NA,
                            xgboost_models = NULL, stan_models = NULL,
                            type = "cellmeans_years", .scale = "ecoregion",
                            final_year = 2024) {
        ## xgboost_models <- more_args[1]
        ## stan_models <- more_args[2]
        ## type <- more_args[3]
        ## .scale <- more_args[4]
        title <- str_replace_all(name, "_", " ")
        ytitle <- str_replace(name, ".*_(.*)", "\\1")
        first_year <- stan_data$all_years[stan_data$data_years[1]]
        figure_path <- paste0(output_path, "figures/", .scale, "/")
        if (type == "cellmeans_years") {
          ## (g1) Bayes + xgboost (where available), full year range, no data, no simple means
          nm <- paste0(figure_path, .scale, "_pdp_V2_", "_", name, ".png")
          ## (g1a) Bayes + xgboost (where available), trimmed year range, no data, no simple means
          nma <- paste0(figure_path, .scale, "_pdp_a_V2_", "_", name, ".png")
          ## (g0) Bayes only, full year range, no data, no simple means
          nm0 <- paste0(figure_path, .scale, "_pdp_bayes_V2_", "_", name, ".png")
          ## (g0a) Bayes, trimmed year range, no data, no simple means
          nm0a <- paste0(figure_path, .scale, "_pdp_bayes_a_V2_", "_", name, ".png")
          ## (g2) Bayes, full year range, with data, no simple means
          nm2 <- paste0(figure_path, .scale, "_pdpraw_V2_", "_", name, ".png")
          ## (g2a) Bayes, trimmed year range, with data, no simple means
          nm2a <- paste0(figure_path, .scale, "_pdpraw_a_V2_", "_", name, ".png")
          ## (g3) Bayes + xgboost, full year range, with data, no simple means
          nm3 <- paste0(figure_path, .scale, "_pdprawxgboost_V2_", "_", name, ".png")
          ## (g3a) Bayes + xgboost, full year range, with data, no simple means
          nm3a <- paste0(figure_path, .scale, "_pdprawxgboost_a_V2_", "_", name, ".png")
          ## (g4) Bayes + xgboost, full year range, with data, simple means
          nm4 <- paste0(figure_path, .scale, "_pdprawxgboost_simple_V2_", "_", name, ".png")
          ## (g4a) Bayes + xgboost, trimmed year range, with data, simple means
          nm4a <- paste0(figure_path, .scale, "_pdprawxgboost_a_simple_V2_", "_", name, ".png")
          ## (g1*) Bayes + xgboost, full year range, no data, simple means
          nm5 <- paste0(figure_path, .scale, "_pdp_simple_V2_", "_", name, ".png")
          ## (g1a*) Bayes, trimmed year range, no data, simple means
          nm5a <- paste0(figure_path, .scale, "_pdp_a_simple_V2_", "_", name, ".png")
          ## (g0a*) Bayes, timmed year range, no data, simple means
          nm6a <- paste0(figure_path, .scale, "_pdp_bayes_a_simple_V2_", "_", name, ".png")
          ## (g7a) Bayes, trimmed year range, with data, simple means
          nm7a <- paste0(figure_path, .scale, "_pdpraw_bayes_a_simple_V2_", "_", name, ".png")
        } else {
          ## (g1) Bayes + xgboost (where available), full year range, no data, no simple means
          nm <- paste0(figure_path, .scale, "_pdp_", "_", name, ".png")
          ## (g1a) Bayes + xgboost (where available), trimmed year range, no data, no simple means
          nma <- paste0(figure_path, .scale, "_pdp_a_", "_", name, ".png")
          ## (g0) Bayes only, full year range, no data, no simple means
          nm0 <- paste0(figure_path, .scale, "_pdp_bayes_", "_", name, ".png")
          ## (g0a) Bayes, trimmed year range, no data, no simple means
          nm0a <- paste0(figure_path, .scale, "_pdp_bayes_a_", "_", name, ".png")
          nm2 <- paste0(figure_path, .scale, "_pdpraw_", "_", name, ".png")
          nm2a <- paste0(figure_path, .scale, "_pdpraw_a_", "_", name, ".png")
          nm3 <- paste0(figure_path, .scale, "_pdprawxgboost_", "_", name, ".png")
          nm3a <- paste0(figure_path, .scale, "_pdprawxgboost_a_", "_", name, ".png")
          nm4 <- paste0(figure_path, .scale, "_pdprawxgboost_simple_", "_", name, ".png")
          nm4a <- paste0(figure_path, .scale, "_pdprawxgboost_a_simple_", "_", name, ".png")
          nm5 <- paste0(figure_path, .scale, "_pdp_simple_", "_", name, ".png")
          nm5a <- paste0(figure_path, .scale, "_pdp_a_simple_", "_", name, ".png")
          nm6a <- paste0(figure_path, .scale, "_pdp_bayes_a_simple_", "_", name, ".png")
          nm7a <- paste0(figure_path, .scale, "_pdpraw_bayes_a_simple_", "_", name, ".png")
        }

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
        ## Bayes only, full year range, no data, no simple means
        ggsave(
          filename = nm0,
          g1,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        g0 <- g1
        
        if (!any(str_detect(name, setdiff(xgboost_models, stan_models))) &
              !is.null(xgboost)) {
          g1 <- g1 +
            geom_ribbon(data = xgboost, inherit.aes = FALSE,
              aes(y = mean, x = year,
                ymin = lower_ci_95, ymax = upper_ci_95),
              color = NA, fill = "blue", alpha = 0.5) +
            geom_line(data = xgboost, inherit.aes = FALSE,
              aes(y = mean, x = year), colour = "blue")
        }
        ## Bayes + xgboost (where available), full year range, no data, no simple means
        ggsave(
          filename = nm,
          g1,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        ## Now a version trimmed to minium data year
        g1a <- stan_partial_plot(
          cellmeans,
          stan_data,
          data = NULL,
          title = title,
          ytitle = ytitle,
          include_raw = FALSE,
          ## min_year = min(dat$Year))
          min_year = first_year,
          max_year = final_year)

        ## Bayes, trimmed year range, no data, no simple means
        ggsave(
          filename = nm0a,
          g1a,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        g0a <- g1a
        
        if (!any(str_detect(name, setdiff(xgboost_models, stan_models))) &
              !is.null(xgboost)) {
          ## Now a version trimmed to minium data year
          g1a <- g1a +
            geom_ribbon(data = xgboost |> filter(year >= first_year) |> droplevels(),
              inherit.aes = FALSE,
              aes(y = mean, x = year,
                ymin = lower_ci_95, ymax = upper_ci_95),
              color = NA, fill = "blue", alpha = 0.5) +
            geom_line(data = xgboost |> filter(year >= first_year) |> droplevels(),
              inherit.aes = FALSE,
              aes(y = mean, x = year), colour = "blue")
        }
        ## Bayes + xgboost (where available), trimmed year range, no data, no simple means
        ggsave(
          filename = nma,
          g1a,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        if (.scale == "ecoregion") {
          g2 <- stan_partial_plot(cellmeans, stan_data,
            data = dat,
            title = title,
            ytitle = ytitle,
            include_raw = TRUE)
          ## Bayes, full year range, with data, no simple means
          ggsave(
            filename = nm2,
            g2,
            width =  6, height =  4, dpi =  72
          ) |> suppressWarnings() |> suppressMessages()
          
          g2a <- stan_partial_plot(
            cellmeans,
            stan_data,
            data = dat,
            title = title,
            ytitle = ytitle,
            include_raw = TRUE,
            min_year = min(dat$Year),
            max_year = final_year)
          ## Bayes, trimmed year range, with data, no simple means
          ggsave(
            filename = nm2a,
            g2a,
            width =  6, height =  4, dpi =  72
          ) |> suppressWarnings() |> suppressMessages()

          if (!any(str_detect(name, setdiff(xgboost_models, stan_models))) &
                !is.null(xgboost)){
            g3 <- g2 +
              geom_ribbon(data = xgboost, inherit.aes = FALSE,
                aes(y = mean, x = year,
                  ymin = lower_ci_95, ymax = upper_ci_95),
                color = NA, fill = "blue", alpha = 0.5) +
              geom_line(data = xgboost, inherit.aes = FALSE,
                aes(y = mean, x = year), colour = "blue")
            ## Bayes + xgboost, full year range, with data, no simple means
            ggsave(
              filename = nm3,
              g3,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()
          
            g3a <- g2a +
              geom_ribbon(data = xgboost |> filter(year >= min(dat$Year)) |> droplevels(),
                inherit.aes = FALSE,
                aes(y = mean, x = year,
                  ymin = lower_ci_95, ymax = upper_ci_95),
                color = NA, fill = "blue", alpha = 0.5) +
              geom_line(data = xgboost |> filter(year >= min(dat$Year)) |> droplevels(),
                inherit.aes = FALSE,
                aes(y = mean, x = year), colour = "blue")
            ## Bayes + xgboost, full year range, with data, no simple means
            ggsave(
              filename = nm3a,
              g3a,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()
          }
          
          if (!any(is.na(simple_means))) {
            ## Add the simple means
            simple_means <- simple_means |>
              mutate(across(c(mean, median, smean, smedian), \(x) x/100)) |>
              filter(as.numeric(as.character(cYear)) <= final_year) |>
              droplevels()
            g1 <- g1 +
              geom_line(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +
              geom_point(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +

              geom_line(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +
              geom_point(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +

              geom_point(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +
              geom_line(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +

              geom_point(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) +
              geom_line(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) 

            ## Bayes + xgboost, full year range, no data, simple means
            ggsave(
              filename = nm5,
              g1,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()
            g0a <- g0a +
              geom_line(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +
              geom_point(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +

              geom_line(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +
              geom_point(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +

              geom_point(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +
              geom_line(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +

              geom_point(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) +
              geom_line(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) 
            ## Bayes, full year range, no data, simple means
            ggsave(
              filename = nm6a,
              g0a,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()

            g1a <- g1a +
              geom_line(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +
              geom_point(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +

              geom_line(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +
              geom_point(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +

              geom_point(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +
              geom_line(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +

              geom_point(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) +
              geom_line(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) 
            ## Bayes, trimmed year range, no data, simple means
            ggsave(
              filename = nm5a,
              g1a,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()
              
            g7a <- g2a +
              geom_line(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +
              geom_point(data = simple_means,
                aes(y = smean, x = as.numeric(as.character(cYear)),
                  color = "simple mean")) +

              geom_line(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +
              geom_point(data = simple_means,
                aes(y = mean, x = as.numeric(as.character(cYear)),
                  color = "hier. mean")) +

              geom_point(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +
              geom_line(data = simple_means,
                aes(y = smedian, x = as.numeric(as.character(cYear)),
                  color = "simple median")) +

              geom_point(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) +
              geom_line(data = simple_means,
                aes(y = median, x = as.numeric(as.character(cYear)),
                  color = "hier. median")) 
            ## Bayes, trimmed year range, with data, simple means
            ggsave(
              filename = nm7a,
              g7a,
              width =  6, height =  4, dpi =  72
            ) |> suppressWarnings() |> suppressMessages()

            if (!any(str_detect(name, setdiff(xgboost_models, stan_models))) &
                  !is.null(xgboost)){
              g4 <- g3 +
                geom_line(data = simple_means,
                  aes(y = smean, x = as.numeric(as.character(cYear)),
                    color = "simple mean")) +
                geom_point(data = simple_means,
                  aes(y = smean, x = as.numeric(as.character(cYear)),
                    color = "simple mean")) +
                geom_line(data = simple_means,
                  aes(y = mean, x = as.numeric(as.character(cYear)),
                    color = "hier. mean")) +
                geom_point(data = simple_means,
                  aes(y = mean, x = as.numeric(as.character(cYear)),
                    color = "hier. mean")) +
                geom_point(data = simple_means,
                  aes(y = smedian, x = as.numeric(as.character(cYear)),
                    color = "simple median")) +
                geom_line(data = simple_means,
                  aes(y = smedian, x = as.numeric(as.character(cYear)),
                    color = "simple median")) +
                geom_point(data = simple_means,
                  aes(y = median, x = as.numeric(as.character(cYear)),
                    color = "hier. median")) +
                geom_line(data = simple_means,
                  aes(y = median, x = as.numeric(as.character(cYear)),
                    color = "hier. median")) 
              ## Bayes + xgboost, full year range, with data, simple means
              ggsave(
                filename = nm4,
                g4,
                width =  6, height =  4, dpi =  72
              ) |> suppressWarnings() |> suppressMessages()

              g4a <- g3a +
                geom_line(data = simple_means,
                  aes(y = smean, x = as.numeric(as.character(cYear)),
                    color = "simple mean")) +
                geom_point(data = simple_means,
                  aes(y = smean, x = as.numeric(as.character(cYear)),
                    color = "simple mean")) +
                geom_line(data = simple_means,
                  aes(y = mean, x = as.numeric(as.character(cYear)),
                    color = "hier. mean")) +
                geom_point(data = simple_means,
                  aes(y = mean, x = as.numeric(as.character(cYear)),
                    color = "hier. mean")) +
                geom_point(data = simple_means,
                  aes(y = smedian, x = as.numeric(as.character(cYear)),
                    color = "simple median")) +
                geom_line(data = simple_means,
                  aes(y = smedian, x = as.numeric(as.character(cYear)),
                    color = "simple median")) +
                geom_point(data = simple_means,
                  aes(y = median, x = as.numeric(as.character(cYear)),
                    color = "hier. median")) +
                geom_line(data = simple_means,
                  aes(y = median, x = as.numeric(as.character(cYear)),
                    color = "hier. median")) 
              ## Bayes + xgboost, trimmed year range, with data, simple means
              ggsave(
                filename = nm4a,
                g4a,
                width =  6, height =  4, dpi =  72
              ) |> suppressWarnings() |> suppressMessages()
            }
          }
          
        }

        nm
      }
      ## ----end
      pdp_plots
    }),

    tar_target(summarise_info_, {
      ## ---- summarise info function
      summarise_info <- function(dat) {
       dat |> 
        mutate(
          min_year = map(.x = stan_data,
            .f = ~ {
              .x$all_years[min(.x$data_years)]
            }),
          max_year = map(.x = stan_data,
            .f = ~ {
              .x$all_years[max(.x$data_years)]
            }),
          cellmeans_trim = pmap(
            .l = list(cellmeans, min_year, max_year),
            .f =  ~ {
              ..1 |> filter(Year >= ..2, Year <= ..3)
            }
          )
        )
      }
      ## ----end
      summarise_info
    }
    ),

    ## Ecoregion summary plots
    tar_target(aggregate_ecoregion_plots_, {
      benthic_posteriors <- aggregate_compile_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      simple_hier_means <- fit_models_simple_hierarchical_means_
      ## ---- aggregate_ecoregion_plots
      print("Ecoregion plots")
      data_xgboost <- data_xgboost |>
        filter(!is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      xgboost_models <- data_xgboost |> pull(ecoregion) |> unique() |> sort()
      stan_models <- benthic_posteriors |> pull(ecoregion) |> unique() |> sort()
      benthic_posteriors_ecoregions <-
        benthic_posteriors |>
        rename(other_data =  data) |>
        left_join(simple_hier_means |>
                  dplyr::select(region, subregion, ecoregion, category,
                                data, name, simple_means),
                  by = c("region", "subregion", "ecoregion", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "subregion", "ecoregion", "category")) |>
        ungroup() |> 
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, data, stan_data, name, xgboost, simple_means = simple_means,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "Years", .scale = "ecoregion"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, subregion, ecoregion, category)
      ## ----end
      benthic_posteriors_ecoregions
    }
    ),
    tar_target(aggregate_ecoregion_plots_V2_, {
      benthic_posteriors <- aggregate_compile_V2_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      benthic_models <- fit_models_stan_predict_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      simple_hier_means <- fit_models_simple_hierarchical_means_
      ## ---- aggregate_ecoregion_plots V2
      print("Ecoregion plots V2")
      data_xgboost <- data_xgboost |>
        filter(!is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      xgboost_models <- data_xgboost |> pull(ecoregion) |> unique() |> sort()
      stan_models <- simple_hier_means |> pull(ecoregion) |> unique() |> sort()
      benthic_posteriors_ecoregions_V2 <-
        benthic_posteriors |>
        rename(other_data =  data) |>
        left_join(simple_hier_means |>
                  dplyr::select(region, subregion, ecoregion, category,
                                data, name, simple_means),
                  by = c("region", "subregion", "ecoregion", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "subregion", "ecoregion", "category")) |>
        ungroup() |> 
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, data, stan_data, name, xgboost, simple_means = simple_means,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "cellmeans_years", .scale = "ecoregion"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, subregion, ecoregion, category)
      ## ----end
      benthic_posteriors_ecoregions_V2
    }
    ),

    ## Subregion summary plots
    tar_target(aggregate_subregion_plots_, {
      benthic_posteriors <- aggregate_compile_
      benthic_posteriors_subregions <- aggregate_subregions_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## benthic_models <- fit_models_stan_predict_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_subregion_plots
      data_xgboost <- data_xgboost |>
        filter(!is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(ecoregion) |>
        ## group_by(region, subregion, category) |>
        dplyr::select(region, subregion, category, stan_data) |>
        ## filter(subregion %in% c("Caribbean 1", "Caribbean 2")) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(subregion) |> unique() |> sort()
      stan_models <- benthic_posteriors_subregions |> pull(subregion) |> unique() |> sort()
        
      benthic_posteriors_subregions <-
        benthic_posteriors_subregions |>
        left_join(stan_data, by = c("region", "subregion", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "subregion", "category")) |>
        mutate(name = paste(subregion, category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "Years", .scale = "subregion"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, subregion, category)

      # ----end
      benthic_posteriors_subregions
    }
    ),
    tar_target(aggregate_subregion_plots_V2_, {
      benthic_posteriors <- aggregate_compile_V2_
      benthic_posteriors_subregions_V2 <- aggregate_subregions_V2_
      data_path <- summary_figures_global_parameters_$data_path
      output_path <- summary_figures_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## benthic_models <- fit_models_stan_predict_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_subregion_plots
      data_xgboost <- data_xgboost |>
        filter(!is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, subregion, ecoregion, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(ecoregion) |>
        ## group_by(region, subregion, category) |>
        dplyr::select(region, subregion, category, stan_data) |>
        ## filter(subregion %in% c("Caribbean 1", "Caribbean 2")) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(subregion) |> unique() |> sort()
      stan_models <- benthic_posteriors_subregions_V2 |> pull(subregion) |> unique() |> sort()
        
      benthic_posteriors_subregions_V2 <-
        benthic_posteriors_subregions_V2 |>
        left_join(stan_data, by = c("region", "subregion", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "subregion", "category")) |>
        mutate(name = paste(subregion, category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "cellmeans_years", .scale = "subregion"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, subregion, category)

      # ----end
      benthic_posteriors_subregions_V2
    }
    ),
    tar_target(summary_subregion_, {
      benthic_posteriors_subregion <- aggregate_subregion_plots_
      summarise_info <- summarise_info_
      ## ---- summary subregion
      summary_subregion <- benthic_posteriors_subregion |> 
        summarise_info()
      # ----end
      summary_subregion
    }
    ),
    tar_target(summary_subregion_V2_, {
      benthic_posteriors_subregion_V2 <- aggregate_subregion_plots_V2_
      summarise_info <- summarise_info_
      ## ---- summary subregion
      summary_subregion_V2 <- benthic_posteriors_subregion_V2 |> 
        summarise_info()
      # ----end
      summary_subregion_V2
    }
    ),

    ## Region summary plots
    tar_target(aggregate_region_plots_, {
      benthic_posteriors <- aggregate_compile_
      benthic_posteriors_regions <- aggregate_regions_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## benthic_models <- fit_models_stan_predict_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_region_plots
      print("Regional plots")
      data_xgboost <- data_xgboost |>
        filter(!is.na(region), is.na(subregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(subregion, ecoregion) |>
        dplyr::select(region, category, stan_data) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(region) |> unique() |> sort()
      stan_models <- benthic_posteriors_regions |> pull(region) |> unique() |> sort()
        
      benthic_posteriors_regions <-
        benthic_posteriors_regions |>
        left_join(stan_data, by = c("region", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "category")) |>
        mutate(name = paste(region, category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "Years", .scale = "region"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, category)

      # ----end
      benthic_posteriors_regions
    }
    ),
    tar_target(aggregate_region_plots_V2_, {
      benthic_posteriors <- aggregate_compile_V2_
      benthic_posteriors_regions_V2 <- aggregate_regions_V2_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      ## benthic_models <- fit_models_stan_predict_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_region_plots
      print("Regional plots")
      data_xgboost <- data_xgboost |>
        filter(!is.na(region), is.na(subregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(subregion, ecoregion) |>
        dplyr::select(region, category, stan_data) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(region) |> unique() |> sort()
      stan_models <- benthic_posteriors_regions_V2 |> pull(region) |> unique() |> sort()
        
      benthic_posteriors_regions_V2 <-
        benthic_posteriors_regions_V2 |>
        left_join(stan_data, by = c("region", "category")) |>
        left_join(data_xgboost,
                  by = c("region", "category")) |>
        mutate(name = paste(region, category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "cellmeans_years", .scale = "region"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(region, category)

      # ----end
      benthic_posteriors_regions_V2
    }
    ),
    tar_target(summary_region_, {
      benthic_posteriors_region <- aggregate_region_plots_
      summarise_info <- summarise_info_
      ## ---- summary region
      summary_region <- benthic_posteriors_region |> 
        summarise_info()
      # ----end
      summary_region
    }
    ),
    tar_target(summary_region_V2_, {
      benthic_posteriors_region_V2 <- aggregate_region_plots_V2_
      summarise_info <- summarise_info_
      ## ---- summary region
      summary_region_V2 <- benthic_posteriors_region_V2 |> 
        summarise_info()
      # ----end
      summary_region_V2
    }
    ),

    ## Global summary plots
    tar_target(aggregate_global_plots_, {
      benthic_posteriors <- aggregate_compile_
      benthic_posteriors_global <- aggregate_global_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_global_plots
      print("Global plots")
      data_xgboost <- data_xgboost |>
        filter(is.na(region), is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(region, subregion, ecoregion) |>
        dplyr::select(category, stan_data) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(category) |> unique() |> sort()
      stan_models <- benthic_posteriors_global |> pull(category) |> unique() |> sort()
        
      benthic_posteriors_global <-
        benthic_posteriors_global |>
        left_join(stan_data, by = c("category")) |>
        left_join(data_xgboost,
                  by = c("category")) |>
        mutate(name = paste(category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "Years", .scale = "global"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(category)

      # ----end
      benthic_posteriors_global
    }
    ),
    tar_target(aggregate_global_plots_V2_, {
      benthic_posteriors <- aggregate_compile_V2_
      benthic_posteriors_global_V2 <- aggregate_global_V2_
      data_path <- fit_models_global_parameters_$data_path
      output_path <- fit_models_global_parameters_$output_path
      wts <- process_spatial_weights_
      all_years <- get_all_years_
      interpolate_values <- interpolate_values_
      stan_partial_plot <- stan_partial_plot_
      data_xgboost <- xgboost_data_
      pdp_plots <- pdp_plots_
      ## ---- aggregate_global_plots V2
      print("Global plots")
      data_xgboost <- data_xgboost |>
        filter(is.na(region), is.na(subregion), is.na(ecoregion)) |>
        droplevels() |>
        mutate(across(c(mean, lower_ci_95, upper_ci_95), function(x) x/100)) |>
        group_by(region, category) |>
        nest(.key = "xgboost")
      ## combine the stan_data
      stan_data  <-
        benthic_posteriors |>
        ungroup(region, subregion, ecoregion) |>
        dplyr::select(category, stan_data) |>
        summarise(stan_data = list(
                    reduce(
                      map(.x = stan_data,
                          ~ .x[names(.x) %in% c("data_years", "all_years")]),
                      ~ map2(.x, .y, ~ sort(unique(c(.x, .y)))))))
      xgboost_models <- data_xgboost |> pull(category) |> unique() |> sort()
      stan_models <- benthic_posteriors_global_V2 |> pull(category) |> unique() |> sort()
        
      benthic_posteriors_global_V2 <-
        benthic_posteriors_global_V2 |>
        left_join(stan_data, by = c("category")) |>
        left_join(data_xgboost,
                  by = c("category")) |>
        mutate(name = paste(category, sep = "_")) |>
        mutate(plot = list(
          parallel::mcmapply(FUN = pdp_plots,
            cellmeans, dat = data.frame(a = 1), stan_data, name, xgboost,
            simple_means = NA,
            MoreArgs = list(xgboost_models = xgboost_models,
              stan_models = stan_models,
              type = "cellmeans_years", .scale = "global"),
            mc.cores = 40,
            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )) |>
        mutate(plot = plot[[1]]) |>
        group_by(category)

      # ----end
      benthic_posteriors_global_V2
    }
    ),
    tar_target(summary_global_, {
      benthic_posteriors_global <- aggregate_global_plots_
      summarise_info <- summarise_info_
      ## ---- summary global
      summary_global <- benthic_posteriors_global |> 
        summarise_info()
      # ----end
      summary_global
    }
    ),
    tar_target(summary_global_V2_, {
      benthic_posteriors_global_V2 <- aggregate_global_plots_V2_
      summarise_info <- summarise_info_
      ## ---- summary global V2
      summary_global_V2 <- benthic_posteriors_global_V2 |> 
        summarise_info()
      # ----end
      summary_global_V2
    }
    )

    
    )
}
