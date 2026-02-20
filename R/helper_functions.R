helper_functions <- function() {
  targets <- list(
    tar_target(global_parameters_, {
      ## ---- global parameters
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

    tar_target(interpolate_values_, {
      ## ---- interpolate values function
      interpolate_values <- function(df, year, col) {
        if (year != min(df$Year)) {
          before <- df[df$Year == year - 1, col][[1]]
          during <- df[df$Year == year, col][[1]]
          after <- df[df$Year == year + 1, col][[1]]
          lower <- before + (during - before) * 0.9
          upper <- during + (after - during) * 0.1
        } else if (year == min(df$Year)) {
          during <- df[df$Year == year, col][[1]]
          after <- df[df$Year == year + 1, col][[1]]
          lower <- during - (after - during) * 0.1
          upper <- during + (after - during) * 0.1
        }
        return(c(lower, upper))
      }
      ## ----end
     interpolate_values 
    }),
    tar_target(stan_partial_plot_, {
      interpolate_values <- interpolate_values_
      ## ---- stan partial plot function
      stan_partial_plot <- function(cellmeans, stan_data, data, ytitle,
                                    title, include_raw = FALSE,
                                    min_year = NULL, max_year = NULL) {
        all_yrs <- cellmeans |>
          reframe(Year =  full_seq(Year, period =  1))
        sampled <- data.frame(Year = cellmeans$Year[stan_data$data_years])
        sample_yrs <- cellmeans |>
          right_join(sampled, by = "Year")
        ## find silo years (including first and last years)
        silo_yrs <- sampled |>
          mutate(missing_before =
                   ifelse(Year - lag(Year) > 1, TRUE,
                   ifelse(is.na(lag(Year)), FALSE, FALSE)),
                 missing_after =
                   ifelse(lead(Year) - Year > 1, TRUE,
                   ifelse(is.na(lead(Year)), FALSE, FALSE))
                 ) |>
          filter(
          (missing_before | is.na(missing_before)) &
          missing_after 
          ) |>
          dplyr::select(Year)
        if (nrow(silo_yrs) > 0) {
          cellmeans_silo_yrs <- cellmeans |>
            ungroup() |> 
            right_join(silo_yrs, by =  "Year") |>
            mutate(grp = 1:n()) |>
            group_by(grp) |> 
            reframe(
              median =  interpolate_values(cellmeans, Year, "median"),
              lower =  interpolate_values(cellmeans, Year, "lower"),
              upper =  interpolate_values(cellmeans, Year, "upper"),
              lower_80 =  interpolate_values(cellmeans, Year, "lower_80"),
              upper_80 =  interpolate_values(cellmeans, Year, "upper_80"),
              Year =  c(Year - 0.1, Year + 0.1)
            ) |>
            ungroup()
          if (!is.null(min_year)) {
            min_year <- min(min_year, cellmeans_silo_yrs$Year)
          }
          if (!is.null(max_year)) {
            max_year <- max(max_year, cellmeans_silo_yrs$Year)
          }
        }
        ## find gaps
        gap_yrs <- setdiff(all_yrs, sampled) |> group_by(Year)
        if (nrow(gap_yrs) > 0) {
          gap_yrss <- gap_yrs |>
            reframe(Year = full_seq(range(Year) + c(-1, 1), period = 1)) |>
            as.data.frame() |>
            pull(Year) |>
            unique()
          cellmeans_gap_yrs <- cellmeans |>
            mutate(across(c(median, lower, upper, lower_80, upper_80),
              ~ ifelse(!Year %in% gap_yrss, NA, .x)))

          data_yrss <- sampled |>
            pull(Year) |>
            unique()
          cellmeans <- cellmeans |>
            mutate(across(c(median, lower, upper, lower_80, upper_80),
              ~ ifelse(!Year %in% data_yrss, NA, .x)))
        }
        ## gap_yrs <- sample_yrs |>
        ##   reframe(Year = setdiff(full_seq(Year, period = 1), Year))
        ## gap_yrs <- sample_yrs |>
        ##   reframe(Year = setdiff(all_yrs$Year, Year))
        ## if (nrow(gap_yrs) > 0) {
        ##   gap_yrs <- gap_yrs |>
        ##     reframe(Year = full_seq(range(Year) + c(-1, 1), period = 1))
        ##   cellmeans_gap_yrs <- cellmeans |>
        ##     right_join(gap_yrs, by = "Year") |>
        ##     filter(!is.na(median))
        ##   sample_yrs <- sample_yrs |>
        ##     full_join(all_yrs, by = "Year")
        ## }
        ## if (nrow(gap_yrs) > 0) {
        ##   alp <- 0.1
        ##   cellmeans <- sample_yrs
        ## }

        ## trim with min_years (if available)
        if (!is.null(min_year)) {
          cellmeans <- cellmeans |>
            filter(Year >= min_year) |>
            droplevels()
          if (nrow(gap_yrs) > 0) {
            cellmeans_gap_yrs <- cellmeans_gap_yrs |>
              filter(Year >= min_year) |>
              droplevels()
          }
          if (nrow(silo_yrs) > 0) {
            cellmeans_silo_yrs <- cellmeans_silo_yrs |> 
              filter(Year >= min_year) |>
              droplevels()
          }
        }

        ## trim with max_years (if available)
        if (!is.null(max_year)) {
          if (include_raw) data <- data |> filter(Year <= max_year) |> droplevels()
          cellmeans <- cellmeans |>
            filter(Year <= max_year) |>
            droplevels()
          if (nrow(gap_yrs) > 0) {
            cellmeans_gap_yrs <- cellmeans_gap_yrs |>
              filter(Year <= max_year) |>
              droplevels()
          }
          if (nrow(silo_yrs) > 0) {
            cellmeans_silo_yrs <- cellmeans_silo_yrs |> 
              filter(Year <= max_year) |>
              droplevels()
          }
        }

        
        ## make the plot
        minor_breaks <- function(lims) {
          seq(floor(lims[1]), ceiling(lims[2]), by = 1)
        }
        g1 <-
          cellmeans |>
          ggplot(aes(y = median, x = Year)) +
          ggtitle(title)
        if (include_raw) {
          g1 <- g1 + 
            geom_point(data = data, aes(y = value),
                       color = "black", alpha = 0.3) +
            geom_line(data =  data, aes(y = value, group = Transect),
                      color = "black", alpha = 0.2) 
        }
        g1 <- g1 +
          geom_ribbon(aes(ymin = lower, ymax = upper),
                      fill = "orange", alpha = 0.5, color = NA) +
          geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                      fill = "orange", alpha = 0.7, color = NA) +
          geom_line(colour = "orange", linewidth = 2) +
          geom_line(colour = "white", linewidth = 1.5) +
          scale_y_continuous(paste(ytitle, "cover (%)"),
            labels =  function(x) x * 100
          ) +
          scale_x_continuous(
            minor_breaks = minor_breaks,
            guide = guide_axis(minor.ticks = TRUE)) +
          theme_classic() 
        if (nrow(gap_yrs) > 0) {
          g1 <- g1 +
            geom_ribbon(data =  cellmeans_gap_yrs,
                        aes(x =  Year, ymin =  lower, ymax =  upper),
                        fill =  "grey", colour =  NA,
                        alpha =  0.2) +
            geom_ribbon(data =  cellmeans_gap_yrs,
                        aes(x =  Year, ymin =  lower_80, ymax =  upper_80),
                        fill =  "grey", colour =  NA,
                        alpha =  0.3) +
            geom_line(data =  cellmeans_gap_yrs,
                      aes(x =  Year, y =  median),
                      colour =  "grey",
                      linewidth =  2) +
            geom_line(data =  cellmeans_gap_yrs,
                      aes(x =  Year, y =  median),
                      colour =  "white",
                      linewidth =  1.5)
        }
        if (nrow(silo_yrs) > 0) {
          g1 <- g1 +
            geom_ribbon(data = cellmeans_silo_yrs,
                        aes(ymin = lower, ymax = upper, group = grp),
                        fill = "orange", alpha = 0.5, color = NA) +
            geom_ribbon(data = cellmeans_silo_yrs,
                        aes(ymin = lower_80, ymax = upper_80, group = grp),
                        fill = "orange", alpha = 0.7, color = NA) +
            geom_line(data = cellmeans_silo_yrs, aes(group = grp),
                      colour = "orange", linewidth = 2) +
            geom_line(data = cellmeans_silo_yrs, aes(group = grp),
                      colour = "white", linewidth = 1.5) 
        }
        g1
      }
      stan_partial_plot
      ## ----end
    }),
    tar_target(partial_plots_, {
      data_path <- global_parameters_$data_path
      stan_partial_plot <- stan_partial_plot_
      output_path <- global_parameters_$output_path
      ## ---- partial_plots function
      partial_plots <- function(.x, dat, stan_data, name, type = "cellmeans_years") {
        cellmeans <- readRDS(.x)
        title <- str_replace_all(name, "_", " ")
        ytitle <- str_replace(name, ".*_(.*)", "\\1")
        ## plot without raw points
        if (type == "cellmeans_years") {
          nm <- paste0(output_path, "figures/pdp/pdp_V2_", "_", name, ".png")
          nm2 <- paste0(output_path, "figures/pdp/pdpraw_V2_", "_", name, ".png")
        } else {
          nm <- paste0(output_path, "figures/pdp/pdp_", "_", name, ".png")
          nm2 <- paste0(output_path, "figures/pdp/pdpraw_", "_", name, ".png")
        }
        ## without raw data
        g1 <- stan_partial_plot(cellmeans, stan_data,
          data = NULL,
          title = title,
          ytitle = ytitle,
          include_raw = FALSE)
        ggsave(
          filename = nm,
          g1,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()
        ## with raw data
        g2 <- stan_partial_plot(cellmeans, stan_data,
          data = dat,
          title = title,
          ytitle = ytitle,
          include_raw = TRUE)
        ggsave(
          filename = nm2,
          g2,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()
        sig <- digest::digest(g1)
        list(path = nm, signature = sig)
      }
      ## ----end
      partial_plots
    }),
    tar_target(stan_partial_plot_algaes_, {
      interpolate_values <- interpolate_values_
      ## ---- stan partial plot algaes function
      stan_partial_plot_algaes <- function(cellmeans, stan_data,
                                           data, ytitle,
                                           title, include_raw = FALSE,
                                           min_year = NULL, max_year = NULL, include_ggplot = TRUE,
                                           ribbon_fill = "orange", g1 = NULL) {
        all_yrs <- cellmeans |>
          reframe(Year =  full_seq(Year, period =  1))
        sampled <- data.frame(Year = cellmeans$Year[stan_data$data_years])
        sample_yrs <- cellmeans |>
          right_join(sampled, by = "Year")
        ## find silo years (including first and last years)
        silo_yrs <- sampled |>
          mutate(missing_before =
                   ifelse(Year - lag(Year) > 1, TRUE,
                     ifelse(is.na(lag(Year)), FALSE, FALSE)),
            missing_after =
              ifelse(lead(Year) - Year > 1, TRUE,
                ifelse(is.na(lead(Year)), FALSE, FALSE))
          ) |>
          filter(
          (missing_before | is.na(missing_before)) &
            missing_after 
          ) |>
          dplyr::select(Year)
        if (nrow(silo_yrs) > 0) {
          cellmeans_silo_yrs <- cellmeans |>
            ungroup() |> 
            right_join(silo_yrs, by =  "Year") |>
            mutate(grp = 1:n()) |>
            group_by(grp) |> 
            reframe(
              median =  interpolate_values(cellmeans, Year, "median"),
              lower =  interpolate_values(cellmeans, Year, "lower"),
              upper =  interpolate_values(cellmeans, Year, "upper"),
              lower_80 =  interpolate_values(cellmeans, Year, "lower_80"),
              upper_80 =  interpolate_values(cellmeans, Year, "upper_80"),
              Year =  c(Year - 0.1, Year + 0.1)
            ) |>
            ungroup()
          if (!is.null(min_year)) {
            min_year <- min(min_year, cellmeans_silo_yrs$Year)
          }
          if (!is.null(max_year)) {
            max_year <- max(max_year, cellmeans_silo_yrs$Year)
          }
        }
        ## find gaps
        gap_yrs <- setdiff(all_yrs, sampled) |> group_by(Year)
        if (nrow(gap_yrs) > 0) {
          gap_yrss <- gap_yrs |>
            reframe(Year = full_seq(range(Year) + c(-1, 1), period = 1)) |>
            as.data.frame() |>
            pull(Year) |>
            unique()
          cellmeans_gap_yrs <- cellmeans |>
            mutate(across(c(median, lower, upper, lower_80, upper_80),
              ~ ifelse(!Year %in% gap_yrss, NA, .x)))

          data_yrss <- sampled |>
            pull(Year) |>
            unique()
          cellmeans <- cellmeans |>
            mutate(across(c(median, lower, upper, lower_80, upper_80),
              ~ ifelse(!Year %in% data_yrss, NA, .x)))
        }
        ## gap_yrs <- sample_yrs |>
        ##   reframe(Year = setdiff(full_seq(Year, period = 1), Year))
        ## gap_yrs <- sample_yrs |>
        ##   reframe(Year = setdiff(all_yrs$Year, Year))
        ## if (nrow(gap_yrs) > 0) {
        ##   gap_yrs <- gap_yrs |>
        ##     reframe(Year = full_seq(range(Year) + c(-1, 1), period = 1))
        ##   cellmeans_gap_yrs <- cellmeans |>
        ##     right_join(gap_yrs, by = "Year") |>
        ##     filter(!is.na(median))
        ##   sample_yrs <- sample_yrs |>
        ##     full_join(all_yrs, by = "Year")
        ## }
        ## if (nrow(gap_yrs) > 0) {
        ##   alp <- 0.1
        ##   cellmeans <- sample_yrs
        ## }

        ## trim with min_years (if available)
        if (!is.null(min_year)) {
          cellmeans <- cellmeans |>
            filter(Year >= min_year) |>
            droplevels()
          if (nrow(gap_yrs) > 0) {
            cellmeans_gap_yrs <- cellmeans_gap_yrs |>
              filter(Year >= min_year) |>
              droplevels()
          }
          if (nrow(silo_yrs) > 0) {
            cellmeans_silo_yrs <- cellmeans_silo_yrs |> 
              filter(Year >= min_year) |>
              droplevels()
          }
        }

        ## trim with max_years (if available)
        if (!is.null(max_year)) {
          if (include_raw) data <- data |> filter(Year <= max_year) |> droplevels()
          cellmeans <- cellmeans |>
            filter(Year <= max_year) |>
            droplevels()
          if (nrow(gap_yrs) > 0) {
            cellmeans_gap_yrs <- cellmeans_gap_yrs |>
              filter(Year <= max_year) |>
              droplevels()
          }
          if (nrow(silo_yrs) > 0) {
            cellmeans_silo_yrs <- cellmeans_silo_yrs |> 
              filter(Year <= max_year) |>
              droplevels()
          }
        }

        ## make the plot
        if (include_ggplot) {
          g1 <-
            cellmeans |>
            ggplot(aes(y = median, x = Year)) +
            ggtitle(title)
        }
        if (include_raw) {
          g1 <- g1 + 
            geom_point(data = data, aes(y = value),
              color = "black", alpha = 0.3) +
            geom_line(data =  data, aes(y = value, group = Transect),
              color = "black", alpha = 0.2) 
        }
        g1 <- g1 +
          geom_ribbon(data = cellmeans, aes(ymin = lower, ymax = upper),
            fill = ribbon_fill, alpha = 0.5, color = NA) +
          geom_ribbon(data = cellmeans, aes(ymin = lower_80, ymax = upper_80),
            fill = ribbon_fill, alpha = 0.7, color = NA) +
          geom_line(data = cellmeans, colour = ribbon_fill, linewidth = 2) +
          geom_line(data = cellmeans, colour = "white", linewidth = 1.5) +
          scale_y_continuous(paste(ytitle, "cover (%)"),
            labels =  function(x) x * 100) +
          theme_classic() 
        if (nrow(gap_yrs) > 0) {
          g1 <- g1 +
            geom_ribbon(data =  cellmeans_gap_yrs,
              aes(x =  Year, ymin =  lower, ymax =  upper),
              fill =  "grey", colour =  NA,
              alpha =  0.2) +
            geom_ribbon(data =  cellmeans_gap_yrs,
              aes(x =  Year, ymin =  lower_80, ymax =  upper_80),
              fill =  "grey", colour =  NA,
              alpha =  0.3) +
            geom_line(data =  cellmeans_gap_yrs,
              aes(x =  Year, y =  median),
              colour =  "grey",
              linewidth =  2) +
            geom_line(data =  cellmeans_gap_yrs,
              aes(x =  Year, y =  median),
              colour =  "white",
              linewidth =  1.5)
        }
        if (nrow(silo_yrs) > 0) {
          g1 <- g1 +
            geom_ribbon(data = cellmeans_silo_yrs,
              aes(ymin = lower, ymax = upper, group = grp),
              fill = ribbon_fill, alpha = 0.5, color = NA) +
            geom_ribbon(data = cellmeans_silo_yrs,
              aes(ymin = lower_80, ymax = upper_80, group = grp),
              fill = ribbon_fill, alpha = 0.7, color = NA) +
            geom_line(data = cellmeans_silo_yrs, aes(group = grp),
              colour = ribbon_fill, linewidth = 2) +
            geom_line(data = cellmeans_silo_yrs, aes(group = grp),
              colour = "white", linewidth = 1.5) 
        }
        g1
      }
      ## ----end
      stan_partial_plot_algaes 
    }),
    tar_target(pdp_plots_algaes_, {
      data_path <- global_parameters_$data_path
      stan_partial_plot_algaes <- stan_partial_plot_algaes_
      output_path <- global_parameters_$output_path
      ## ---- pdp_plots_algaes function
      pdp_plots_algaes <- function(cellmeans, dat, stan_data, name,
                                   simple_means =  NA,
                                   stan_models = NULL,
                                   type = "cellmeans_years", .scale = "ecoregion",
                                   final_year = 2024) {
        ## xgboost_models <- more_args[1]
        ## stan_models <- more_args[2]
        ## type <- more_args[3]
        ## .scale <- more_args[4]
        title <- str_replace_all(name, "_", " ")
        ytitle <- str_replace(name, ".*_(.*)", "\\1")
        figure_path <- paste0(output_path, "figures/", .scale, "/")
        cellmeans_ma <- cellmeans |> filter(category == "Macroalgae") 
        cellmeans_ta <- cellmeans |> filter(category == "Turf algae") 
        ## Start with Macroalgae
        stan_data_ma <- stan_data |> filter(category == "Macroalgae") |>
          _[["stan_data"]]
        first_year_ma <- stan_data_ma$all_years[stan_data_ma$data_years[1]]
        first_year_ta <- first_year_ma
        if (nrow(cellmeans_ta) > 0) {
          stan_data_ta <- stan_data |> filter(category == "Turf algae") |>
            _[["stan_data"]]
          first_year_ta <- stan_data_ta$all_years[stan_data_ta$data_years[1]]
        }
        first_year <- min(first_year_ma, first_year_ta)
        
        if (type == "cellmeans_years") {
          ## (g0) Bayes only, full year range, no data, no simple means
          nm0 <- paste0(figure_path, .scale, "_pdp_bayes_V2_", "_", name, ".png")
          ## (g0a) Bayes, trimmed year range, no data, no simple means
          nm0a <- paste0(figure_path, .scale, "_pdp_bayes_a_V2_", "_", name, ".png")
        } else {
          ## (g0) Bayes only, full year range, no data, no simple means
          nm0 <- paste0(figure_path, .scale, "_pdp_bayes_", "_", name, ".png")
          ## (g0a) Bayes, trimmed year range, no data, no simple means
          nm0a <- paste0(figure_path, .scale, "_pdp_bayes_a_", "_", name, ".png")
        }
        ## cellmeans_ma <- cellmeans |> filter(category == "Macroalgae") 
        ## cellmeans_ta <- cellmeans |> filter(category == "Turf algae") 
        ## plot without raw points
        g1 <- stan_partial_plot_algaes(cellmeans_ma, 
          stan_data_ma, 
          data = NULL,
          title = title,
          ytitle = ytitle,
          include_raw = FALSE, ribbon_fill = "green", g1 = NULL)
        ## g1 <- stan_partial_plot_algaes(cellmeans_ta, 
        ##   stan_data_ta, 
        ##   data = NULL,
        ##   title = title,
        ##   ytitle = ytitle,
        ##   include_raw = FALSE, ribbon_fill = "green", g1 = NULL)
        if (nrow(cellmeans_ta)>0) {
          g1_A <- stan_partial_plot_algaes(cellmeans_ta, stan_data_ta,
            data = NULL,
            title = title,
            ytitle = ytitle,
            include_raw = FALSE, include_ggplot = FALSE, ribbon_fill = "blue", g1 = g1)
          g1 <- g1_A
        }
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
        ggsave(
          filename = str_replace(nm0, "png", "_hr.png"),
          g1,
          width =  6, height =  4, dpi =  500
        ) |> suppressWarnings() |> suppressMessages()

        g0 <- g1
        
        ## Now a version trimmed to minium data year
        g1a <- stan_partial_plot_algaes(
          cellmeans_ma,
          stan_data_ma,
          data = NULL,
          title = title,
          ytitle = ytitle,
          include_raw = FALSE,
          ## min_year = min(dat$Year))
          min_year = first_year,
          max_year = final_year,
          include_ggplot = TRUE, ribbon_fill = "green", g1 = NULL)
        if (nrow(cellmeans_ta)>0) {
          g1a_A <- stan_partial_plot_algaes(
            cellmeans_ta,
            stan_data_ta,
            data = NULL,
            title = title,
            ytitle = ytitle,
            include_raw = FALSE,
            ## min_year = min(dat$Year))
            min_year = first_year,
            max_year = final_year,
            include_ggplot = FALSE, ribbon_fill = "blue", g1 = g1a)
          g1a <- g1a_A
        }

        ## Bayes, trimmed year range, no data, no simple means
        ggsave(
          filename = nm0a,
          g1a,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()
        ggsave(
          filename = str_replace(nm0a, "png", "_hr.png"),
          g1a,
          width =  6, height =  4, dpi =  500
        ) |> suppressWarnings() |> suppressMessages()

        nm0
      }
      ## ----end
      pdp_plots_algaes
    }),
    tar_target(raw_plots_, {
      data_path <- global_parameters_$data_path
      stan_partial_plot <- stan_partial_plot_
      output_path <- global_parameters_$output_path
      ## ---- raw_plots function
      raw_plots <- function(dat, .name, simple_means) {
        nm <- paste0(output_path, "figures/raw/", "raw_pdp_", "_", .name, ".png")
        nm2 <- paste0(output_path, "figures/raw/", "raw_pdp_simple_", "_", .name, ".png")
        nm3 <- paste0(output_path, "figures/raw/", "raw_pdp_datasetID_", "_", .name, ".png")
        nm4 <- paste0(output_path, "figures/raw/", "raw_pdp_grid_id_", "_", .name, ".png")
        g1 <- dat |> ggplot(aes(x = Year, y = value)) +
          geom_point(color = "black", alpha = 0.3) +
          geom_line(aes(group = Transect),
            color = "black", alpha = 0.2) +
          theme_bw()
        ggsave(
          filename = nm,
          g1,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        ## Now with simple means overlayed
        ## mean is salmon, median is teal 
        simple_means <- simple_means |>
          mutate(across(c(mean, median, smean, smedian), \(x) x/100))
        g2 <- g1 +
          geom_point(data = simple_means,
            aes(y = mean, x = as.numeric(as.character(cYear)),
              color = "hier. mean")) +
          geom_line(data = simple_means,
            aes(y = mean, x = as.numeric(as.character(cYear)),
              color = "hier. mean"), show.legend = FALSE) +
          geom_point(data = simple_means,
            aes(y = smean, x = as.numeric(as.character(cYear)),
              color = "simple mean")) +
          geom_line(data = simple_means,
            aes(y = smean, x = as.numeric(as.character(cYear)),
              color = "simple mean"), show.legend = FALSE) +
          geom_point(data = simple_means,
            aes(y = median, x = as.numeric(as.character(cYear)),
              color = "hier. median")) +
          geom_line(data = simple_means,
            aes(y = median, x = as.numeric(as.character(cYear)),
              color = "hier. median"), show.legend = FALSE) +
          geom_point(data = simple_means,
            aes(y = smedian, x = as.numeric(as.character(cYear)),
              color = "simple median")) +
          geom_line(data = simple_means,
            aes(y = smedian, x = as.numeric(as.character(cYear)),
              color = "simple median"), show.legend = FALSE) +
          theme_bw()
        ggsave(
          filename = nm2,
          g2,
          width =  6, height =  4, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        ## Now facetted on datasetID
        g3 <- g1 +
          geom_smooth(color = "red", fill = "red", alpha = 0.3) +
          facet_wrap(~datasetID)
        ggsave(
          filename = nm3,
          g3,
          width =  12, height =  12, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()

        ## Now facetted on grid_id
        g4 <- g1 +
          geom_smooth(color = "red", fill = "red", alpha = 0.3) +
          facet_wrap(~grid_id)
        ggsave(
          filename = nm4,
          g4,
          width =  12, height =  12, dpi =  72
        ) |> suppressWarnings() |> suppressMessages()
        nm
      }
      ## ----end
      raw_plots
    })
    
    
    )      
}

