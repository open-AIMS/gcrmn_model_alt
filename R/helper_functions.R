helper_functions <- function() {
  targets <- list(
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
          year_conversions =  year_conversions
        )
        stan_data
      }
      ## ----end
      prepare_data_for_stan
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
                                    title, include_raw = FALSE, min_year = NULL) {
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
        }
        ## find gaps
        ## gap_yrs <- sample_yrs |>
        ##   reframe(Year = setdiff(full_seq(Year, period = 1), Year))
        gap_yrs <- sample_yrs |>
          reframe(Year = setdiff(all_yrs$Year, Year))
        if (nrow(gap_yrs) > 0) {
          gap_yrs <- gap_yrs |>
            reframe(Year = full_seq(range(Year) + c(-1, 1), period = 1))
          cellmeans_gap_yrs <- cellmeans |>
            right_join(gap_yrs, by = "Year") |>
            filter(!is.na(median))
          sample_yrs <- sample_yrs |>
            full_join(all_yrs, by = "Year")
        }
        if (nrow(gap_yrs) > 0) {
          alp <- 0.1
          cellmeans <- sample_yrs
        }

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
        ## make the plot

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
          geom_line(colour = "white", linewidth = 2) +
          scale_y_continuous(paste(ytitle, "cover (%)"),
                             labels =  function(x) x * 100) +
          theme_classic() 
        if (nrow(gap_yrs) > 0) {
          g1 <- g1 +
            geom_ribbon(data =  cellmeans_gap_yrs,
                        aes(x =  Year, ymin =  lower, ymax =  upper),
                        fill =  "orange", colour =  NA,
                        alpha =  0.3) +
            geom_line(data =  cellmeans_gap_yrs,
                      aes(x =  Year, y =  median),
                      colour =  "white",
                      linewidth =  2)
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
                      colour = "white", linewidth = 2) 
        }
        g1
      }
      stan_partial_plot
      ## ----end
    })

    )      
}

