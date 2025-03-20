
library(mgcv)
library(ggplot2)
library(viridis)

# Based on the data: https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009098#sec010
load('./Data/contact_all.rdata')

# visual contact matrix
source('./Code/strategy.R')

# function ----------------------------------------------------------------

fit_contact_gam <- function(contact_matrix, group_size = 5, k = 30, method = "GCV.Cp") {
     # Convert the NxN matrix to a data frame:
     # - x and y will represent the midpoint ages for each 5-year group.
     # - z will be the contact rate from the matrix cell.
     N <- nrow(contact_matrix)
     age_centers <- seq(2.5, by = group_size, length.out = N)
     
     df <- data.frame(
          x = rep(age_centers, times = N),
          y = rep(age_centers, each  = N),
          z = as.vector(contact_matrix)
     )
     
     # Fit a 2D smooth GAM:
     # - k controls the number of basis functions for smoothing.
     # - method = "REML" is often recommended, but you can choose "GCV.Cp" or others.
     gam_model <- gam(z ~ s(x, y, k = k), data = df, method = method, gamma  = 0.8)
     return(gam_model)
}

smooth_and_aggregate_contact_matrix <- function(gam_model, 
                                                new_breaks, 
                                                step = 1, 
                                                max_age = 80) {
     # Create a fine grid of ages from 0 to max_age.
     ages <- seq(0, max_age, by = step)
     grid_data <- expand.grid(x = ages, y = ages)
     
     # Predict contact rates on this grid using the GAM model.
     pred_values <- predict(gam_model, newdata = grid_data, type = "response")
     pred_df <- data.frame(x = grid_data$x, y = grid_data$y, z = pred_values)
     
     # Determine how many new groups we'll have.
     M <- length(new_breaks) - 1
     agg_matrix <- matrix(0, nrow = M, ncol = M)
     count_matrix <- matrix(0, nrow = M, ncol = M)
     
     # Helper function to find which group index an age belongs to.
     find_group_index <- function(age_val) {
          # Identifies the interval i for which new_breaks[i] <= age_val < new_breaks[i+1].
          idx <- which(age_val >= new_breaks[-length(new_breaks)] & 
                            age_val < new_breaks[-1])
          if (length(idx) == 1) return(idx) else return(NA)
     }
     
     # Loop through each predicted point and assign it to the appropriate (i, j) cell.
     for (i in seq_len(nrow(pred_df))) {
          row_age <- pred_df$x[i]
          col_age <- pred_df$y[i]
          val     <- pred_df$z[i]
          
          row_g <- find_group_index(row_age)
          col_g <- find_group_index(col_age)
          
          if (!is.na(row_g) && !is.na(col_g)) {
               agg_matrix[row_g, col_g]   <- agg_matrix[row_g, col_g] + val
               count_matrix[row_g, col_g] <- count_matrix[row_g, col_g] + 1
          }
     }
     
     # Compute the average contact rate per cell.
     final_mat <- agg_matrix / count_matrix
     final_mat[is.nan(final_mat)] <- NA
     
     return(final_mat)
}

plot_contact_matrices <- function(original_matrix,
                                  gam_model,
                                  aggregated_mat,
                                  old_group_size = 5,
                                  new_breaks,
                                  step = 1,
                                  max_age = 80) {
     N <- nrow(original_matrix)
     original_centers <- seq(old_group_size / 2, by = old_group_size, length.out = N)
     df_orig <- data.frame(
          x = rep(original_centers, times = N),
          y = rep(original_centers, each = N),
          z = as.vector(original_matrix)
     )
     
     # Plot 1: Original matrix
     p1 <- ggplot(df_orig, aes(x = x, y = y, fill = z)) +
          geom_tile() +
          coord_fixed() +
          scale_fill_viridis_c(option = "magma", direction = -1) +
          theme_minimal() +
          labs(
               title = "Original Contact Matrix",
               x = "Age (years)",
               y = "Age (years)",
               fill = "Contact"
          )
     
     # 2) Create a data frame for the smoothed surface by predicting on a fine grid.
     df_smooth <- expand.grid(
          x = seq(0, max_age, by = step),
          y = seq(0, max_age, by = step)
     )
     df_smooth$z <- predict(gam_model, newdata = df_smooth, type = "response")
     
     # Plot 2: Smoothed matrix
     p2 <- ggplot(df_smooth, aes(x = x, y = y, fill = z)) +
          geom_tile() +
          coord_fixed() +
          scale_fill_viridis_c(option = "magma", direction = -1) +
          theme_minimal() +
          labs(
               title = "Smoothed (GAM) Surface",
               x = "Age (years)",
               y = "Age (years)",
               fill = "Contact"
          )
     
     # 3) Create a data frame for the aggregated matrix.
     #    We assume aggregated_mat is MxM with breakpoints given by new_breaks.
     #    The 'center' of each age interval i is (new_breaks[i] + new_breaks[i+1]) / 2.
     M <- nrow(aggregated_mat)
     df_agg_rect <- expand.grid(row_i = seq_len(M),
                                col_j = seq_len(M))
     
     df_agg_rect$xmin <- new_breaks[df_agg_rect$col_j]
     df_agg_rect$xmax <- new_breaks[df_agg_rect$col_j + 1]
     df_agg_rect$ymin <- new_breaks[df_agg_rect$row_i]
     df_agg_rect$ymax <- new_breaks[df_agg_rect$row_i + 1]
     
     df_agg_rect$z <- mapply(function(i, j) aggregated_mat[i, j],
                             df_agg_rect$row_i, df_agg_rect$col_j)
     
     # Plot 3: Aggregated matrix
     p3 <- ggplot(df_agg_rect, aes(xmin = xmin, xmax = xmax,
                                   ymin = ymin, ymax = ymax, fill = z)) +
          geom_rect(color = NA) +
          coord_fixed() +
          scale_fill_viridis_c(option = "magma", direction = -1) +
          theme_minimal() +
          labs(
               title = "Aggregated Matrix (New Groups)",
               x = "Age (years)",
               y = "Age (years)",
               fill = "Contact"
          )
     
     # Return the three ggplot objects in a list
     return(cowplot::plot_grid(p1, p2, p3, nrow = 1))
}

# China -------------------------------------------------------------------

mr_china <- contact_all$CHN
ages <- c(0,
          strategy_china$strategy_A$primary_schedule,
          strategy_china$strategy_B$primary_schedule,
          strategy_china$strategy_C$primary_schedule,
          strategy_china$strategy_A$booster_schedule,
          strategy_china$strategy_B$booster_schedule,
          strategy_china$strategy_C$booster_schedule,
          20, 40, 60, 80) |> 
     unique() |>
     sort()

model <- fit_contact_gam(mr_china, group_size = 5)

mr_china_resampled <- smooth_and_aggregate_contact_matrix(model, ages, 0.1, 80)

plots <- plot_contact_matrices(
  original_matrix = mr_china,
  gam_model       = model,
  aggregated_mat  = mr_china_resampled,
  old_group_size  = 5,
  new_breaks      = ages,
  step            = 0.1,
  max_age         = 80
)

ggsave('./Outcome/S fig6_3_china.png',
       plots, width = 12, height = 4, dpi = 300)

# Thailand ----------------------------------------------------------------
mr_thailand <- contact_all$THA

ages <- c(0,
          strategy_thailand$strategy_A$primary_schedule,
          strategy_thailand$strategy_B$primary_schedule,
          strategy_thailand$strategy_C$primary_schedule,
          strategy_thailand$strategy_A$booster_schedule,
          strategy_thailand$strategy_B$booster_schedule,
          strategy_thailand$strategy_C$booster_schedule,
          20, 40, 60, 80) |> 
     unique() |>
     sort()

model <- fit_contact_gam(mr_thailand, group_size = 5)

mr_thailand_resampled <- smooth_and_aggregate_contact_matrix(model, ages, 0.1, 80)

plots <- plot_contact_matrices(
  original_matrix = mr_thailand,
  gam_model       = model,
  aggregated_mat  = mr_thailand_resampled,
  old_group_size  = 5,
  new_breaks      = ages,
  step            = 0.1,
  max_age         = 80
)

ggsave('./Outcome/S fig6_3_thailand.png',
       plots, width = 12, height = 4, dpi = 300)

# Indonesia ----------------------------------------------------------------
mr_indonesia <- contact_all$IDN

ages <- c(0,
          strategy_indonesia$strategy_A$primary_schedule,
          strategy_indonesia$strategy_B$primary_schedule,
          strategy_indonesia$strategy_C$primary_schedule,
          strategy_indonesia$strategy_A$booster_schedule,
          strategy_indonesia$strategy_B$booster_schedule,
          strategy_indonesia$strategy_C$booster_schedule,
          20, 40, 60, 80) |> 
     unique() |>
     sort()

model <- fit_contact_gam(mr_indonesia, group_size = 5)

mr_indonesia_resampled <- smooth_and_aggregate_contact_matrix(model, ages, 0.1, 80)

plots <- plot_contact_matrices(
  original_matrix = mr_indonesia,
  gam_model       = model,
  aggregated_mat  = mr_indonesia_resampled,
  old_group_size  = 5,
  new_breaks      = ages,
  step            = 0.1,
  max_age         = 80
)

ggsave('./Outcome/S fig6_3_indonesia.png',
       plots, width = 12, height = 4, dpi = 300)

# Philippines -------------------------------------------------------------
mr_philippines <- contact_all$PHL

ages <- c(0,
          strategy_philippines$strategy_A$primary_schedule,
          strategy_philippines$strategy_B$primary_schedule,
          strategy_philippines$strategy_C$primary_schedule,
          strategy_philippines$strategy_A$booster_schedule,
          strategy_philippines$strategy_B$booster_schedule,
          strategy_philippines$strategy_C$booster_schedule,
          20, 40, 60, 80) |> 
     unique() |>
     sort()

model <- fit_contact_gam(mr_philippines, group_size = 5)

mr_philippines_resampled <- smooth_and_aggregate_contact_matrix(model, ages, 0.01, 80)

plots <- plot_contact_matrices(
  original_matrix = mr_philippines,
  gam_model       = model,
  aggregated_mat  = mr_philippines_resampled,
  old_group_size  = 5,
  new_breaks      = ages,
  step            = 0.1,
  max_age         = 80
)

ggsave('./Outcome/S fig6_3_philippines.png',
       plots, width = 12, height = 4, dpi = 300)

# Myanmar -----------------------------------------------------------------
mr_myanmar <- contact_all$MMR

ages <- c(0,
          strategy_myanmar$strategy_A$primary_schedule,
          strategy_myanmar$strategy_B$primary_schedule,
          strategy_myanmar$strategy_C$primary_schedule,
          strategy_myanmar$strategy_A$booster_schedule,
          strategy_myanmar$strategy_B$booster_schedule,
          strategy_myanmar$strategy_C$booster_schedule,
          20, 40, 60, 80) |> 
     unique() |>
     sort()

model <- fit_contact_gam(mr_myanmar, group_size = 5)

mr_myanmar_resampled <- smooth_and_aggregate_contact_matrix(model, ages, 0.1, 80)

plots <- plot_contact_matrices(
  original_matrix = mr_myanmar,
  gam_model       = model,
  aggregated_mat  = mr_myanmar_resampled,
  old_group_size  = 5,
  new_breaks      = ages,
  step            = 0.1,
  max_age         = 80
)

ggsave('./Outcome/S fig6_3_myanmar.png',
       plots, width = 12, height = 4, dpi = 300)

save(mr_china = mr_china_resampled,
     mr_thailand = mr_thailand_resampled,
     mr_indonesia = mr_indonesia_resampled,
     mr_philippines = mr_philippines_resampled,
     mr_myanmar = mr_myanmar_resampled,
     file = './Outcome/contact_resampled.rdata')
