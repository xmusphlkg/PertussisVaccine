
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

source('./Code/idm.R')

# appendix ----------------------------------------------------------------

# Visualize the beta function
beta_distribution <- function(i){
     # Using MCMC to simulate the model when some parameters are uncertain
     # k: vaccine decay rate: 4 - 12 years
     k <- 1/(rlnorm(1, log(8), 0.2)*365)
     # k_n: natural immunity decay rate: 4 - 20 years
     k_n <- 1/(rlnorm(1, log(12), 0.2)*365)
     # infectious_period: 2 - 6 weeks
     infectious_period <- rgamma(1, shape = 10, scale = 4*7/10)
     # latent_period: 4 - 21 days
     latent_period <- rgamma(1, shape = 8, scale = 12/10)
     # reproductive_number: 5.5 - 17
     reproductive_number <- max(rnorm(1, 10, 3), 5.5)
     
     # Estimate Beta
     gamma <- 1/infectious_period
     sigma <- 1/latent_period
     beta <- reproductive_number * gamma / (sigma / (sigma + gamma))
     
     # Seasonal variation in beta
     t_seq <- seq(0, 365*5, by = 1)
     beta_seq <- sapply(t_seq, get_seasonal_beta, base_beta = beta)
     
     return(data.frame(t = i, t_seq = t_seq, beta = beta_seq))
}

beta_seq <- lapply(1:1e3, beta_distribution)
beta_seq <- do.call(rbind, beta_seq)
beta_stats <- beta_seq |> 
     group_by(t_seq) |> 
     summarize(mean_beta = mean(beta),
               median_beta = median(beta),
               lower_beta = quantile(beta, 0.025),
               upper_beta = quantile(beta, 0.975))

fig1 <- ggplot(beta_stats, aes(x = t_seq/365)) +
     geom_line(data = beta_stats, aes(y = mean_beta), color = "red", linewidth = 1) +
     geom_ribbon(data = beta_stats, aes(ymin = lower_beta, ymax = upper_beta), fill = "red", alpha = 0.3) +
     scale_y_continuous(limits = c(0, NA),
                        expand = expansion(mult = c(0, 0.05))) +
     scale_x_continuous(limits = c(0, 5),
                        breaks = seq(0, 5, by = 1),
                        expand = c(0, 0)) +
     labs(title = "Seasonal Variation in Transmission Rate (Beta)",
          x = "Time (Years)",
          y = "Transmission Rate (Beta)") +
     theme_bw()

ggsave("./Outcome/S fig6_2.png",
       fig1,
       width = 6,
       height = 4,
       dpi = 300)

## visual test of the initial population
pop_list <- lapply(c("World", "China", "Indonesia", "Thailand", "Philippines", "Myanmar"), function(location) {
     set.seed(20250310)  # for reproducibility
     pop <- set_initial_population(1e6, location = location)
     
     pop_total <- read.csv("./Data/unpopulation_dataportal_20250309214006.csv") |> 
          filter(Location == location) |> 
          select(Age, Value) |>
          mutate(Frequency = Value/sum(Value),
                 Age = as.numeric(gsub("\\+", "", Age)))
     
     ggplot(pop, aes(x = age)) +
          geom_histogram(binwidth = 1, fill = "skyblue",
                         # using density instead of count to normalize the y-axis
                         aes(y = after_stat(density))) +
          geom_line(data = pop_total, aes(x = Age, y = Frequency), color = "red", linewidth = 1) +
          labs(title = paste("Initial Age Distribution (", location, ")", sep = ""), x = "Age", y = "Count") +
          theme_minimal()
})

fig <- wrap_plots(pop_list, ncol = 3)

ggsave("./Outcome/S fig6_1.png",
       fig,
       width = 12,
       height = 6,
       dpi = 300)

# run all countries -------------------------------------------------------

library(parallel)
library(doParallel)

# Prepare task function for parLapply
task_function <- function(task_row, country, tasks) {
     param_id <- tasks[task_row, 1]  # First column is param_id
     strategy_type <- as.character(tasks[task_row, 2])  # Second column is strategy
     params <- param_list[[param_id]]
     
     # Get country-specific strategies
     strategy_list <- switch(tolower(country),
                             "china" = strategy_china,
                             "indonesia" = strategy_indonesia,
                             "philippines" = strategy_philippines,
                             "myanmar" = strategy_myanmar,
                             "thailand" = strategy_thailand,
                             stop("Unknown country"))
     
     # Complete parameter set for this simulation
     full_params <- list(
          # Transmission rate
          beta = params$beta,
          # Latent period
          latent_period = params$latent_period,
          # Infectious period
          infectious_period = params$infectious_period,
          # Initial efficacy for primary vaccine dose
          VE0_primary = 0.8,
          # Initial efficacy for booster dose
          VE0_booster = 0.9,
          # Vaccine immunity decay rate
          k = params$k,
          # Maternal immunity decay rate
          delta_m = 1/(3*30),
          # Birth rate (per year per 1000 individuals)
          birth_rate = strategy_list$birth_rate,
          # Initial maternal immunity level
          M0 = 1,
          # Immunity level after recovery
          natural_immunity = 0.8,
          # Natural immunity decay rate
          k_n = params$k_n
     )
     
     # Select appropriate strategy
     current_strategy <- switch(strategy_type,
                                "A" = strategy_list$strategy_A,
                                "B" = strategy_list$strategy_B,
                                "C" = strategy_list$strategy_C)
     
     # Run single simulation
     result <- run_simulation(current_strategy, full_params, T_end, dt, N, location = country)
     
     # Add metadata to results
     result$summary$param_id <- param_id
     result$summary$strategy <- current_strategy$label
     result$summary$country <- country
     result$summary$sim_id <- params$sim_id
     
     # Return summary data
     return(result$summary)
}

run_parallel_simulation <- function(countries = c("China", "Indonesia", "Philippines", "Myanmar", "Thailand"),
                                    param_samples = 1000, T_end = 365*5, dt = 1, N = 1e6) {
     
     # Create task list for each parameter set and strategy combination
     tasks <- expand.grid(
          param_id = 1:param_samples,
          strategy = c("A", "B", "C")
     )
     
     # setting parallel computing
     n_cores <- detectCores()/2
     cl <- makeCluster(n_cores)
     registerDoParallel(cl)
     
     cat(sprintf("Using %d cores for parallel computing.\n", n_cores))
     
     # using MCMC to simulate the model when some parameters are uncertain
     # Set seed for reproducibility
     set.seed(20250310)
     
     # Generate parameter sets for all simulations
     param_list <- lapply(1:param_samples, function(i) {
          # Vaccine immunity decay rate: 4-12 years
          k <- 1/(rlnorm(1, log(8), 0.2)*365)
          # Natural immunity decay rate: 4-20 years
          k_n <- 1/(rlnorm(1, log(12), 0.2)*365)
          # Infectious period: 2-6 weeks
          infectious_period <- rgamma(1, shape = 10, scale = 4*7/10)
          # Latent period: 4-21 days
          latent_period <- rgamma(1, shape = 8, scale = 12/10)
          # Reproductive number: 5.5-17
          reproductive_number <- max(rnorm(1, 10, 3), 5.5)
          
          # Calculate Beta from basic parameters
          gamma <- 1/infectious_period
          sigma <- 1/latent_period
          beta <- reproductive_number * gamma / (sigma / (sigma + gamma))
          
          list(
               beta = beta,
               latent_period = latent_period,
               infectious_period = infectious_period,
               k = k,
               k_n = k_n,
               sim_id = i
          )
     })  
     
     # Load required packages on each cluster node
     clusterEvalQ(cl, {
          library(dplyr)
          library(ggplot2)
          source("./Code/strategy.R")  # Load strategy definitions
          source('./Code/idm.R') # Load model function
     })
     
     # Initialize results storage
     all_results <- list()
     
     # Run simulations for each country
     for(country in countries) {
          cat(sprintf("\nStarting %d simulations for %s...\n", param_samples, country))
          
          # Export necessary functions to the cluster
          clusterExport(cl, ls()[ls() != "cl"], envir = environment())
          
          # Run parallel simulations using parLapply
          results_list <- parLapply(cl, 1:nrow(tasks), task_function, country = country, tasks = tasks)
          
          # test
          # param_samples = 1000; T_end = 365*10; dt = 1; N = 1e6; country = 'China'
          # task_function(1, country, tasks)
          
          # Combine results from list into a data frame
          results <- do.call(rbind, results_list)
          
          # Store country results
          all_results[[country]] <- results
          
          # Save intermediate results
          saveRDS(results, file = sprintf("./Outcome/%s_simulation_results.rds", country))
          cat(sprintf("Completed simulations for %s and saved results.\n", country))
     }
     
     # Return combined results
     return(combined_results)
}

results <- run_parallel_simulation(countries = c("China", "Indonesia", "Philippines", "Myanmar", "Thailand"),
                                   param_samples = 1000)
