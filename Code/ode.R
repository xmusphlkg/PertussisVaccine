
library(deSolve)
library(tidyverse)

source("./Code/strategy.R")

load('./Outcome/contact_resampled.rdata')

# Assign realistic initial ages based on real population data
set_initial_population <- function(N, ages, location = 'World') {
     # read datafile
     df <- read.csv('./Data/unpopulation_dataportal_20250309214006.csv') |> 
          filter(Location == location) |> 
          select(Age, Value)
     
     # Handle the "100+" age category
     df$Age <- as.character(df$Age)
     df$Age[df$Age == "100+"] <- "100"
     df$Age <- as.numeric(df$Age)
     
     # Ensure all ages have values and are properly formatted
     if(any(is.na(df$Age))) {
          warning("Some age values could not be converted to numeric. They will be removed.")
          df <- df[!is.na(df$Age),]
     }
     
     # Normalize the values to use as probabilities
     df$Value <- df$Value / sum(df$Value)
     
     # Sample ages based on the population distribution
     #
     set.seed(20250309)
     initial_ages <- sample(df$Age, size = N, replace = TRUE, prob = df$Value)
     
     # Add noise (0-0.99) to the ages to avoid ties
     initial_ages <- initial_ages + runif(N)
     
     # return group of ages
     group_ages <- cut(initial_ages, breaks = ages)
     group_ages <- table(group_ages)
     
     return(group_ages)
}

# Seasonal variation in transmission rate
get_seasonal_beta <- function(base_beta, t, amplitude = 0.6, pperiod = 365) {
     period <- 365  # 1 year
     amplitude <- 0.6  # 60% variation
     offset <- -pi/2  # Peak from June to October
     return(base_beta * (1 + amplitude * sin(2 * pi * t / period + offset)))
}

# case fatality rate
get_case_fatality_rate <- function(ages) {
     cfr <- case_when(
          ages < 1 ~ 0.1 * exp(-ages * 3),                  # High mortality for infants, decays quickly
          ages >= 1 & ages < 10 ~ 0.002 + 0.05 * exp(-ages), # Children: Lower but still declining
          ages >= 10 & ages < 60 ~ 0.0002,                  # Adults: Very low mortality
          ages >= 60 ~ 0.0002 + 0.00002 * (ages - 60)^1.5    # Elderly: Gradual increase
     )
     
     return(cfr)
}

# SEIRS model with age structure, multiple-dose vaccination & seasonal transmission
SEIRS_age_multi_dose_ODE <- function(t, state, parms) {
     # 1. Extract parameters
     n_age <- parms$n_age           # Number of age groups
     n_doses <- parms$n_doses       # Number of vaccine doses
     alpha <- parms$alpha           # Aging rate (1/average duration in each age group)
     p_m <- parms$p_m               # Proportion of newborns with maternal immunity
     birth_rate <- parms$birth_rate  # Birth rate
     beta <- get_seasonal_beta(parms$base_beta, t)  # Time-dependent transmission rate (seasonal variation)
     C <- parms$C                    # Contact matrix
     mu <- parms$mu                   # Mortality rate for each age group
     
     # Vaccination parameters (vectors of length n_doses)
     phi <- parms$phi                 # Vaccination rate for each dose
     epsilon <- parms$epsilon         # Vaccine efficacy for each dose
     omega <- parms$omega             # Immunity waning rate for each dose
     
     # Other parameters
     omega_M <- parms$omega_M  # Maternal immunity waning rate
     omega_R <- parms$omega_R  # Natural immunity waning rate
     sigma <- parms$sigma      # Rate of progression from E -> I
     gamma <- parms$gamma      # Recovery rate (I -> R)
     
     # 2. Extract state variables - vectorized approach
     # Create vectors for all compartments
     M <- numeric(n_age)
     S <- numeric(n_age)
     E <- numeric(n_age)
     I <- numeric(n_age)
     R <- numeric(n_age)
     V <- array(0, dim=c(n_age, n_doses))  # Vaccine dose compartments
     
     # Extract maternal immunity for first age group only
     M[1] <- state["M_1"]
     
     # Extract all state variables using vectorized operations
     for(g in 1:n_age) {
          S[g] <- state[paste0("S_", g)]
          E[g] <- state[paste0("E_", g)]
          I[g] <- state[paste0("I_", g)]
          R[g] <- state[paste0("R_", g)]
          
          if(n_doses > 0) {
               for(k in 1:n_doses) {
                    V[g, k] <- state[paste0("V", k, "_", g)]
               }
          }
     }
     
     # 3. Compute force of infection (lambda) for each age group - vectorized
     # Calculate total population including maternal immunity
     Ngroup <- M + S + E + I + R
     if(n_doses > 0) {
          Ngroup <- Ngroup + rowSums(V)
     }
     
     # Calculate infectious ratio and force of infection
     I_ratio <- ifelse(Ngroup > 0, I / Ngroup, 0)
     
     # Vectorized calculation of force of infection using matrix multiplication
     lambda <- beta * as.vector(C %*% I_ratio)
     
     # 4. Initialize derivative vectors for all compartments
     dM <- numeric(n_age)
     dS <- numeric(n_age)
     dE <- numeric(n_age)
     dI <- numeric(n_age)
     dR <- numeric(n_age)
     dV <- array(0, dim=c(n_age, n_doses))
     
     # Total population for birth calculations
     total_pop <- sum(Ngroup)
     
     # 5. Compute differential equations for all age groups - vectorized where possible
     
     # First age group - includes births and maternal immunity
     dM[1] <- birth_rate * total_pop * p_m -   # new births with maternal immunity
          (omega_M + alpha[1]) * M[1]          # waning immunity and aging
     
     # Susceptible equations for all age groups
     # First age group includes births
     dS[1] <- birth_rate * total_pop * (1 - p_m) + # new births without maternal immunity
          (omega_M + alpha[1] * (1 - phi[1])) * M[1] -            # waning maternal immunity and aging
          (lambda[1] + alpha[1]) * S[1]            # infections, vaccination, and aging
     
     # Add waning immunity from recovered to susceptible
     dS <- dS + omega_R * R
     
     # Add aging from previous age group to current (except for first group)
     if(n_age > 1) {
          for(g in 2:n_age) {
               if (g == 2) {
                    # from S to S
                    dS[g] <- dS[g] + alpha[g-1] * S[g-1] * (1 - phi[g-1])
                    # from S to V
                    dV[g,1] <- dV[g,1] + alpha[g-1] * S[g-1] * phi[g-1]
                    # from V to S
                    dS[g] <- dS[g] + omega[g-1] * V[g-1,1] * epsilon[g-1]
                    # from M to V
                    dV[g,1] <- alpha[g-1] * M[1] * phi[g-1]
               }
               else if (g <= n_doses+1) {
                    # for vaccine age group
                    # from S to S
                    dS[g] <- dS[g] + alpha[g-1] * S[g-1] * (1 - phi[g-1])
                    # from S to V
                    dV[g,g-1] <- dV[g,g-1] + alpha[g-1] * S[g-1] * phi[g-1]
                    # from V to S
                    dS[g] <- dS[g] + omega[g-1] * V[g-1,1] * epsilon[g-1]
                    # from V to V
                    dV[g,g-1] <- dV[g,g-1] + alpha[g-1] * V[g-1,g-2] * (1 - epsilon[g-1])
               } else {
                    # for unvaccine age group
                    # from S to S
                    dS[g] <- dS[g] + alpha[g-1] * S[g-1]
                    # from S to V
                    dV[g,n_doses] <- 0
                    # from V to S
                    dS[g] <- dS[g] + omega[n_doses] * V[g-1,n_doses] * epsilon[n_doses]
                    # from V to V
                    dV[g,n_doses] <- dV[g,n_doses] + alpha[g-1] * V[g-1, n_doses] * (1 - epsilon[n_doses])
               }
          }
     }
     
     # Exposed compartment equations
     # Force of infection on susceptible
     dE <- lambda * S - sigma * E
     
     # Handle aging for exposed (except for first age group)
     if(n_age > 1) {
          for(g in 2:n_age) {
               dE[g] <- dE[g] + alpha[g-1] * E[g-1]
          }
     }
     
     # Handle aging out of current age group (except for last age group)
     for(g in 1:(n_age-1)) {
          dE[g] <- dE[g] - alpha[g] * E[g]
     }
     
     # Infectious compartment equations
     dI <- sigma * E - (gamma + mu) * I
     dInf <- sigma * E
     dDeath <- mu * I
     
     # Handle aging for infectious (except for first age group)
     if(n_age > 1) {
          for(g in 2:n_age) {
               dI[g] <- dI[g] + alpha[g-1] * I[g-1]
          }
     }
     
     # Handle aging out of current age group (except for last age group)
     for(g in 1:(n_age-1)) {
          dI[g] <- dI[g] - alpha[g] * I[g]
     }
     
     # Recovered compartment equations
     dR <- gamma * I - omega_R * R
     
     # Handle aging for recovered (except for first age group)
     if(n_age > 1) {
          for(g in 2:n_age) {
               dR[g] <- dR[g] + alpha[g-1] * R[g-1]
          }
     }
     
     # Handle aging out of current age group (except for last age group)
     for(g in 1:(n_age-1)) {
          dR[g] <- dR[g] - alpha[g] * R[g]
     }
     
     # 6. Combine all derivatives into a single vector in the correct order
     derivs <- c('M_1' = dM[1])  # Only first age group has maternal immunity
     
     for(g in 1:n_age) {
          group_derivs <- c(dS[g], dE[g], dI[g], dR[g], dInf[g], dDeath[g])
          names(group_derivs) <- c(paste0("S_", g), paste0("E_", g), paste0("I_", g), paste0("R_", g), paste0("inci_", g), paste0("death_", g))
          
          # Add vaccine compartments if they exist
          if(n_doses > 0) {
               add_derivs <- dV[g,]
               names(add_derivs) <- c(paste0("V", 1:n_doses, "_", g))
               group_derivs <- c(group_derivs, add_derivs)
          }
          
          derivs <- c(derivs, group_derivs)
     }
     
     # print(round(derivs))
     
     return(list(derivs))
}

# run simulation ----------------------------------------------------------

# Prepare task function for parLapply
task_function <- function(task_row, country, tasks, mr) {
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
     
     # Select appropriate strategy
     current_strategy <- switch(strategy_type,
                                "A" = strategy_list$strategy_A,
                                "B" = strategy_list$strategy_B,
                                "C" = strategy_list$strategy_C)
     
     ## parameters ---------------------------------------------------------
     
     # Get number of age groups based on strategy
     ages <- c(0, current_strategy$primary_schedule, current_strategy$booster_schedule,
               20, 40, 60, 80)
     n_age <- length(ages) - 1
     alpha <- 1/(diff(ages)*365)
     
     # Mortality rate for each age group
     mu <- get_case_fatality_rate((ages[-1] + ages[-length(ages)])/2)
     
     # Transmission rate
     beta <- params$beta
     
     # Contact matrix
     C <- mr

     # Number of vaccine doses
     n_doses <- length(current_strategy$primary_schedule) + length(current_strategy$booster_schedule)
     
     # Vaccination coverage for each dose
     phi <- c(rep(current_strategy$primary_coverage, length(current_strategy$primary_schedule)),
              rep(current_strategy$booster_coverage, length(current_strategy$booster_schedule)))
     
     # Initial vaccine efficacy for each dose
     epsilon <- c(rep(0.8, length(current_strategy$primary_schedule)),
                  rep(0.9, length(current_strategy$booster_schedule)))
     
     # Vaccine immunity decay rate for each dose
     omega <- rep(params$omega, n_doses)
     omega_M <- params$omega_M
     omega_R <- params$omega_R
     p_m <-current_strategy$maternal_coverage
     
     # Disease parameters
     sigma <- 1/params$latent_period
     gamma <- 1/params$infectious_period
     
     # Birth rate
     birth_rate <- (strategy_list$birth_rate/1000)/365
     
     ## initial values ----------------------------------------------------
     
     population <- set_initial_population(1e6, ages, location = country)
     
     init_state <- c(
          M_1   = as.numeric(population[1]*p_m),
          S_1   = as.numeric(population[1]*(1 - p_m)),
          E_1   = 0,
          I_1   = 0,
          R_1   = 0,
          inci_1 = 0,
          death_1 = 0
     )
     
     # Initialize vaccinated groups
     for (k in 1:n_doses) {
          init_state[paste0("V", k, "_1")] <- 0
     }
     rm(k)
     
     # Initialize other age groups
     for (g in 2:n_age) {
          init_state[paste0("S_", g)] <- ifelse((g-1 <= n_doses),
                                                population[g] * (1 - phi[g-1]),
                                                population[g] * (1 - 0.1 - 0.01))
          init_state[paste0("E_", g)] <- 0
          init_state[paste0("I_", g)] <- population[g] * 0.01
          init_state[paste0("R_", g)] <- population[g] * 0.1
          init_state[paste0("inci_", g)] <- 0
          init_state[paste0("death_", g)] <- 0
          for (k in 1:n_doses) {
               init_state[paste0("V", k, "_", g)] <- ifelse(k == g-1, population[g] * (phi[k] - 0.1 - 0.01), 0)
          }
     }
     
     # Run ODE simulation --------------------------------------------------
     
     times <- seq(0, 5*365, by = 1)
     
     parms <- list(
          n_age = n_age,
          n_doses = n_doses,
          alpha = alpha,
          p_m = p_m,
          birth_rate = birth_rate,
          base_beta = beta,
          C = C,
          mu = mu,
          phi = phi,
          epsilon = epsilon,
          omega = omega,
          omega_M = omega_M,
          omega_R = omega_R,
          sigma = sigma,
          gamma = gamma
     )
     
     # Solve the ODE system
     out <- ode(y = init_state, times = times, func = SEIRS_age_multi_dose_ODE, parms = parms, method = 'rk4')
     df_out <- as.data.frame(out)
     
     # Add total infected column
     df_out$I_total <- rowSums(df_out[, grep("^inci_", names(df_out))])
     df_out$death_total <- rowSums(df_out[, grep("^death_", names(df_out))])
     df_out$strategy <- strategy_type

     return(df_out[, c("time", "I_total", "death_total", "strategy")])
}

run_simulation <- function(country = 'China', param_samples = 5000) {
     # country
     # c("China", "Indonesia", "Philippines", "Myanmar", "Thailand")
     
     # Contact matrix
     mr <- switch(tolower(country),
                  "china" = contact_all$CHN,
                  "indonesia" = contact_all$IDN,
                  "philippines" = contact_all$PHL,
                  "myanmar" = contact_all$MMR,
                  "thailand" = contact_all$THA,
                  stop("Unknown country"))
     
     # Create task list for each parameter set and strategy combination
     tasks <- expand.grid(
          param_id = 1:param_samples,
          strategy = c("A", "B", "C")
     )
     
     # setting parallel computing
     n_cores <- round(detectCores()/2)
     cl <- makeCluster(n_cores)
     registerDoParallel(cl)

     clusterExport(cl, ls()[!ls() == "cl"], envir = environment())
     
     cat(sprintf("Using %d cores for parallel computing.\n", n_cores))
     
     # using MCMC to simulate the model when some parameters are uncertain
     # Set seed for reproducibility
     set.seed(20250310)
     
     # Generate parameter sets for all simulations
     param_list <- lapply(1:param_samples, function(i) {
          # Vaccine immunity decay rate: 4-12 years
          omega <- 1/(rlnorm(1, log(8), 0.2)*365)
          # Natural immunity decay rate: 4-20 years
          omega_R <- 1/(rlnorm(1, log(12), 0.2)*365)
          # Maternal immunity decay rate: 1-3 months
          omega_M <- 1/(rlnorm(1, log(2), 0.2)*30)
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
               omega = omega,
               omega_R = omega_R,
               omega_M = omega_M,
               sim_id = i
          )
     })

     outcome <- parLapply(cl, 1:nrow(tasks), task_function, country = countries[1], tasks = tasks, mr = mr)
     stopCluster(cl)

     # Combine results
     outcome_df <- do.call(rbind, outcome)
     outcome_df$country <- country

     outcome_summary <- outcome_df |>
               group_by(strategy, time) |>
               summarise(
                    mean_cases = mean(I_total),
                    lower_cases = quantile(I_total, 0.025),
                    upper_cases = quantile(I_total, 0.975),
                    mean_deaths = mean(death_total),
                    lower_deaths = quantile(death_total, 0.025),
                    upper_deaths = quantile(death_total, 0.975),
                    .groups = 'drop'
               )

     return(outcome_summary)
}
