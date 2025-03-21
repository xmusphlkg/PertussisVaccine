# Load required packages
library(ggplot2)
library(dplyr)
library(patchwork)
library(openxlsx)

# status functions --------------------------------------------------------
# Assign realistic initial ages based on real population data
set_initial_population <- function(N, location = 'World', max_age = 100) {
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
     set.seed(20250309)
     initial_ages <- sample(df$Age, size = N, replace = TRUE, prob = df$Value)
     
     # Add noise (0-0.99) to the ages to avoid ties
     initial_ages <- initial_ages + runif(N)
     
     # Initialize the population with varied ages
     pop <- data.frame(
          id = 1:N,
          age = initial_ages,  # Set initial ages
          state = rep("S", N),  # Start in susceptible state
          immunity = rep(0, N),
          last_vax = rep(NA, N),
          vax_type = rep("none", N),
          infection_time = rep(NA, N),
          recovered_time = rep(NA, N),
          location = rep(location, N),  # Record the location used
          vax_history = I(replicate(N, list())),  # Initialize empty list for vaccination history
          outcome = rep(NA, N),
          stringsAsFactors = FALSE
     )
     
     return(pop)
}

# pop <- set_initial_population(N, location = 'China')

# Assign pre-existing immunity based on age
assign_preexisting_immunity <- function(pop, strategy, params) {
     N <- nrow(pop)
     
     # Assign maternal immunity at birth (only for newborns)
     newborns <- pop$age < 0.1
     if(sum(newborns) > 0) {
          # First assign vax_type
          pop$vax_type[newborns] <- ifelse(runif(sum(newborns)) < strategy$maternal_coverage, "maternal", "none")
          # Then assign immunity based on the updated vax_type for newborns only
          pop$immunity[newborns] <- ifelse(pop$vax_type[newborns] == "maternal", params$M0, 0)
     }
     
     # Assign vaccine-derived immunity based on age-specific vaccination coverage
     primary_intervals <- data.frame(start = strategy$primary_schedule,
                                     end = c(strategy$primary_schedule[-1], 
                                             ifelse(length(strategy$booster_schedule) > 0, 
                                                    strategy$booster_schedule[1], 
                                                    Inf)),
                                     dose_number = 1:length(strategy$primary_schedule))
     
     for(i in 1:nrow(primary_intervals)) {
          interval <- primary_intervals[i, ]
          eligible <- which(pop$age >= interval$start & 
                                 pop$age < interval$end & 
                                 pop$vax_type == "none")
          
          if(length(eligible) > 0) {
               vaccinated <- eligible[runif(length(eligible)) < strategy$primary_coverage]
               
               if(length(vaccinated) > 0) {
                    # Update vaccination status and immunity
                    pop$vax_type[vaccinated] <- "primary"
                    pop$immunity[vaccinated] <- params$VE0_primary * exp(-params$k * (pop$age[vaccinated] - interval$start))
                    pop$last_vax[vaccinated] <- interval$start
                    
                    # Create vaccination records for each individual
                    vax_records <- lapply(pop$age[vaccinated] - interval$start, function(time) {
                         list(time = time, type = "primary", dose_number = interval$dose_number)
                    })
                    
                    # Update vaccination history
                    for(j in seq_along(vaccinated)) {
                         pop$vax_history[[vaccinated[j]]] <- c(
                              pop$vax_history[[vaccinated[j]]], 
                              list(vax_records[[j]])
                         )
                    }
               }
          }
     }
     
     # Assign booster-derived immunity
     if(length(strategy$booster_schedule) > 0) {
          booster_intervals <- data.frame(
               start = strategy$booster_schedule,
               end = c(strategy$booster_schedule[-1], 
                       strategy$booster_schedule[length(strategy$booster_schedule)] + 2),
               dose_number = 1:length(strategy$booster_schedule)
          )
          
          for(i in 1:nrow(booster_intervals)) {
               interval <- booster_intervals[i, ]
               eligible <- which(pop$age >= interval$start & 
                                      pop$age < interval$end & 
                                      pop$vax_type == "none")
               
               if(length(eligible) > 0) {
                    boosted <- eligible[runif(length(eligible)) < strategy$booster_coverage]
                    
                    if(length(boosted) > 0) {
                         # Update vaccination status and immunity
                         pop$vax_type[boosted] <- "booster"
                         pop$immunity[boosted] <- params$VE0_booster * 
                              exp(-params$k * (pop$age[boosted] - interval$start))
                         pop$last_vax[boosted] <- interval$start
                         
                         # Create vaccination records for each individual
                         vax_records <- lapply(pop$age[boosted] - interval$start, function(time) {
                              list(time = time, type = "booster", dose_number = interval$dose_number)
                         })
                         
                         # Update vaccination history
                         for(j in seq_along(boosted)) {
                              pop$vax_history[[boosted[j]]] <- c(
                                   pop$vax_history[[boosted[j]]], 
                                   list(vax_records[[j]])
                              )
                         }
                    }
               }
          }
     }
     
     # Assign natural immunity for older individuals (assuming some past exposure)
     previously_infected <- which(pop$age > 5 & runif(N) < strategy$natural_percent)
     if(length(previously_infected) > 0) {
          pop$state[previously_infected] <- "R"
          pop$immunity[previously_infected] <- params$natural_immunity * 
               exp(-params$k_n * pop$age[previously_infected])
          pop$vax_type[previously_infected] <- "natural"
     }
     
     return(pop)
}

# Initialize the population with infection events
initialize_infections <- function(pop, init_infected_fraction) {
     N_infected <- round(nrow(pop) * init_infected_fraction)
     infected_ids <- sample(which(pop$state == "S" & pop$age != 0), N_infected)
     pop$state[infected_ids] <- "I"
     pop$infection_time[infected_ids] <- 0  # They were already infected at start
     return(pop)
}

# Vectorized function: given age (in years), return the per-year probability of death due to disease.
mortality_rate_vec <- function(age) {
     # For simplicity, we define age-dependent mortality as follows:
     # age < 2 month: 1% | 2-6 months: 0.5% | 6-12 months: 0.25% | 1-5 years: 0.1% | 5-65 years: 0% | 65+ years: 0.1%
     age_breaks <- c(-Inf, 2*30, 6*30, 12*30, 5*365, 65*365, Inf)
     mortality_rates <- c(0.1, 0.05, 0.025, 0.001, 0, 0.001)
     rate <- mortality_rates[findInterval(age, age_breaks)]
     return(rate)
}

# Add birth events to the population
add_births <- function(pop, dt, strategy, params) {
     # Calculate the current population size and the number of new births
     N_current <- sum(pop$state != "D")
     N_births <- round(N_current * (params$birth_rate/1000) / 365)
     
     if(N_births > 0) {
          new_ids <- max(pop$id) + 1:N_births
          
          newborns <- data.frame(
               id = new_ids,
               age = runif(N_births, 0, dt),
               state = rep("S", N_births),
               immunity = rep(0, N_births),
               last_vax = rep(NA, N_births),
               vax_type = rep("none", N_births),
               infection_time = rep(NA, N_births),
               recovered_time = rep(NA, N_births),
               location = rep(unique(pop$location)[1], N_births),
               vax_history = I(replicate(N_births, list())),
               outcome = rep(NA, N_births),
               stringsAsFactors = FALSE
          )
          
          # add maternal immunity to newborns
          maternal_immunity <- runif(N_births) < strategy$maternal_coverage
          if(sum(maternal_immunity) > 0) {
               newborns$vax_type[maternal_immunity] <- "maternal"
               newborns$immunity[maternal_immunity] <- params$M0
          }
          
          # combine newborns with the existing population
          pop <- rbind(pop, newborns)
     }
     
     return(pop)
}

# Update age for all individuals
update_age <- function(pop, dt) {
     pop$age <- pop$age + dt
     return(pop)
}

# Update infection events:
# For susceptibles, the probability of infection is reduced by their immunity (protection).
update_infection <- function(pop, beta, dt) {
     # Calculate force of infection (lambda) using mass action: lambda = beta * (# I) / N_alive
     alive <- pop$state != "D"
     N_eff <- sum(alive)
     I_count <- sum(pop$state == "I")
     lambda <- beta * I_count / N_eff
     # For individuals in state "S", compute probability of infection
     idx_S <- which(pop$state == "S")
     if(length(idx_S) > 0) {
          # Basic probability (if fully susceptible) is: 1 - exp(-lambda * dt)
          p_base <- 1 - exp(-lambda * dt)
          # Effective probability is reduced by the individual's immunity (if immunity = 1, fully protected)
          p_inf <- p_base * (1 - pop$immunity[idx_S])
          # Draw a random uniform for each susceptible
          new_infections <- idx_S[ runif(length(idx_S)) < p_inf ]
          if(length(new_infections) > 0) {
               pop$state[new_infections] <- "E"         # move to Exposed
               pop$infection_time[new_infections] <- NA   # record time later if desired
          }
     }
     return(pop)
}

# Update progression from E -> I, and from I -> R or death.
update_progression <- function(pop, t, dt, latent_period, infectious_period, natural_immunity, k_n) {
     # For Exposed individuals: progress to I with probability dt/latent_period
     idx_E <- which(pop$state == "E")
     if(length(idx_E) > 0) {
          p_EI <- dt / latent_period
          progress <- idx_E[ runif(length(idx_E)) < p_EI ]
          if(length(progress) > 0) {
               pop$state[progress] <- "I"
               pop$infection_time[progress] <- t  # record time when becoming infectious
               
               # estimate case fatality rate based on age
               case_fatality_rate <- mortality_rate_vec(pop$age[progress]) * infectious_period
               case_fatality_rate <- pmin(case_fatality_rate, 1)
               will_die <- runif(length(progress)) < case_fatality_rate
               
               # store outcome
               pop$outcome[progress] <- ifelse(will_die, "death", "recovery")
          }
     }
     
     # For Infectious individuals: they progress based on their predetermined outcome
     idx_I <- which(pop$state == "I")
     if(length(idx_I) > 0) {
          p_end <- dt / infectious_period
          rand_end <- runif(length(idx_I))
          end_idx <- idx_I[rand_end < p_end]
          
          if(length(end_idx) > 0) {
               die_idx <- end_idx[pop$outcome[end_idx] == "death"]
               rec_idx <- end_idx[pop$outcome[end_idx] == "recovery"]
               
               # Death
               if(length(die_idx) > 0) {
                    pop$state[die_idx] <- "D"
               }
               
               # Recovery
               if(length(rec_idx) > 0) {
                    pop$state[rec_idx] <- "R"
                    pop$recovered_time[rec_idx] <- t
                    pop$immunity[rec_idx] <- natural_immunity   # set natural immunity level upon recovery
                    pop$vax_type[rec_idx] <- "natural"
               }
          }
     }
     return(pop)
}

# Update immunity decay for individuals based on the type of immunity:
# Maternal immunity decays with rate delta_m;
# Vaccine-induced immunity (primary/booster) decays with rate k;
# Natural immunity decays with rate k_n.
update_immunity <- function(pop, dt, delta_m, k, k_n) {
     # For maternal immunity
     idx_maternal <- which(pop$vax_type == "maternal")
     if(length(idx_maternal) > 0) {
          pop$immunity[idx_maternal] <- pop$immunity[idx_maternal] * exp(-delta_m * dt)
          # If maternal immunity decays below a threshold, clear it (set to 0 and mark as none)
          drop_idx <- idx_maternal[ pop$immunity[idx_maternal] < 0.1 ]
          if(length(drop_idx) > 0) {
               pop$immunity[drop_idx] <- 0
               pop$vax_type[drop_idx] <- "none"
          }
     }
     # For vaccine-induced immunity (primary or booster)
     idx_vax <- which(pop$vax_type %in% c("primary", "booster"))
     if(length(idx_vax) > 0) {
          pop$immunity[idx_vax] <- pop$immunity[idx_vax] * exp(-k * dt)
     }
     # For natural immunity
     idx_nat <- which(pop$vax_type == "natural")
     if(length(idx_nat) > 0) {
          pop$immunity[idx_nat] <- pop$immunity[idx_nat] * exp(-k_n * dt)
          # If natural immunity decays below a threshold, lose protection and revert to susceptible
          drop_nat <- idx_nat[ pop$immunity[idx_nat] < 0.1 ]
          if(length(drop_nat) > 0) {
               pop$immunity[drop_nat] <- 0
               pop$vax_type[drop_nat] <- "none"
               pop$state[drop_nat] <- "S"
          }
     }
     return(pop)
}

# Update vaccination events.
# Based on a scheduled dose (primary or booster), if an individual's age is in the window
# and they have not yet been vaccinated (or only have primary), then administer the dose with a given probability.
update_vaccination <- function(pop, t, dt, strategy, params) {
     # Primary vaccination doses
     primary_intervals <- data.frame(start = strategy$primary_schedule,
                                     end = c(strategy$primary_schedule[-1], 
                                             ifelse(length(strategy$booster_schedule) > 0, 
                                                    strategy$booster_schedule[1], 
                                                    Inf)),
                                     dose_number = 1:length(strategy$primary_schedule))
     
     for(i in 1:nrow(primary_intervals)) {
          interval <- primary_intervals[i, ]
          # Eligible individuals: alive, within the age window, and not already vaccinated with this dose
          idx <- which(pop$state != "D" & 
                            pop$age >= interval$start & 
                            pop$age < (interval$start + dt))
          
          if(length(idx) > 0) {
               has_dose <- logical(length(idx))
               
               # Check if they have already received this dose
               check_dose <- function(j) {
                    any(sapply(pop$vax_history[[j]], function(vax) {
                         !is.null(vax$dose_number) && 
                              vax$type == "primary" && 
                              vax$dose_number == interval$dose_number
                    }))
               }
               
               # Check each individual for the dose
               has_dose <- sapply(idx, check_dose)
               eligible_idx <- idx[!has_dose]
               
               if(length(eligible_idx) > 0) {
                    vacc <- rbinom(length(eligible_idx), 1, strategy$p_primary)
                    dose_idx <- eligible_idx[which(vacc == 1)]
                    
                    if(length(dose_idx) > 0) {
                         # Update vaccination status and immunity
                         pop$vax_type[dose_idx] <- "primary"
                         pop$immunity[dose_idx] <- pmax(pop$immunity[dose_idx], params$VE0_primary)
                         pop$last_vax[dose_idx] <- t
                         
                         # Record vaccination history
                         vax_records <- lapply(seq_along(dose_idx), function(j) {
                              list(time = t, 
                                   type = "primary", 
                                   dose_number = interval$dose_number,
                                   age = pop$age[dose_idx[j]])
                         })
                         
                         # Update vaccination history
                         for(j in seq_along(dose_idx)) {
                              pop$vax_history[[dose_idx[j]]] <- c(
                                   pop$vax_history[[dose_idx[j]]], 
                                   list(vax_records[[j]])
                              )
                         }
                    }
               }
          }
     }
     
     # Booster vaccination doses (only for those already vaccinated with primary)
     if(length(strategy$booster_schedule) > 0) {
          booster_intervals <- data.frame(start = strategy$booster_schedule,
                                          end = c(strategy$booster_schedule[-1], 
                                                  strategy$booster_schedule[length(strategy$booster_schedule)] + 2),
                                          dose_number = 1:length(strategy$booster_schedule))
          
          for(i in 1:nrow(booster_intervals)) {
               interval <- booster_intervals[i, ]
               # Eligible individuals: alive, within the age window, and not already vaccinated with this dose
               idx <- which(pop$state != "D" & 
                                 pop$age >= interval$start & 
                                 pop$age < (interval$start + dt))
               
               if(length(idx) > 0) {
                    has_dose <- logical(length(idx))
                    
                    # Check if they have already received this dose
                    check_dose <- function(j) {
                         any(sapply(pop$vax_history[[j]], function(vax) {
                              !is.null(vax$dose_number) && 
                                   vax$type == "booster" && 
                                   vax$dose_number == interval$dose_number
                         }))
                    }
                    
                    # Check each individual for the dose
                    has_dose <- sapply(idx, check_dose)
                    eligible_idx <- idx[!has_dose]
                    
                    if(length(eligible_idx) > 0) {
                         vacc <- rbinom(length(eligible_idx), 1, strategy$p_booster)
                         booster_idx <- eligible_idx[which(vacc == 1)]
                         
                         if(length(booster_idx) > 0) {
                              # Update vaccination status and immunity
                              pop$vax_type[booster_idx] <- "booster"
                              pop$immunity[booster_idx] <- pmax(pop$immunity[booster_idx], params$VE0_booster)
                              pop$last_vax[booster_idx] <- t
                              
                              # Create vaccination records for each individual
                              vax_records <- lapply(seq_along(booster_idx), function(j) {
                                   list(time = t, 
                                        type = "booster", 
                                        dose_number = interval$dose_number,
                                        age = pop$age[booster_idx[j]])
                              })
                              
                              # Update vaccination history
                              for(j in seq_along(booster_idx)) {
                                   pop$vax_history[[booster_idx[j]]] <- c(
                                        pop$vax_history[[booster_idx[j]]], 
                                        list(vax_records[[j]])
                                   )
                              }
                         }
                    }
               }
          }
     }
     
     return(pop)
}

# Set up seasonal variation in transmission rate
get_seasonal_beta <- function(base_beta, t) {
     # For simplicity, we assume a sinusoidal variation in beta over the year
     # with a peak from June to October
     period <- 365  # 1 year
     amplitude <- 0.6 # 60% variation
     offset <- -pi/2  
     
     seasonal_beta <- base_beta * (1 + amplitude * sin(2 * pi * t / period + offset))
     
     return(seasonal_beta)
}

# main function -----------------------------------------------------------
run_simulation <- function(strategy, params, T_end, dt, N, location) {
     # Step 1: Set Initial Population
     pop <- set_initial_population(N, location = location)
     # hist(pop$age, breaks = 20, main = "Initial Age Distribution")
     cat("Initial Age Distribution.\n")
     
     # Step 2: Assign Pre-existing Immunity
     pop <- assign_preexisting_immunity(pop, strategy, params)
     cat("Vaccination Status:")
     print(knitr::kable(table(pop$vax_type)/N, col.names = c('Vaccine Type', 'Count')))
     
     # Step 3: Introduce Initial Infections
     pop <- initialize_infections(pop, init_infected_fraction = 0.001)
     cat("Population Status:")
     print(knitr::kable(table(pop$state)/N, col.names = c('State', 'Count')))
     
     # Step 4: Prepare summary storage
     time_seq <- seq(1, T_end, by = dt)
     summary_df <- data.frame(time = time_seq, S = NA, E = NA, I = NA, R = NA, D = NA, New = NA,
                              Population = NA, Births = NA, Deaths = NA)
     
     # Step 5: Simulation loop
     prev_population <- sum(pop$state != "D")
     prev_deaths <- 0
     
     for (t in time_seq) {
          pop <- add_births(pop, dt, strategy, params)
          pop <- update_age(pop, dt/365)
          pop <- update_vaccination(pop, t, dt, strategy, params)
          pop <- update_immunity(pop, dt, params$delta_m, params$k, params$k_n)
          
          # Using seasonal variation in transmission rate
          beta <- get_seasonal_beta(params$beta, t)
          pop <- update_infection(pop, beta, dt)
          
          # Update disease progression
          pop <- update_progression(pop, t, dt,
                                    params$latent_period,
                                    params$infectious_period,
                                    params$natural_immunity,
                                    params$k_n)
          
          # Compute new infections
          new_infections <- sum(pop$state == "I" & pop$infection_time == t)
          current_deaths <- sum(pop$state == "D" & pop$recovered_time == t)
          current_population <- sum(pop$state != "D")
          new_deaths <- current_deaths - prev_deaths
          new_births <- current_population - prev_population + new_deaths
          
          # Store results
          summary_df$S[summary_df$time == t] <- sum(pop$state == "S")
          summary_df$E[summary_df$time == t] <- sum(pop$state == "E")
          summary_df$I[summary_df$time == t] <- sum(pop$state == "I")
          summary_df$R[summary_df$time == t] <- sum(pop$state == "R")
          summary_df$D[summary_df$time == t] <- sum(pop$state == "D")
          summary_df$New[summary_df$time == t] <- new_infections
          summary_df$Population[summary_df$time == t] <- current_population
          summary_df$Births[summary_df$time == t] <- new_births
          summary_df$Deaths[summary_df$time == t] <- new_deaths
          
          prev_population <- current_population
          prev_deaths <- current_deaths
          
          # check for early termination
          if (sum(pop$state == "E") == 0 && sum(pop$state == "I") == 0) {
               cat(sprintf("Discrete Event Simulation terminated at t = %.1f days.\n", t))
               break
          }
     }
     
     return(list(population = pop, summary = summary_df))
}
