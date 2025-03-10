#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2025-03-10 11:04:28
## @LastEditors: Li Kangguo
## @LastEditTime: 2025-03-10 21:07:04
#####################################

# China -------------------------------------------------------------------

strategy_china <- list(
     strategy_A = list(
          label = "Baseline",
          primary_schedule = c(3/12, 4/12, 5/12),  # times in years
          primary_coverage = 0.99,         # baseline primary dose coverage
          booster_schedule = c(18/12),     # booster at 18 months
          booster_coverage = 0.99,          # not applicable
          p_primary = 0.8,                 # uptake probability for primary doses
          p_booster = 0.8,                   # not applicable
          natural_percent = 0.1,          # recovery rate from natural infection
          maternal_coverage = 0.1          # baseline maternal coverage
     ),
     # Strategy B: Optimized 1 – Additional booster at 4 and 10 years; improved booster coverage.
     strategy_B = list(
          label = "Booster at 4 and 10 years",
          primary_schedule = c(3/12, 4/12, 5/12),
          primary_coverage = 0.99,
          booster_schedule = c(18/12, 4, 10), # booster at 4 and 10 years
          booster_coverage = 0.99,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     # Strategy C: Optimized 2 – Improved antenatal vaccination (increased maternal coverage).
     strategy_C = list(
          label = "Maternal vaccination",
          primary_schedule = c(3/12, 4/12, 5/12),
          primary_coverage = 0.99,
          booster_schedule = c(18/12),
          booster_coverage = 0.99,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.98
     ),
     # birth rate: per 1000 population
     # https://www.reuters.com/world/china/chinas-population-falls-third-consecutive-year-2025-01-17/
     birth_rate = 6.77
)

# Indonesia ----------------------------------------------------------------

strategy_indonesia <- list(
     strategy_A = list(
          label = "Baseline",
          primary_schedule = c(2/12, 3/12, 4/12),  # 2,3,4 months
          primary_coverage = 0.87,                  # baseline primary dose coverage
          booster_schedule = c(18/12),             # booster at 18 months
          booster_coverage = 0.85,                 # baseline booster coverage
          p_primary = 0.8,                         # uptake probability for primary doses
          p_booster = 0.8,                         # uptake probability for booster
          natural_percent = 0.1,                   # recovery rate from natural infection
          maternal_coverage = 0.1                  # baseline maternal coverage
     ),
     
     # Strategy B: Improved coverage for primary and booster doses
     strategy_B = list(
          label = "Improved coverage",
          primary_schedule = c(2/12, 3/12, 4/12),
          primary_coverage = 0.95,                 # improved primary coverage
          booster_schedule = c(18/12),
          booster_coverage = 0.95,                 # improved booster coverage
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     
     # Strategy C: Additional booster dose
     strategy_C = list(
          label = "Additional booster",
          primary_schedule = c(2/12, 3/12, 4/12),
          primary_coverage = 0.87,
          booster_schedule = c(18/12, 4, 10),         # additional booster at 4 and 10 years
          booster_coverage = 0.85,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     # birth rate: per 1000 population
     # https://en.wikipedia.org/wiki/Demographics_of_Indonesia
     birth_rate = 15.32
)

# Philippines --------------------------------------------------------------

strategy_philippines <- list(
     strategy_A = list(
          label = "Baseline",
          primary_schedule = c(6/52, 10/52, 14/52),  # 6,10,14 weeks (converted to years)
          primary_coverage = 0.74,                    # baseline primary dose coverage
          booster_schedule = c(),                     # no booster in baseline
          booster_coverage = 0.72,                    # baseline booster coverage
          p_primary = 0.8,                            # uptake probability for primary doses
          p_booster = 0.8,                            # uptake probability for booster
          natural_percent = 0.1,                      # recovery rate from natural infection
          maternal_coverage = 0.1                     # baseline maternal coverage
     ),
     
     # Strategy B: Improved coverage for primary and booster doses
     strategy_B = list(
          label = "Improved coverage",
          primary_schedule = c(6/52, 10/52, 14/52),
          primary_coverage = 0.95,                    # improved primary coverage
          booster_schedule = c(),
          booster_coverage = 0.95,                    # improved booster coverage
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     
     # Strategy C: Additional booster doses
     strategy_C = list(
          label = "Additional boosters",
          primary_schedule = c(6/52, 10/52, 14/52),
          primary_coverage = 0.74,
          booster_schedule = c(18/12, 4, 10),         # add boosters at 18 months, 4 years, and 10 years
          booster_coverage = 0.72,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     # birth rate: per 1000 population
     # https://en.wikipedia.org/wiki/Demographics_of_the_Philippines
     birth_rate = 12.4
)

# Myanmar ------------------------------------------------------------------

strategy_myanmar <- list(
     strategy_A = list(
          label = "Baseline",
          primary_schedule = c(2/12, 4/12, 6/12),  # 2,4,6 months
          primary_coverage = 0.81,                 # baseline primary dose coverage
          booster_schedule = c(18/12),             # booster at 18 months
          booster_coverage = 0.71,                 # baseline booster coverage
          p_primary = 0.8,                         # uptake probability for primary doses
          p_booster = 0.8,                         # uptake probability for booster
          natural_percent = 0.1,                   # recovery rate from natural infection
          maternal_coverage = 0.1                  # baseline maternal coverage
     ),
     
     # Strategy B: Improved coverage for primary and booster doses
     strategy_B = list(
          label = "Improved coverage",
          primary_schedule = c(2/12, 4/12, 6/12),
          primary_coverage = 0.95,                 # improved primary coverage
          booster_schedule = c(18/12),
          booster_coverage = 0.95,                 # improved booster coverage
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     
     # Strategy C: Additional booster doses
     strategy_C = list(
          label = "Additional boosters",
          primary_schedule = c(2/12, 4/12, 6/12),
          primary_coverage = 0.81,
          booster_schedule = c(18/12, 4, 10),      # add boosters at 18 months, 4 years, and 10 years
          booster_coverage = 0.71,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     # birth rate: per 1000 population
     # https://en.wikipedia.org/wiki/Demographics_of_Myanmar
     birth_rate = 16.34
)

# Thailand -----------------------------------------------------------------

strategy_thailand <- list(
     strategy_A = list(
          label = "Baseline",
          primary_schedule = c(2/12, 4/12, 6/12),  # 2,4,6 months
          primary_coverage = 0.99,                 # high primary dose coverage
          booster_schedule = c(1.5, 4),            # boosters at 1.5 and 4 years
          booster_coverage = 0.97,                 # high booster coverage
          p_primary = 0.8,                        
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     
     # Strategy B: Booster at 1.5, 4 and 10 years
     strategy_B = list(
          label = "Booster at 10 years",
          primary_schedule = c(2/12, 4/12, 6/12),
          primary_coverage = 0.99,
          booster_schedule = c(1.5, 4, 10),       # boosters at 1.5, 4, and 10 years
          booster_coverage = 0.97,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.1
     ),
     
     # Strategy C: Maternal vaccination
     strategy_C = list(
          label = "Maternal vaccination",
          primary_schedule = c(2/12, 4/12, 6/12),
          primary_coverage = 0.99,
          booster_schedule = c(1.5, 4),
          booster_coverage = 0.97,
          p_primary = 0.8,
          p_booster = 0.8,
          natural_percent = 0.1,
          maternal_coverage = 0.98
     ),
     # birth rate: per 1000 population
     # https://en.wikipedia.org/wiki/Demographics_of_Thailand
     birth_rate = 7
)