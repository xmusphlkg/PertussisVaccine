
Sys.setlocale("LC_TIME", "English")

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(jsonlite)

source('./Code/function.R')

# Data --------------------------------------------------------------------

## Data from the World Health Organization (WHO)
## https://immunizationdata.who.int/global/wiise-detail-page/diphtheria-tetanus-toxoid-and-pertussis-(dtp)-vaccination-coverage?ANTIGEN=&YEAR=&CODE=
## Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage
## These data represent administrative and official Diphtheria Tetanus Toxoid and Pertussis (DTP) vaccination coverage reported annually through the WHO/UNICEF Joint Reporting Form on Immunization (JRF). Data is updated as country data is received.
## WHO/UNICEF Estimates of National Immunization Coverage (WUENIC) are also displayed here. National, regional, and global data are updated annually mid-July.

DataVacCover <- read.xlsx("./Data/DTP vaccination coverage.xlsx")
DataVacCover <- DataVacCover |>
     filter(COVERAGE_CATEGORY == 'WUENIC' & GROUP == 'COUNTRIES') |>
     select(CODE, NAME, ANTIGEN, COVERAGE, YEAR) |>
     group_by(CODE, NAME, ANTIGEN) |>
     mutate(MAX_YEAR = max(YEAR)) |>
     filter(YEAR == MAX_YEAR) |>
     summarise(
          COVERAGE = max(COVERAGE),
          MAX_YEAR = max(YEAR),
          .groups = 'drop'
     ) |> 
     pivot_wider(names_from = ANTIGEN, values_from = COVERAGE) |> 
     select(-MAX_YEAR) |> 
     rename(CoverageDTP3 = DTPCV3,
            CoverageDTP1 = DTPCV1)

## Data from the World Health Organization (WHO)
## https://immunizationdata.who.int/global/wiise-detail-page/vaccination-schedule-for-pertussis?ISO_3_CODE=&TARGETPOP_GENERAL=
## Vaccination schedule for Pertussis
## The vaccine scheduler table summarizes the current vaccination schedule for young children, adolescents, and adults for Pertussis. The data is updated regularly with the most recent official country reporting collected through the WHO/UNICEF joint reporting process.

DataVacSchedule <- read.xlsx("./Data/Vaccination schedule for Pertussis.xlsx") |> 
     filter(!is.na(COUNTRYNAME))
DataInfo <- DataVacSchedule |> 
     select(ISO_3_CODE, COUNTRYNAME, WHO_REGION) |>
     unique() |>
     rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME)
DataVacSchedule <- DataVacSchedule |> 
     select(ISO_3_CODE, COUNTRYNAME, VACCINECODE, SCHEDULEROUNDS, TARGETPOP_DESCRIPTION,
            AGEADMINISTERED, VACCINEDESCRIPTIONSHORT, YEAR, SCHEDULERCODE) |>
     rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME)


# Use wP vaccine
DataVacWP <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% c('General/routine')) |> 
     select(CODE, NAME, SCHEDULERCODE) |> 
     unique() |> 
     mutate(A = T) |>
     pivot_wider(names_from = SCHEDULERCODE, values_from = A) |> 
     mutate(VaccineCode = case_when(
          aP & wP ~ 'Both',
          aP ~ 'aP',
          wP ~ 'wP',
          TRUE ~ 'Unknown'
          ),
          VaccineCode = factor(VaccineCode, levels = c('wP', 'aP', 'Both', 'Unknown'))) |> 
     select(CODE, NAME, VaccineCode)

# Adult
DataVacScheduleAdult <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% 'Adults') |> 
     select(CODE, NAME, SCHEDULEROUNDS) |> 
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(ADULT = 1,
               .groups = 'drop')

# "Risk group(s)" or "Health workers"
DataVacScheduleRisk <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% c('Risk group(s)', "Health workers")) |> 
     select(CODE, NAME, SCHEDULEROUNDS) |> 
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(RISK = 1,
               .groups = 'drop')

# Pregnant women
DataVacSchedulePreg <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION == 'Pregnant women') |> 
     select(CODE, NAME, SCHEDULEROUNDS) |> 
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(PREGNANT = 1,
               .groups = 'drop')
# General/routine
DataVacScheduleGenM <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION == 'General/routine') |> 
     # filter AGEADMINISTERED contains M or W
     filter(str_detect(AGEADMINISTERED, '[MМ]|W')) |> 
     select(CODE, NAME, SCHEDULEROUNDS, AGEADMINISTERED) |>
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(AGEADMINISTERED = length(unique(AGEADMINISTERED)),
               SCHEDULEROUNDS = length(unique(SCHEDULEROUNDS)),
               .groups = 'keep') |> 
     mutate(GENERALM = min(SCHEDULEROUNDS, AGEADMINISTERED)) |> 
     select(CODE, NAME, GENERALM)

DataVacScheduleGenY <- DataVacSchedule |>
     filter(TARGETPOP_DESCRIPTION == 'General/routine') |> 
     # filter AGEADMINISTERED contains Y
     filter(str_detect(AGEADMINISTERED, 'Y')) |>
     select(CODE, NAME, SCHEDULEROUNDS, AGEADMINISTERED) |>
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(AGEADMINISTERED = length(unique(AGEADMINISTERED)),
               SCHEDULEROUNDS = length(unique(SCHEDULEROUNDS)),
               .groups = 'keep') |> 
     mutate(GENERALY = min(SCHEDULEROUNDS, AGEADMINISTERED)) |> 
     select(CODE, NAME, GENERALY)

DataVacScheduleGen <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION == 'General/routine') |> 
     group_by(CODE, NAME) |>
     summarise(GENERALLASTSHOT = max(process_date_column(unique(AGEADMINISTERED))),
               GENERALFIRSTSHOT = min(process_date_column(unique(AGEADMINISTERED))),
               .groups = 'drop') |>
     left_join(DataVacScheduleGenM, by = c('CODE', 'NAME')) |> 
     left_join(DataVacScheduleGenY, by = c('CODE', 'NAME')) |> 
     mutate(GENERALY = replace_na(GENERALY, 0),
            GENERAL = GENERALM + GENERALY)

DataVacSchedule <- DataVacSchedule |> 
     select(CODE, NAME) |> 
     unique() |>
     full_join(DataVacSchedulePreg, by = c('CODE', 'NAME')) |>
     mutate(PREGNANT = replace_na(PREGNANT, 0)) |>
     full_join(DataVacScheduleAdult, by = c('CODE', 'NAME')) |>
     mutate(ADULT = replace_na(ADULT, 0)) |>
     full_join(DataVacScheduleRisk, by = c('CODE', 'NAME')) |>
     mutate(RISK = replace_na(RISK, 0)) |>
     full_join(DataVacScheduleGen, by = c('CODE', 'NAME'))
DataVacSchedule <- DataVacSchedule |>
     rename(TimeFirstShot = GENERALFIRSTSHOT,
            TimeLastShot = GENERALLASTSHOT,
            VaccinePregnant = PREGNANT,
            VaccineAdult = ADULT,
            VaccineRisk = RISK,
            VaccineGeneral = GENERAL)

remove(DataVacSchedulePreg, DataVacScheduleAdult, DataVacScheduleRisk,
       DataVacScheduleGen, DataVacScheduleGenM, DataVacScheduleGenY)

# ## Data from the World Health Organization (WHO)
# ## https://immunizationdata.who.int/global/wiise-detail-page/introduction-of-ap-(acellular-pertussis)-vaccine?ISO_3_CODE=&YEAR=
# ## Introduction of aP (acellular pertussis) vaccine
# ## Introduction status of aP (acellular pertussis) vaccine over time. These data summarize country introduction status of aP (acellular pertussis) vaccine in the national immunization programme. Data are updated regularly and are derived from official country reporting to the World Health Organization.
# 
# DataVacAP <- read.xlsx("./Data/Introduction of aP.xlsx")
# print(length(unique(DataVacAP$COUNTRYNAME)))
# 
# # Introduction of aP (acellular pertussis) vaccine
# DataVacAPYes <- DataVacAP |> 
#      select(ISO_3_CODE, COUNTRYNAME, INTRO, YEAR) |>
#      filter(INTRO == 'Yes') |> 
#      # find the min year of introduction
#      group_by(ISO_3_CODE, COUNTRYNAME) |>
#      summarise(IntroAPYear = min(YEAR),
#                .groups = 'drop') |> 
#      rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME) |> 
#      select(CODE, NAME, IntroAPYear)
# 
# # No introduction of aP (acellular pertussis) vaccine
# DataVacAPNo <- DataVacAP |> 
#      select(ISO_3_CODE, COUNTRYNAME, INTRO, YEAR) |>
#      # find the country which has no introduction of aP vaccine
#      group_by(ISO_3_CODE, COUNTRYNAME) |>
#      summarise(Intro = all(unique(INTRO) != 'Yes'),
#                IntroAPYear = NA,
#                .groups = 'drop') |> 
#      filter(Intro == TRUE) |>
#      rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME) |> 
#      select(CODE, NAME, IntroAPYear)
# 
# # Merge data
# DataVacAP <- DataVacAPYes |>
#      bind_rows(DataVacAPNo) |>
#      mutate(IntroAP = !is.na(IntroAPYear))
# remove(DataVacAPYes, DataVacAPNo)

## Data from the World Health Organization (WHO)
## Merge all data
DataVac <- DataVacCover |> 
     merge(DataVacSchedule, by = c('CODE', 'NAME'), all = T) |>
     merge(DataVacWP, by = c('CODE', 'NAME'), all = T)

# remove(DataVacCover, DataVacSchedule, DataVacAP, DataVacWP)

# ## Data from the HealthMap
# ## https://www.healthmap.org/en/
# ## HealthMap is a global disease alert mapping system that provides real-time information on infectious diseases around the world. The system is used by governments, international organizations, and the public to monitor the spread of diseases and plan interventions.
# 
# DataOutbreak <- read_json('./Data/healthmap_pertussis.json')
# DataOutbreak <- DataOutbreak$markers
# DataOutbreak <- DataOutbreak |> 
#      map_df(~as.data.frame(.x), .id = 'id') |> 
#      select(place_id, place_name, html) |> 
#      mutate(Date = as.Date(str_extract(html, "\\d{1,2} \\w+ \\d{4}"), format = "%d %b %Y"),
#             YEAR = year(Date),
#             # get the country name from place_name
#             NAME = str_extract(place_name, "[^,]+$"),
#             # remove the space at the beginning of the country name
#             NAME = str_remove(NAME, "^ ")) |> 
#      select(Date, YEAR, NAME) |> 
#      group_by(NAME) |>
#      summarise(Outbreak = 1,
#                .groups = 'drop') |> 
#      mutate(NameCheck = !NAME %in% DataVac$NAME)
# 
# # replace unknown country names
# print(DataOutbreak$NAME[DataOutbreak$NameCheck])
# DataName <- c(
#      "Bolivia" = "Bolivia (Plurinational State of)",
#      "Czech Republic" = "Czechia",
#      "Macau" = "China, Macao SAR",
#      "Netherlands" = "Netherlands (Kingdom of the)",
#      "Northern Ireland" = "United Kingdom of Great Britain and Northern Ireland",
#      "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
#      "United States" = "United States of America",
#      "Vietnam" = "Viet Nam"
# )
# DataOutbreak <- DataOutbreak |> 
#      mutate(NAME = recode(NAME, !!!DataName),
#             NameCheck = !NAME %in% DataVac$NAME) |> 
#      select(NAME, Outbreak) |> 
#      # remove duplicates
#      unique()

## Data from the World Health Organization (WHO)
## https://immunizationdata.who.int/global/wiise-detail-page/pertussis-reported-cases-and-incidence
## Pertussis incidence after 2010

DataInci <- read.xlsx("./Data/Pertussis incidence.xlsx")[,1:17]
DataInci <- DataInci |> 
     filter(!is.na(Disease)) |> 
     select(-c(Denominator, Disease)) |> 
     rename(NAME = `Country./.Region`) |> 
     filter(NAME %in% DataVac$NAME) |>
     pivot_longer(cols = -c(NAME),
                  names_to = 'YEAR',
                  values_to = 'Incidence') |>
     mutate(Period = case_when(
          YEAR <= 2019 ~ 'Pre-epidemic',
          YEAR <= 2021 ~ 'Epidemic',
          YEAR == 2022 ~ '2022',
          YEAR == 2023 ~ '2023'),
          Period = factor(Period, levels = c('Pre-epidemic', 'Epidemic', '2023', '2022')),
          Incidence = as.numeric(Incidence),
          Incidence = if_else(is.na(Incidence), Incidence, Incidence +0.001)
     ) |> 
     group_by(NAME, Period) |>
     summarise(Incidence_50 = median(Incidence, na.rm = T),
               Incidence_25 = quantile(Incidence, 0.25, na.rm = T),
               Incidence_75 = quantile(Incidence, 0.75, na.rm = T),
               IQR = Incidence_75 - Incidence_25,
               .groups = 'drop') |> 
     pivot_wider(names_from = Period,
                 values_from = c(Incidence_50, Incidence_25, Incidence_75, IQR),
                 names_glue = "{Period}_{.value}") |> 
     select(NAME,
            `Pre-epidemic_Incidence_50`, `Pre-epidemic_Incidence_25`, `Pre-epidemic_Incidence_75`, `Pre-epidemic_IQR`,
            `2022_Incidence_50`, `2023_Incidence_50`) |>
     rename(`2022` = `2022_Incidence_50`,
            `2023` = `2023_Incidence_50`) |>
     mutate(OutbreakSize2022 = case_when(`2022` <= `Pre-epidemic_Incidence_25` ~ 'Low',
                                         `2022` > `Pre-epidemic_Incidence_25` & `2022` <= `Pre-epidemic_Incidence_75` ~ 'Normal',
                                         `2022` > `Pre-epidemic_Incidence_75` & 
                                              `2022` <= `Pre-epidemic_Incidence_75` + 1.5*`Pre-epidemic_IQR` ~ 'High',
                                         `2022` > `Pre-epidemic_Incidence_75` + 1.5*`Pre-epidemic_IQR` ~ 'Resurgence',
                                         TRUE ~ 'Unavailable'),
            OutbreakSize2023 = case_when(`2023` <= `Pre-epidemic_Incidence_25` ~ 'Low',
                                         `2023` > `Pre-epidemic_Incidence_25` & `2023` <= `Pre-epidemic_Incidence_75` ~ 'Normal',
                                         `2023` > `Pre-epidemic_Incidence_75` & 
                                              `2023` <= `Pre-epidemic_Incidence_75` + 1.5*`Pre-epidemic_IQR` ~ 'High',
                                         `2023` > `Pre-epidemic_Incidence_75` + 1.5*`Pre-epidemic_IQR` ~ 'Resurgence',
                                         TRUE ~ 'Unavailable'),
            `2023-change` = `2023`/`Pre-epidemic_Incidence_50`,
            `2022-change` = `2022`/`Pre-epidemic_Incidence_50`,
            OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Unavailable', 'Low', 'Normal', 'High', 'Resurgence')),
            OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Unavailable', 'Low', 'Normal', 'High', 'Resurgence')),
            OutbreakSize = case_when(
                 OutbreakSize2022 == 'Resurgence' | OutbreakSize2023 == 'Resurgence' ~ 3,
                 OutbreakSize2022 == 'High' | OutbreakSize2023 == 'High' ~ 2,
                 OutbreakSize2022 == 'Normal' | OutbreakSize2023 == 'Normal' ~ 1,
                 OutbreakSize2022 == 'Low' | OutbreakSize2023 == 'Low' ~ 0,
                 TRUE ~ NA_real_
            )) |> 
     select(NAME, OutbreakSize2022, OutbreakSize2023, OutbreakSize,
            `Pre-epidemic_Incidence_50`, `Pre-epidemic_Incidence_25`, `Pre-epidemic_Incidence_75`, `Pre-epidemic_IQR`,
            `2022`, `2022-change`, `2023`, `2023-change`) |> 
     rename(IncidencePre = `Pre-epidemic_Incidence_50`,
            IncidencePre25 = `Pre-epidemic_Incidence_25`,
            IncidencePre75 = `Pre-epidemic_Incidence_75`,
            IncidencePreIQR = `Pre-epidemic_IQR`,
            Incidence2022 = `2022`,
            Incidence2023 = `2023`,
            Change2022 = `2022-change`,
            Change2023 = `2023-change`) |> 
     mutate(IncidencePre = IncidencePre - 0.001,
            IncidencePre25 = IncidencePre25 - 0.001,
            IncidencePre75 = IncidencePre75 - 0.001,
            Incidence2022 = Incidence2022 - 0.001,
            Incidence2023 = Incidence2023 - 0.001,
            Change2022 = round(Change2022, 2),
            Change2023 = round(Change2023, 2))

## Merge all data
# Data <- DataVac |> 
#      merge(DataOutbreak, by = c('NAME'), all = T) |>
#      mutate(OutbreakNews = replace_na(Outbreak, 0)) |>
#      merge(DataInci, by = c('NAME'), all = T) |>
#      select(-Outbreak)
Data <- DataVac |> 
     merge(DataInci, by = c('NAME'), all = T) |> 
     left_join(DataInfo, by = c('CODE', 'NAME')) |> 
     select(WHO_REGION, CODE, NAME, everything())

write.csv(Data, './Outcome/S table1.csv', row.names = F)

print("Data cleaned successfully!")
