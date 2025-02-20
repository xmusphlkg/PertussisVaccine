
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

DataVacScheduleRaw <- read.xlsx("./Data/Vaccination schedule for Pertussis 2025-21-01 12-27 UTC.xlsx") |> 
     filter(!is.na(COUNTRYNAME))
DataInfo <- DataVacScheduleRaw |> 
     select(ISO_3_CODE, COUNTRYNAME, WHO_REGION) |>
     unique() |>
     rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME)
DataVacSchedule <- DataVacScheduleRaw |> 
     select(ISO_3_CODE, COUNTRYNAME, VACCINECODE, SCHEDULEROUNDS, TARGETPOP_DESCRIPTION, TARGETPOP,
            AGEADMINISTERED, VACCINEDESCRIPTIONSHORT, YEAR, SCHEDULERCODE) |>
     rename(CODE = ISO_3_CODE, NAME = COUNTRYNAME)

# Use wP vaccine
DataVacWP <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% c('General/routine', 'Catch-up children')) |> 
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
DataAddition <- read.xlsx("./Data/Maternal pertussis immunization 2021.xlsx") |> 
     filter(!is.na(DISEASE)) |> 
     select(COUNTRYNAME) |>
     rename(NAME = COUNTRYNAME) |> 
     ## add CODE for the countries
     left_join(DataInfo, by = c('NAME')) |>
     select(CODE, NAME)

DataVacSchedulePreg <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION == 'Pregnant women') |> 
     select(CODE, NAME) |> 
     # rbind with the data from the additional file
     rbind(DataAddition) |> 
     unique() |> 
     group_by(CODE, NAME) |>
     summarise(PREGNANT = 1,
               .groups = 'drop')
     
# General/routine
DataVacScheduleGenM <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% c('General/routine'),
            # filter AGEADMINISTERED contains M or W
            str_detect(AGEADMINISTERED, '[MМ]|W')) |> 
     select(CODE, NAME, SCHEDULEROUNDS, AGEADMINISTERED, TARGETPOP) |>
     unique() |>
     mutate(SCHEDULEROUNDS = paste(SCHEDULEROUNDS, TARGETPOP, sep = '_')) |> 
     group_by(CODE, NAME) |>
     summarise(GENERALM = length(unique(SCHEDULEROUNDS)),
               .groups = 'keep') |> 
     select(CODE, NAME, GENERALM)

DataVacScheduleGenY <- DataVacSchedule |>
     filter(TARGETPOP_DESCRIPTION %in% c('General/routine'), 
            # filter AGEADMINISTERED contains Y
            str_detect(AGEADMINISTERED, 'Y')) |>
     select(CODE, NAME, SCHEDULEROUNDS, AGEADMINISTERED, TARGETPOP) |>
     unique() |> 
     mutate(SCHEDULEROUNDS = paste(SCHEDULEROUNDS, TARGETPOP)) |> 
     group_by(CODE, NAME) |>
     summarise(GENERALY = length(unique(SCHEDULEROUNDS)),
               .groups = 'keep') |> 
     select(CODE, NAME, GENERALY)

DataVacScheduleGen <- DataVacSchedule |> 
     filter(TARGETPOP_DESCRIPTION %in% c('General/routine')) |> 
     group_by(CODE, NAME) |>
     summarise(GENERALLASTSHOT = max(process_date_column(unique(AGEADMINISTERED), 'max')),
               GENERALFIRSTSHOT = min(process_date_column(unique(AGEADMINISTERED), 'min')),
               .groups = 'drop') |>
     # left_join(DataVacScheduleGenM, by = c('CODE', 'NAME')) |> 
     # left_join(DataVacScheduleGenY, by = c('CODE', 'NAME')) |> 
     # mutate(GENERALY = replace_na(GENERALY, 0),
     #        GENERAL = GENERALM + GENERALY) |> 
     arrange(CODE)

DataVacSchedule <- DataVacSchedule |> 
     select(CODE, NAME) |> 
     unique() |>
     full_join(DataVacSchedulePreg, by = c('CODE', 'NAME')) |>
     mutate(PREGNANT = replace_na(PREGNANT, 0)) |>
     full_join(DataVacScheduleAdult, by = c('CODE', 'NAME')) |>
     mutate(ADULT = replace_na(ADULT, 0)) |>
     full_join(DataVacScheduleRisk, by = c('CODE', 'NAME')) |>
     mutate(RISK = replace_na(RISK, 0)) |>
     full_join(DataVacScheduleGen, by = c('CODE', 'NAME')) |>
     rename(TimeFirstShot = GENERALFIRSTSHOT,
            TimeLastShot = GENERALLASTSHOT,
            VaccinePregnant = PREGNANT,
            # VaccineGeneral = GENERAL,
            VaccineAdult = ADULT,
            VaccineRisk = RISK)

remove(DataVacSchedulePreg, DataVacScheduleAdult, DataVacScheduleRisk,
       DataVacScheduleGen, DataVacScheduleGenM, DataVacScheduleGenY)

## Data from the World Health Organization (WHO)
## Merge all data
DataVac <- DataVacCover |> 
     merge(DataVacSchedule, by = c('CODE', 'NAME'), all = T) |>
     merge(DataVacWP, by = c('CODE', 'NAME'), all = T)

## Data from the World Health Organization (WHO)
## https://immunizationdata.who.int/global/wiise-detail-page/pertussis-reported-cases-and-incidence
## Pertussis incidence after 2010

DataInci <- read.xlsx("./Data/Pertussis reported cases and incidence 2025-12-02 17-37 UTC.xlsx")[,1:17]
DataInci <- DataInci |> 
     filter(!is.na(Disease)) |> 
     select(-c(Disease)) |> 
     mutate(# replace &#39; with '
          `Country./.Region` = str_replace_all(`Country./.Region`, '&#39;', "'"),
          # replace American Samoa with Samoa
          `Country./.Region` = str_replace(`Country./.Region`, 'American Samoa', 'Samoa')) |>
     rename(NAME = `Country./.Region`) |> 
     filter(NAME %in% DataVac$NAME) |>
     pivot_longer(cols = -c(NAME),
                  names_to = 'YEAR',
                  values_to = 'Incidence') |>
     # filter year >= 2010
     filter(as.numeric(YEAR) >= 2010) |>
     mutate(Period = case_when(YEAR <= 2019 ~ 'Pre-epidemic',
                               YEAR <= 2021 ~ 'Epidemic',
                               YEAR == 2022 ~ '2022',
                               YEAR == 2023 ~ '2023'),
            Period = factor(Period, levels = c('Pre-epidemic', 'Epidemic', '2023', '2022')),
            Incidence = as.numeric(str_replace(Incidence, ',', '')),
            Incidence = if_else(is.na(Incidence), Incidence, Incidence + 0.001)) |> 
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

DataClass <- read.xlsx('./Data/CLASS.xlsx') |> 
     select(Code, Region, Income.group) |> 
     mutate(Income.group = case_when(
          Income.group == 'Low income' ~ 'Low income',
          Income.group == 'Lower middle income' ~ 'Mild income',
          Income.group == 'Upper middle income' ~ 'Mild income',
          Income.group == 'High income' ~ 'High income',
          TRUE ~ NA_character_
     ))

Data <- DataVac |> 
     merge(DataInci, by = c('NAME'), all = T) |> 
     left_join(DataInfo, by = c('CODE', 'NAME')) |> 
     select(WHO_REGION, CODE, NAME, everything()) |> 
     left_join(DataClass, by = c(CODE = 'Code')) |> 
     mutate(Income.group = case_when(
          is.na(Income.group) ~ 'Low income',
          TRUE ~ Income.group
     ))

write.csv(Data, './Outcome/S table2.csv', row.names = F)

print("Data cleaned successfully!")
