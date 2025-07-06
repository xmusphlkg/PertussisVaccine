
# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(openxlsx)
library(sf)
library(Cairo)
library(randomForest)
library(ggrepel)
library(paletteer)

source('./Code/function.R')

# data --------------------------------------------------------------------

# Load the map data
DataMap <- st_read("./Data/world.zh.json",
                    quiet = TRUE) |> 
     filter(iso_a3  != "ATA")

# Load Country code
DataCountry <- read.csv('./Data/GBD/iso_code.csv') |> 
     select(-location_name)

# Load GBD data
DataInciGBD <- read.csv("./Data/GBD/IHME-GBD_2021_DATA-a5bddb9a-1.csv") |> 
     filter(measure_name %in% c('Incidence', 'DALYs (Disability-Adjusted Life Years)')) |>
     select(location_id, measure_name, age_name, metric_name, year, val) |> 
     left_join(DataCountry, by = c('location_id'))

# Load Country code
DataCountry1 <- read.xlsx("./Data/DTP vaccination coverage.xlsx") |> 
     select(CODE, NAME) |>
     unique()

# Load reported data
DataInciReport <- read.xlsx("./Data/Pertussis reported cases and incidence 2025-12-02 17-37 UTC.xlsx")[,1:17] |> 
     filter(!is.na(Disease)) |> 
     select(-c(Disease)) |> 
     mutate(# replace &#39; with '
          `Country./.Region` = str_replace_all(`Country./.Region`, '&#39;', "'")) |>
     rename(NAME = `Country./.Region`) |> 
     pivot_longer(cols = -c(NAME),
                  names_to = 'YEAR',
                  values_to = 'Incidence') |>
     # filter year >= 2010
     filter(as.numeric(YEAR) >= 2010) |>
     mutate(Incidence = as.numeric(str_replace(Incidence, ',', ''))) |> 
     left_join(DataCountry1, by = c('NAME'))

rm(DataCountry1)

# Vaccine coverage and strategy
DataVaccine <- read.csv("./Outcome/S table2.csv") |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineAP = as.factor(VaccineCode %in% c('aP', 'Both')),
            VaccineWP = as.factor(VaccineCode %in% c('wP', 'Both')),
            OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Low', 'Normal', 'High', 'Resurgence')),
            OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Low', 'Normal', 'High', 'Resurgence')))

# appendix ---------------------------------------------------

# Compare GBD and reported data
DataGBD <- DataInciGBD |> 
     filter(age_name == 'All ages',
            measure_name == 'Incidence',
            metric_name == 'Number') |>
     select(-c(age_name, measure_name, metric_name))
            
plot_point <- function(y){
     data <- DataGBD |> 
          filter(year == y) |>
          left_join(filter(DataInciReport, YEAR == y), by = c('ISO3' = 'CODE')) |> 
          select(InciGBD = val, InciReport = Incidence, NAME, ISO3) |> 
          # find outlier countries and add label
          mutate(labelGBD = case_when(InciGBD > median(InciGBD, na.rm = T) + 10 * IQR(InciGBD, na.rm = T) ~ NAME,
                                     TRUE ~ ''),
                 labelReport = case_when(InciReport > median(InciReport, na.rm = T) + 10 * IQR(InciReport, na.rm = T) ~ NAME,
                                        TRUE ~ ''),
                 label = case_when(labelGBD != '' & labelReport != '' ~ ISO3,
                                   labelGBD != '' ~ ISO3,
                                   labelReport != '' ~ ISO3,
                                   TRUE ~ NA),
                 highlight = case_when(labelGBD != '' & labelReport != '' ~ '',
                                       labelGBD != '' ~ '',
                                       labelReport != '' ~ '',
                                       TRUE ~ NA))
     
     ggplot(data, aes(x = InciGBD, y = InciReport)) +
          geom_point(aes(color = highlight)) +
          geom_abline(intercept = 0, slope = 1, color = 'red') +
          geom_text_repel(aes(label = label), nudge_x = 0.1, nudge_y = 0.1) +
          scale_x_continuous(limits = c(0, NA),
                             expand = expansion(mult = c(0, 0.1))) +
          scale_y_continuous(limits = c(0, NA),
                             expand = expansion(mult = c(0, 0.1))) +
          labs(x = 'GBD estimated incidence',
               y = 'Reported Incidence',
               title = paste('Year:', y)) +
          theme_classic()+
          theme(legend.position = 'none')
}

fig <- lapply(unique(DataGBD$year), plot_point) |> 
     wrap_plots(nrow = 3)

ggsave("./Outcome/S fig4_1.png",
       fig,
       width = 15,
       height = 12,
       dpi = 300)

rm(DataGBD, plot_point, fig)

# incidence -------------------------------------------------------

DataGBD2021 <- DataInciGBD |> 
     filter(year == 2021,
            measure_name == 'Incidence',
            metric_name == 'Rate') |>
     select(ISO3, age_name, val) |> 
     group_by(ISO3, age_name) |>
     summarise(val = median(val, na.rm = T),
               .groups = 'drop') |>
     pivot_wider(names_from = age_name,
                 values_from = val)

Data <- DataMap |> 
     left_join(DataGBD2021, by = c('iso_a3' = 'ISO3')) |> 
     rename(y = 'All ages') 

fill_color <- paletteer_d("MetBrewer::Hiroshige", direction = -1)

fig_1 <- ggplot(Data) +
     geom_sf(aes(fill = y)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 1000),
                          expand = c(0, 0),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'inside',
           legend.position.inside = c(0.01, 0.01),
           legend.justification.inside = c(0, 0),
           plot.title.position = 'plot') +
     labs(title = "A", x = NULL, y = NULL, fill = 'Estimated\nincidence rate')

# RF model ----------------------------------------------------------------

DataLabel <- data.frame(
     Variable = c("CoverageDTP1", "CoverageDTP3",
                  "VaccinePregnant", "VaccineAdult", "VaccineRisk",
                  'VaccineAP, VaccineWP',
                  "TimeLastShot", "TimeFirstShot",
                  "VaccineAP", "VaccineWP"),
     text = c("DTP1 coverage", "DTP3 coverage", 
              "Maternal immunization", "Vaccine for adult", "Vaccine for risk",
              "aP vaccine, wP vaccine",
              "Time of last shot", "Time of first shot",
              "aP vaccine", "wP vaccine")
)

age_group <- c("All ages", "<1 year")

Data <- DataVaccine |> 
     mutate(VaccineAP = as.numeric(VaccineAP == 'TRUE'),
            VaccineWP = as.numeric(VaccineWP == 'TRUE'))  |>
     mutate_at(vars(VaccinePregnant, VaccineAdult, VaccineRisk, VaccineAP, VaccineWP), as.character) |>
     filter(!is.na(OutbreakSize) & !is.infinite(OutbreakSize))  |>
     select(-c(VaccineCode)) |> 
     left_join(DataGBD2021, by = c('CODE' = 'ISO3')) |>
     na.omit()  |>
     select(CoverageDTP1, CoverageDTP3,
            TimeLastShot, TimeFirstShot,
            VaccinePregnant, VaccineAdult,
            VaccineRisk, VaccineAP, VaccineWP,
            names(DataGBD2021)[-1])

rf_model <- function(i, start_i){
     set.seed(20250303)
     
     y <- age_group[i]
     
     data <- Data |> 
          select(CoverageDTP1:VaccineWP, all_of(y)) |> 
          rename(y = y)
     
     # Random forest
     rf_model <- randomForest(y ~ .,
                        data = data,
                        ntree = 100000,
                        importance = T)
     
     importance_data <- importance(rf_model, scale = TRUE, type = 1)
     importance_df <- data.frame(Variable = rownames(importance_data), Importance = importance_data[,1]) |> 
          arrange(desc(Importance)) |> 
          left_join(DataLabel, by = 'Variable') |> 
          mutate(age = y)
     print(paste(LETTERS[start_i + i], y, sep = ': '))
     print(importance_df)
     
     ggplot(importance_df, aes(x = Importance, y = fct_reorder(Variable, Importance))) +
          geom_linerange(aes(xmin = 0, xmax = Importance)) +
          geom_point(mapping = aes(color = Importance),
                     size = 3) +
          scale_y_discrete(breaks = importance_df$Variable,
                           labels = importance_df$text) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                             limits = c(0, NA)) +
          scale_color_gradientn(colors = paletteer_d("MetBrewer::Hiroshige", direction = -1))+
          theme_bw() +
          theme(panel.grid = element_blank(),
                legend.position = 'none',
                axis.text = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                plot.title.position = 'plot') +
          labs(title = paste(LETTERS[start_i + i], y, sep = ': '),
               y = NULL, x = "Mean decrease in accuracy")
}

fig_2 <- lapply(1:length(age_group), rf_model, start_i = 1) |>
     wrap_plots(nrow = 1)

# DALYs ------------------------------------------------------------------------

DataGBD2021 <- DataInciGBD |> 
     filter(year == 2021,
            measure_name == 'DALYs (Disability-Adjusted Life Years)',
            metric_name == 'Rate') |>
     select(ISO3, age_name, val) |> 
     pivot_wider(names_from = age_name,
                 values_from = val)

Data <- DataMap |>
     left_join(DataGBD2021, by = c('iso_a3' = 'ISO3')) |>
     rename(y = 'All ages')

fig_3 <- ggplot(Data) +
     geom_sf(aes(fill = y)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 1000),
                          expand = c(0, 0),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'inside',
           legend.position.inside = c(0.01, 0.01),
           legend.justification.inside = c(0, 0),
           plot.title.position = 'plot') +
     labs(title = "D", x = NULL, y = NULL, fill = 'Estimated\nDALYs rate')

# RF model ----------------------------------------------------------------

Data <- DataVaccine |> 
     mutate(VaccineAP = as.numeric(VaccineAP == 'TRUE'),
            VaccineWP = as.numeric(VaccineWP == 'TRUE'))  |>
     mutate_at(vars(VaccinePregnant, VaccineAdult, VaccineRisk, VaccineAP, VaccineWP), as.character) |>
     filter(!is.na(OutbreakSize) & !is.infinite(OutbreakSize))  |>
     select(-c(VaccineCode)) |> 
     left_join(DataGBD2021, by = c('CODE' = 'ISO3')) |>
     na.omit()  |>
     select(CoverageDTP1, CoverageDTP3,
            TimeLastShot, TimeFirstShot,
            VaccinePregnant, VaccineAdult,
            VaccineRisk, VaccineAP, VaccineWP,
            names(DataGBD2021)[-1])

fig_4 <- lapply(1:length(age_group), rf_model, start_i = 4) |>
     wrap_plots(nrow = 1)

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig_1, fig_2, fig_3, fig_4, nrow = 2, byrow = T)

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 15,
       height = 7,
       device = cairo_pdf)

ggsave("./Outcome/fig4.png",
       fig,
       width = 15,
       height = 7)

write.csv(Data,
          "./Outcome/fig data/fig4.csv",
          row.names = F)
