
# This script generates Figure 3 in the manuscript.
# To determine the pertussis status in 2022 and 2023, 
# we used the median incidence in the pre-epidemic period as the threshold.

# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(openxlsx)
library(sf)
library(cowplot)
library(paletteer)
library(Cairo)
library(ggpubr)

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table2.csv")|> 
     mutate(
          OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Unavailable', 'Low', 'Normal', 'High', 'Resurgence')),
          OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Unavailable', 'Low', 'Normal', 'High', 'Resurgence'))
     )

DataInci <- read.xlsx("./Data/Pertussis reported cases and incidence 2025-12-02 17-37 UTC.xlsx")[,1:17]|> 
     filter(!is.na(Disease)) |> 
     select(-c(Disease)) |> 
     rename(NAME = `Country./.Region`) |> 
     pivot_longer(cols = -c(NAME),
                  names_to = 'YEAR',
                  values_to = 'Incidence') |>
     mutate(Period = case_when(
          YEAR <= 2019 ~ 'Pre-epidemic',
          YEAR <= 2021 ~ 'Epidemic',
          YEAR == 2022 ~ '2022',
          YEAR == 2023 ~ '2023'),
          Period = factor(Period, levels = c('Pre-epidemic', 'Epidemic', '2023', '2022')),
          Incidence = as.numeric(Incidence)
     )

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

# find the country not in Map data
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataMapPlot <- DataMap |> 
     left_join(DataAll, by = c('iso_a3' = 'CODE'))

# write csv
data <- DataAll |> 
     select(WHO_REGION, NAME, CODE, 
            IncidencePre, IncidencePre25, IncidencePre75, IncidencePreIQR,
            Incidence2022, OutbreakSize2022, 
            Incidence2023, OutbreakSize2023)

write.csv(data, './Outcome/fig data/fig3.csv', row.names = F)

## panel b&c -----------------------------------------------------------------

fill_color <- c("grey50", '#43B284FF', '#0F7BA2FF', '#FAB255FF', '#DD5129FF')

fig_1_m <- plot_map_col(DataAll$OutbreakSize2022, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$OutbreakSize2022),
                       limits = levels(DataAll$OutbreakSize2022),
                       na.translate = F)+
     theme(axis.text.x = element_blank())

fig_1 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = factor(OutbreakSize2022)),
             show.legend = F) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$OutbreakSize2022),
                       limits = levels(DataAll$OutbreakSize2022),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "A", x = NULL, y = NULL, fill = 'Pertussis status in 2022')+
     guides(fill = guide_legend(nrow = 1))

fig_1 <- fig_1 + inset_element(fig_1_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

fig_2_m <- plot_map_col(DataAll$OutbreakSize2023, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$OutbreakSize2022),
                       limits = levels(DataAll$OutbreakSize2022),
                       na.translate = F)+
     theme(axis.text.x = element_blank())

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = factor(OutbreakSize2023))) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$OutbreakSize2022),
                       limits = levels(DataAll$OutbreakSize2022),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL, fill = 'Pertussis status')+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

# combine -----------------------------------------------------------------

# design <- "
# AB
# CC
# DD
# "
# 
# fig_r <- fig_0_1 + fig_0_2 + fig_1 + fig_2 +
#      plot_layout(design = design, widths = c(3, 1.1))

fig <- cowplot::plot_grid(fig_1, fig_2, nrow = 2)

ggsave("./Outcome/fig3.pdf",
       fig,
       width = 6,
       height = 7,
       device = cairo_pdf)
