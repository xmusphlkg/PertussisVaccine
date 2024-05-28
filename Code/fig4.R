
# This script generates Figure 4 in the manuscript.
# To visual the distribution of DTP1 and DTP3 vaccine coverage, vaccination schedule,
# targeting children, adult vaccination, and pregnant vaccination in the world.

# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(openxlsx)
library(sf)
library(cowplot)
library(paletteer)
library(Cairo)

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table1.csv") 

DataAll <- DataAll |> 
     mutate(
          VaccineGeneral = factor(VaccineGeneral,
                                  levels = c('1 dose', paste(c(2:6), 'doses'))),
          TimeLastShotG = case_when(
               TimeLastShot <= 52 ~ '-1',
               TimeLastShot <= 104 ~ '-2',
               TimeLastShot <= 156 ~ '-3',
               TimeLastShot <= 312 ~ '-6',
               TimeLastShot <= 416 ~ '-8',
               TimeLastShot <= 624 ~ '-12',
               TimeLastShot > 625 ~ '12+',
               TRUE ~ NA),
          TimeLastShotG = factor(TimeLastShotG, levels = c('-1', '-2', '-3', '-6', '-8', '-12', '12+')),
          VaccineAdultRisk = case_when(
               VaccineAdult == 0 & VaccineRisk == 0 ~ 'No',
               VaccineAdult == 1 & VaccineRisk == 0 ~ 'Only adult',
               VaccineAdult == 0 & VaccineRisk == 1 ~ 'Only risk',
               VaccineAdult == 1 & VaccineRisk == 1 ~ 'Adult and risk',
               TRUE ~ NA_character_),
          VaccineAdultRisk = factor(VaccineAdultRisk,
                                    levels = c('No', 'Only risk', 'Only adult', 'Adult and risk')),
          VaccinePregnant = factor(VaccinePregnant,
                                   levels = c(0:1),
                                   labels = c('No', 'Yes')))

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

# find the country not in Map data
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataMapPlot <- DataMap |> 
     left_join(DataAll, by = c('iso_a3' = 'CODE'))

## panel a&b ----------------------------------------------------------------

fill_color <- paletteer_d("MexBrewer::Revolucion")

fig_1_m <- plot_map_density(DataMapPlot$CoverageDTP1, fill_color)+
     labs(title = 'DTP-containing\nvaccine, 1st dose')

fig_1 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = CoverageDTP1)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 1),
                          breaks = seq(0, 1, 0.1),
                          labels = scales::percent_format(accuracy = 1),
                          na.value = "grey")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     guides(fill = guide_colorbar(barwidth = 30)) +
     labs(title = "A", x = NULL, y = NULL, fill = 'DTP vaccine\nCoverage(%)')

fig_1 <- fig_1 + inset_element(fig_1_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

fig_2_m <- plot_map_density(DataMapPlot$CoverageDTP3, fill_color)+
     labs(title = 'DTP-containing\nvaccine, 3rd dose')

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = CoverageDTP3)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 1),
                          breaks = seq(0, 1, 0.1),
                          labels = scales::percent_format(accuracy = 1),
                          na.value = "grey50")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     guides(fill = guide_colorbar(barwidth = 30)) +
     labs(title = "B", x = NULL, y = NULL, fill = 'DTP vaccine\nCoverage(%)')

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel c -----------------------------------------------------------------

fill_color <- rev(paletteer_d("awtools::a_palette"))

fig_3_m <- plot_map_col(DataAll$VaccineGeneral, fill_color) +
     scale_fill_manual(values = fill_color[c(1:4, 6:8)],
                       breaks = levels(DataMapPlot$VaccineGeneral),
                       limits = levels(DataMapPlot$VaccineGeneral))

fig_3 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineGeneral)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[c(1:4, 6:8)],
                       na.translate = F,
                       breaks = levels(DataMapPlot$VaccineGeneral),
                       limits = levels(DataMapPlot$VaccineGeneral))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "C", x = NULL, y = NULL, fill = 'Vaccination schedule')+
     guides(fill = guide_legend(nrow = 1))

fig_3 <- fig_3 + inset_element(fig_3_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

## panel d -----------------------------------------------------------------

fig_4_m <- plot_map_col(DataAll$TimeLastShotG, fill_color) +
     scale_fill_manual(values = fill_color[c(1:4, 6:8)])

fig_4 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = TimeLastShotG)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[c(1:4, 6:8)],
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "D", x = NULL, y = NULL, fill = 'Targeting children (years)')+
     guides(fill = guide_legend(nrow = 1))

fig_4 <- fig_4 + inset_element(fig_4_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

## panel e -----------------------------------------------------------------

fill_color <- paletteer_d("MetBrewer::Egypt")[c(1, 4, 2, 3)]

fig_5_m <- plot_map_col(DataAll$VaccineAdultRisk, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = c('No', 'Only risk', 'Only adult', 'Adult and risk'),
                       limits = c('No', 'Only risk', 'Only adult', 'Adult and risk'),
                       na.translate = F)

fig_5 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineAdultRisk)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.translate = F,
                       na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "E", x = NULL, y = NULL, fill = 'Adult vaccination')+
     guides(fill = guide_legend(nrow = 1))

fig_5 <- fig_5 + inset_element(fig_5_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

## panel f -------------------------------------------------------------

fill_color <- paletteer_d("MetBrewer::Egypt")[c(1, 3)]

fig_6_m <- plot_map_col(DataAll$VaccinePregnant, fill_color) +
     scale_fill_manual(values = fill_color,
                       na.translate = F)

fig_6 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccinePregnant)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.translate = F,
                       na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "F", x = NULL, y = NULL, fill = 'Pregnant vaccination')+
     guides(fill = guide_legend(nrow = 1))

fig_6 <- fig_6 + inset_element(fig_6_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

# combine all figures -----------------------------------------------------

fig <- cowplot::plot_grid(
     fig_1, fig_2, fig_3, fig_4, fig_5, fig_6,
     ncol = 2, align = 'hv', rel_widths = c(1, 1, 1)
)

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 14,
       height = 11.5,
       device = cairo_pdf,
       family = "Helvetica")
