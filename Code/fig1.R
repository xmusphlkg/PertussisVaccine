
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
library(scales)

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table1.csv") 

DataAll <- DataAll |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineGeneral = factor(VaccineGeneral,
                                    levels = 3:6,
                                    labels = as.character(3:6)),
            VaccineGeneral = factor(VaccineGeneral,
                                    levels = as.character(3:6)),
            GENERALY = case_when(GENERALY > 1 ~ paste0(GENERALY, ' doses'),
                                 GENERALY == 1 ~ '1 dose',
                                 GENERALY == 0 ~ 'Not provided',
                                 TRUE ~ 'Unavailable'),
            GENERALY = factor(GENERALY,
                              levels = c('Not provided', '1 dose', '2 doses', '3 doses')),
            GENERALM = paste0(GENERALM, ' doses'),
            GENERALM = factor(GENERALM,
                              levels = c('3 doses', '4 doses', '5 doses', '6 doses')),
            VaccineAP = if_else(VaccineCode %in% c('aP', 'Both'), 1, 0),
            VaccineWP = if_else(VaccineCode %in% c('wP', 'Both'), 1, 0),
            TimeLastShotG = case_when(
                 TimeLastShot < 12 ~ '<1',
                 TimeLastShot < 24 ~ '<2',
                 TimeLastShot < 12*6 ~ '<6',
                 TimeLastShot < 12*12 ~ '<12',
                 TimeLastShot >= 12*12 ~ '12+',
                 TRUE ~ NA),
            TimeLastShotG = factor(TimeLastShotG, levels = c('<1', '<2', '<6', '<12', '12+')),
            VaccineAdultRisk = case_when(
                 VaccineAdult == 0 & VaccineRisk == 0 ~ 'No',
                 VaccineAdult == 1 & VaccineRisk == 0 ~ 'Adult',
                 VaccineAdult == 0 & VaccineRisk == 1 ~ 'Risk',
                 VaccineAdult == 1 & VaccineRisk == 1 ~ 'Both',
                 TRUE ~ NA_character_),
            VaccineAdultRisk = factor(VaccineAdultRisk,
                                      levels = c('No', 'Risk', 'Adult', 'Both')),
            VaccinePregnant = factor(VaccinePregnant,
                                     levels = c(0:1),
                                     labels = c('No', 'Yes')))

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

# find the country not in Map data
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataMapPlot <- DataMap |> 
     left_join(DataAll, by = c('iso_a3' = 'CODE'))

## panel a -----------------------------------------------------------------

fill_color <- c("#DD5129FF", "#FAB255FF", "#0F7BA2FF", "#43B284FF")

fig_1_m <- plot_map_col(DataAll$VaccineGeneral, fill_color) +
     scale_fill_manual(values = fill_color[1:4],
                       breaks = levels(DataMapPlot$VaccineGeneral),
                       limits = levels(DataMapPlot$VaccineGeneral)) +
     labs(title = 'Vccination schedule\n(doses)')

fig_1 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineGeneral)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[1:4],
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccineGeneral),
                       limits = levels(DataMapPlot$VaccineGeneral))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "A", x = NULL, y = NULL)+
     guides(fill = guide_legend(nrow = 1))

fig_1 <- fig_1 + inset_element(fig_1_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel b -----------------------------------------------------------------

fill_color <- c("#DD5129FF", "#FAB255FF", "#30B4CCFF", "#0F7BA2FF", "#43B284FF")

fig_2_m <- plot_map_col(DataAll$TimeLastShotG, fill_color) +
     scale_fill_manual(values = fill_color[1:5]) +
     labs(title = 'Targeting children\n(years)')

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = TimeLastShotG)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[1:5],
                       na.value = "white",
                       breaks = levels(DataMapPlot$TimeLastShotG),
                       limits = levels(DataMapPlot$TimeLastShotG))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL)+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel c -----------------------------------------------------------------

fill_color <- c("#DD5129FF", "#FAB255FF", "#0F7BA2FF", "#43B284FF")

fig_3_m <- plot_map_col(DataAll$VaccineAdultRisk, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = c('No', 'Risk', 'Adult', 'Both'),
                       limits = c('No', 'Risk', 'Adult', 'Both'),
                       na.translate = F)+
     labs(title = 'Providing vaccine\nfor adults & risk groups')

fig_3 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineAdultRisk)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccineAdultRisk),
                       limits = levels(DataMapPlot$VaccineAdultRisk))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "C", x = NULL, y = NULL)+
     guides(fill = guide_legend(nrow = 1))

fig_3 <- fig_3 + inset_element(fig_3_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel d -------------------------------------------------------------

fill_color <- fill_color[c(1, 4)]

fig_4_m <- plot_map_col(DataAll$VaccinePregnant, fill_color) +
     scale_fill_manual(values = fill_color,
                       na.translate = F)+
     labs(title = 'Provided vaccine\nfor pregnant woman')

fig_4 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccinePregnant)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccinePregnant),
                       limits = levels(DataMapPlot$VaccinePregnant))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "D", x = NULL, y = NULL)+
     guides(fill = guide_legend(nrow = 1))

fig_4 <- fig_4 + inset_element(fig_4_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel e ----------------------------------------------------------------

fill_color <- paletteer_d("MetBrewer::Hiroshige")

fig_5_m <- plot_map_density(DataMapPlot$CoverageDTP1, fill_color)+
     labs(title = 'DTP-containing\nvaccine, 1st dose')

fig_5 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = CoverageDTP1)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0.3, 1),
                          breaks = c(0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 1),
                          labels = scales::percent_format(accuracy = 1),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     guides(fill = guide_colorbar(barwidth = 30)) +
     labs(title = "E", x = NULL, y = NULL)

fig_5 <- fig_5 + inset_element(fig_5_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel f -----------------------------------------------------------------

fig_6_m <- plot_map_density(DataMapPlot$CoverageDTP3, fill_color)+
     labs(title = 'DTP-containing\nvaccine, 3rd dose')

fig_6 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = CoverageDTP3)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0.3, 1),
                          breaks = c(0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 1),
                          labels = scales::percent_format(accuracy = 1),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     guides(fill = guide_colorbar(barwidth = 30)) +
     labs(title = "F", x = NULL, y = NULL, fill = 'DTP vaccine\nCoverage(%)')

fig_6 <- fig_6 + inset_element(fig_6_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

# combine all figures -----------------------------------------------------

fig <- cowplot::plot_grid(
     fig_1, fig_2, fig_3, fig_4, fig_5, fig_6,
     ncol = 2, align = 'hv', rel_widths = c(1, 1, 1)
)

ggsave("./Outcome/fig1.pdf",
       fig,
       width = 15,
       height = 11,
       device = cairo_pdf)

# content -----------------------------------------------------------------

print('################## Figure 1 ##################')
# print Coverages of DTP1 and DTP3 vaccines more than 90% in some countries.
print(paste(length(which(!is.na(DataAll$CoverageDTP1))), 'countries have DTP1 vaccine coverage data.'))
