
# This script generates Figure 2 in the manuscript.
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

DataAll <- read.csv("./Outcome/S table2.csv") 

DataAll <- DataAll |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineAP = if_else(VaccineCode %in% c('aP', 'Both'), 1, 0),
            VaccineWP = if_else(VaccineCode %in% c('wP', 'Both'), 1, 0),
            TimeFirstShotG = case_when(
                 TimeFirstShot < 2 ~ '[1,2)',
                 TimeFirstShot < 3 ~ '[2,3)',
                 TimeFirstShot == 3 ~ '3+',
                 TRUE ~ NA),
            TimeFirstShotG = factor(TimeFirstShotG, levels = c('[1,2)', '[2,3)', '3+')),
            TimeLastShotG = case_when(
                 TimeLastShot < 12 ~ '<1',
                 TimeLastShot < 24 ~ '[1,2)',
                 TimeLastShot < 12*6 ~ '[2,6)',
                 TimeLastShot < 12*12 ~ '[6,12)',
                 TimeLastShot >= 12*12 ~ '12+',
                 TRUE ~ NA),
            TimeLastShotG = factor(TimeLastShotG, levels = c('<1', '[1,2)', '[2,6)', '[6,12)', '12+')),
            TimeShotG = paste(TimeFirstShotG, TimeLastShotG, sep = ' '),
            VaccineDoseG = as.character(VaccineDose),
            VaccineDoseG = factor(VaccineDoseG, levels = c('3', '4', '5', '6')),
            VaccineCodeG = factor(VaccineCode, levels = c('aP', 'wP', 'Both'))) |> 
     select(CODE, NAME, VaccineCodeG, VaccineDoseG,
            TimeFirstShot, TimeLastShot, TimeFirstShotG, TimeLastShotG, TimeShotG,
            CoverageDTP1, CoverageDTP3)

DataMap <- st_read("./Data/world.zh.json",
                  quiet = TRUE) |> 
     filter(iso_a3  != "ATA")

# find the country not in Map data
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataMapPlot <- DataMap |> 
     left_join(DataAll, by = c('iso_a3' = 'CODE'))

## panel a -----------------------------------------------------------------

table(DataAll$TimeShotG)

fill_color <- c("#DD5129FF", "#FAB255FF", "#43B284FF") |> rev()

fig_1_m <- plot_map_col(DataAll$TimeFirstShotG, fill_color) +
     scale_fill_manual(values = fill_color) +
     labs(title = 'Targeting children\n(months)')

fig_1 <- ggplot(data = DataMapPlot)+
     geom_sf(aes(fill = TimeFirstShotG)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[1:5],
                       na.value = "white",
                       breaks = levels(DataMapPlot$TimeFirstShotG),
                       limits = levels(DataMapPlot$TimeFirstShotG))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
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
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL)+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel c -----------------------------------------------------------------

table(DataAll$VaccineDoseG)

fill_color <- c("#DD5129FF", "#FAB255FF", "#0F7BA2FF", "#43B284FF")

fig_3_m <- plot_map_col(DataAll$VaccineDoseG, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = c('3', '4', '5', '6'),
                       limits = c('3', '4', '5', '6')) +
     labs(title = 'Vaccine dose')

fig_3 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineDoseG)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccineDoseG),
                       limits = levels(DataMapPlot$VaccineDoseG))+
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

## panel d ----------------------------------------------------------------

fill_color <- paletteer_d("MetBrewer::Hiroshige")

fig_4_m <- plot_map_density(DataMapPlot$CoverageDTP1, fill_color)+
     labs(title = 'Vaccine coverage,\n1st dose')

fig_4 <- ggplot(data = DataMapPlot) +
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
     labs(title = "D", x = NULL, y = NULL)

fig_4 <- fig_4 + inset_element(fig_4_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel e -----------------------------------------------------------------

fig_5_m <- plot_map_density(DataMapPlot$CoverageDTP3, fill_color)+
     labs(title = 'Vaccine coverage,\n3rd dose')

fig_5 <- ggplot(data = DataMapPlot) +
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
     labs(title = "E", x = NULL, y = NULL, fill = 'DTP vaccine\nCoverage(%)')

fig_5 <- fig_5 + inset_element(fig_5_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

## panel f -------------------------------------------------------------

fill_color <- c("#DD5129FF", "#FAB255FF", "#43B284FF")

fig_6_m <- plot_map_col(DataAll$VaccineCodeG, fill_color) +
     scale_fill_manual(values = fill_color,
                       na.translate = F)+
     labs(title = 'Vaccine type')

fig_6 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccineCodeG)) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccineCodeG),
                       limits = levels(DataMapPlot$VaccineCodeG))+
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


fig_6 <- fig_6 + inset_element(fig_6_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

# combine all figures -----------------------------------------------------

fig <- cowplot::plot_grid(
     fig_1, fig_2, fig_3, fig_4, fig_5, fig_6,
     ncol = 2, align = 'hv', rel_widths = c(1, 1, 1)
)

ggsave("./Outcome/fig2.pdf",
       fig,
       width = 15,
       height = 11,
       device = cairo_pdf)

ggsave("./Outcome/fig2.png",
       fig,
       width = 15,
       height = 11)

write.csv(DataAll,
          "./Outcome/fig data/fig2.csv",
          row.names = F)

# content -----------------------------------------------------------------

print('################## Figure 2 ##################')
# print Coverages of DTP1 and DTP3 vaccines more than 90% in some countries.
print(paste(length(which(!is.na(DataAll$CoverageDTP1))), 'countries have DTP1 vaccine coverage data.'))
