
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
     mutate(VaccineAdultRisk = case_when(VaccineAdult == 0 & VaccineRisk == 0 ~ 'No',
                                         VaccineAdult == 1 & VaccineRisk == 0 ~ 'Adult',
                                         VaccineAdult == 0 & VaccineRisk == 1 ~ 'Risk',
                                         VaccineAdult == 1 & VaccineRisk == 1 ~ 'Both',
                                         TRUE ~ NA_character_),
            VaccineAdultRisk = factor(VaccineAdultRisk,
                                      levels = c('No', 'Risk', 'Adult', 'Both')),
            VaccinePregnant = factor(VaccinePregnant,
                                     levels = c(0:1),
                                     labels = c('No', 'Yes'))) |> 
     select(CODE, NAME, VaccineAdultRisk, VaccinePregnant,
            VaccinePregnant, VaccinePregnantTime)

DataMap <- st_read("./Data/world.zh.json",
                   quiet = TRUE) |> 
     filter(iso_a3  != "ATA")

# find the country not in Map data
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataMapPlot <- DataMap |> 
     left_join(DataAll, by = c('iso_a3' = 'CODE'))

## panel a -----------------------------------------------------------------

table(DataAll$VaccineAdultRisk)

fill_color <- c("#DD5129FF", "#FAB255FF", "#30B4CCFF", "#43B284FF") |> rev()

fig_1_m <- plot_map_col(DataAll$VaccineAdultRisk, fill_color) +
     scale_fill_manual(values = fill_color) +
     labs(title = 'Vaccine ofr\nadults & risk groups')

fig_1 <- ggplot(data = DataMapPlot)+
     geom_sf(aes(fill = VaccineAdultRisk)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[1:5],
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccineAdultRisk),
                       limits = levels(DataMapPlot$VaccineAdultRisk))+
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

table(DataAll$VaccinePregnant)

fill_color <- c("#FAB255FF", "#43B284FF")

fig_2_m <- plot_map_col(DataAll$VaccinePregnant, fill_color) +
     scale_fill_manual(values = fill_color[1:5]) +
     labs(title = 'Vaccine for\npregnant woman')

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = VaccinePregnant)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color[1:5],
                       na.value = "white",
                       breaks = levels(DataMapPlot$VaccinePregnant),
                       limits = levels(DataMapPlot$VaccinePregnant))+
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

DataTime <- DataAll |> 
     select(CODE, VaccinePregnantTime) |> 
     filter(!is.na(VaccinePregnantTime)) |> 
     # split into two columns
     separate(VaccinePregnantTime, into = c('VaccinePregnantTime1', 'VaccinePregnantTime2'), sep = '-') |> 
     # remove w in second column
     mutate(VaccinePregnantTime2 = gsub('w', '', VaccinePregnantTime2),
            VaccinePregnantTime2 = as.numeric(VaccinePregnantTime2),
            VaccinePregnantTime1 = as.numeric(VaccinePregnantTime1)) |> 
     arrange(VaccinePregnantTime1, VaccinePregnantTime2)

# periods of pregnancy
DataPeriods <- data.frame(Periods = c('First trimester', 'Second trimester', 'Third trimester'),
                          Start = c(0, 13, 29),
                          End = c(12.9, 28.9, 40.9)) |> 
     mutate(Periods = factor(Periods, 
                             levels = c('First trimester', 'Second trimester', 'Third trimester')))

# Visualization
fill_color <- paletteer_d("wesanderson::AsteroidCity3")

fig_3 <- ggplot(DataTime) +
     # background color
     geom_rect(data = DataPeriods,
               aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Periods),
               alpha = 0.5) +
     geom_linerange(aes(y = CODE, 
                        xmin = VaccinePregnantTime1, 
                        xmax = VaccinePregnantTime2), 
                    linewidth = 2, color = "#5785C1FF")+
     geom_point(aes(x = VaccinePregnantTime1, y = CODE), 
                size = 4, color = "#5785C1FF") +
     geom_point(aes(x = VaccinePregnantTime2, y = CODE),
                size = 4, color = "#5785C1FF") +
     scale_y_discrete(limits = rev(DataTime$CODE))+
     scale_x_continuous(breaks = seq(0, 41, 2),
                        labels = seq(0, 41, 2),
                        expand = c(0, 0),
                        limits = c(0, 41))+
     scale_fill_manual(values = fill_color,
                       name = NULL,
                       labels = levels(DataPeriods$Periods)) +
     theme_bw()+
     theme(legend.position = 'right',
           axis.text = element_text(color = 'black', face = 'plain'),
           plot.title.position = 'plot')+
     labs(title = 'C',
          x = 'Weeks of pregnancy',
          y = NULL)

# combine all figures -----------------------------------------------------

fig <- cowplot::plot_grid(fig_1 + fig_2,
                          fig_3,
                          rel_heights = c(1, 1.5),
                          ncol = 1)

ggsave("./Outcome/fig3.pdf",
       fig,
       width = 15,
       height = 10,
       device = cairo_pdf)

ggsave("./Outcome/fig3.png",
       fig,
       width = 15,
       height = 10)

write.csv(DataAll,
          "./Outcome/fig data/fig3.csv",
          row.names = F)

# content -----------------------------------------------------------------

print('################## Figure 3 ##################')

print(paste(length(unique(DataAll$CODE)), 'countries have data on adult vaccination.'))
