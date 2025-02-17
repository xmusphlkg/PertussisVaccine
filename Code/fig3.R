
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

DataInci <- read.xlsx("./Data/Pertussis incidence.xlsx")[,1:17]|> 
     filter(!is.na(Disease)) |> 
     select(-c(Denominator, Disease)) |> 
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

# appendix ----------------------------------------------------------------

# visualize the pertussis incidence and income.group
fig <- ggboxplot(DataAll,
          x = "Income.group",
          y = "IncidencePre", 
          color = "Income.group",
          add = "jitter", 
          ylab = "Average incidence in pre-epidemic period",
          xlab = "Income group",
          ggtheme = theme_bw()) +
     stat_compare_means(aes(group = Income.group),
                        method = "anova") +
     theme(plot.title = element_text(hjust = 0.5),
           legend.position = "none")
ggsave("./Outcome/S fig3_1.png",
       fig,
       width = 6,
       dpi = 300,
       height = 4)

# panel a -----------------------------------------------------------------

# fill_color <- rev(c("grey50", '#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF'))

# DataExample <- DataInci |> 
#      filter(NAME %in% c('China', 'Canada')) |> 
#      as.data.frame() |> 
#      mutate(YEAR = as.numeric(YEAR))
# 
# DataExampleThreshold <- DataAll |> 
#      filter(NAME %in% c('China', 'Canada')) |>
#      mutate(line1 = 0,
#             line2 = IncidencePre25,
#             line3 = IncidencePre75,
#             line4 = IncidencePre75 + 1.5*IncidencePreIQR,
#             line5 = Inf) |> 
#      select(NAME, line1:line5)
# 
# fig_0_1 <- ggplot(data = DataExample,
#                   mapping = aes(x = YEAR, y = Incidence, shape = NAME)) +
#      geom_line(color = "#862633FF") +
#      geom_point(color = "#862633FF")+
#      geom_vline(xintercept = 2019.5) +
#      scale_x_continuous(limits = c(2009, 2024),
#                         expand = c(0, 0),
#                         breaks = seq(2010, 2023, 3)) +
#      theme_bw() +
#      theme(panel.grid = element_blank(),
#            axis.text = element_text(color = 'black', face = 'plain'),
#            axis.title = element_text(color = 'black', face = 'plain'),
#            legend.position = c(0.99, 0.99),
#            legend.justification = c(1, 1),
#            legend.box = 'horizontal',
#            plot.title.position = 'plot') +
#      labs(x = NULL, y = "Yearly incidence", shape = NULL, title = 'A')
# 
# fig_0_2 <- DataExample |> 
#      filter(Period == 'Pre-epidemic') |>
#      ggplot() +
#      geom_rect(data = DataExampleThreshold,
#                aes(xmin = -Inf, xmax = Inf, ymin = line1, ymax = line2, fill = 'Low'),
#                show.legend = F,
#                alpha = 1) +
#      geom_rect(data = DataExampleThreshold,
#                aes(xmin = -Inf, xmax = Inf, ymin = line2, ymax = line3, fill = 'Normal'),
#                show.legend = F,
#                alpha = 1) +
#      geom_rect(data = DataExampleThreshold,
#                aes(xmin = -Inf, xmax = Inf, ymin = line3, ymax = line4, fill = 'High'),
#                show.legend = F,
#                alpha = 1) +
#      geom_rect(data = DataExampleThreshold,
#                aes(xmin = -Inf, xmax = Inf, ymin = line4, ymax = line5, fill = 'Resurgence'),
#                show.legend = F,
#                alpha = 1) +
#      geom_jitter(aes(x = NAME, y = Incidence),
#                  fill = NA,
#                  height = 0,
#                  color = 'black',
#                  show.legend = F) +
#      geom_point(data = filter(DataExample, YEAR %in% 2022:2023),
#                 mapping = aes(x = NAME, y = Incidence),
#                 shape = 3,
#                 color = 'white') +
#      geom_text(data = filter(DataExample, YEAR %in% 2022:2023),
#                mapping = aes(x = NAME, y = Incidence, label = YEAR),
#                vjust = -0.5,
#                hjust = -0.1,
#                color = 'white',
#                fontface = 'bold',
#                size = 2.5) +
#      scale_fill_manual(values = rev(fill_color)[-1],
#                        breaks = rev(c('Low', 'Normal', 'High', 'Resurgence')),
#                        na.translate = F) +
#      scale_y_continuous(limits = c(0, NA),
#                         expand = expansion(mult = c(0, 0.3))) +
#      facet_wrap(~NAME, scales = 'free') +
#      theme(axis.text = element_text(color = 'black', face = 'plain'),
#            axis.title = element_text(color = 'black', face = 'plain'),
#            axis.line.y = element_line(color = 'black'),
#            legend.position = 'left',
#            legend.title.align = 0,
#            legend.direction = 'vertical',
#            plot.title.position = 'plot',
#            strip.text = element_blank(),
#            legend.text = element_text(angle = 90),
#            legend.title = element_text(angle = 90)) +
#      labs(x = NULL, y = "Yearly incidence", fill = '', title = 'B') +
#      guides(fill = guide_legend(title.position = 'left',
#                                 label.position = 'bottom',
#                                 label.vjust = 0.5,
#                                 nrow = 4))
# 
# fig_0 <- fig_0_1 + fig_0_2

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
