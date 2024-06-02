
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

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table1.csv")|> 
     mutate(
          OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable')),
          OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'))
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

# panel a -----------------------------------------------------------------

fill_color <- rev(c("grey50", '#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF'))

DataExample <- DataInci |> 
     filter(NAME %in% c('China', 'Canada')) |> 
     as.data.frame() |> 
     mutate(YEAR = as.numeric(YEAR))

DataExampleThreshold <- DataExample|> 
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
     mutate(line1 = 0,
            line2 = `Pre-epidemic_Incidence_25`,
            line3 = `Pre-epidemic_Incidence_75`,
            line4 = `Pre-epidemic_Incidence_75` + 1.5*`Pre-epidemic_IQR`,
            line5 = Inf) |> 
     select(NAME, line1:line5)

fig_0_1 <- ggplot(data = DataExample,
                  mapping = aes(x = YEAR, y = Incidence, linetype = NAME)) +
     geom_line(color = "#862633FF") +
     geom_point(color = "#862633FF")+
     geom_vline(xintercept = 2019.5) +
     scale_x_continuous(limits = c(2009, 2024),
                        expand = c(0, 0),
                        breaks = seq(2010, 2023, 3)) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0.99, 0.99),
           legend.justification = c(1, 1),
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(x = NULL, y = "Yearly incidence", linetype = '', title = 'A')

fig_0_2 <- DataExample |> 
     filter(Period == 'Pre-epidemic') |>
     ggplot() +
     geom_rect(data = DataExampleThreshold,
               aes(xmin = -Inf, xmax = Inf, ymin = line1, ymax = line2, fill = 'Low'),
               show.legend = F,
               alpha = 1) +
     geom_rect(data = DataExampleThreshold,
               aes(xmin = -Inf, xmax = Inf, ymin = line2, ymax = line3, fill = 'Normal'),
               show.legend = F,
               alpha = 1) +
     geom_rect(data = DataExampleThreshold,
               aes(xmin = -Inf, xmax = Inf, ymin = line3, ymax = line4, fill = 'High'),
               show.legend = F,
               alpha = 1) +
     geom_rect(data = DataExampleThreshold,
               aes(xmin = -Inf, xmax = Inf, ymin = line4, ymax = line5, fill = 'Resurgence'),
               show.legend = F,
               alpha = 1) +
     geom_jitter(aes(x = NAME, y = Incidence),
                  fill = NA,
                 vjust = 0,
                  color = 'black',
                  show.legend = F) +
     geom_point(data = filter(DataExample, YEAR %in% 2022:2023),
                mapping = aes(x = NAME, y = Incidence),
                shape = 3,
                color = 'white') +
     geom_text(data = filter(DataExample, YEAR %in% 2022:2023),
               mapping = aes(x = NAME, y = Incidence, label = YEAR),
               vjust = -0.5,
               hjust = -0.1,
               color = 'white',
               fontface = 'bold',
               size = 2.5) +
     scale_fill_manual(values = rev(fill_color)[-1],
                       breaks = rev(c('Low', 'Normal', 'High', 'Resurgence')),
                       na.translate = F) +
     scale_y_continuous(limits = c(0, NA),
                        expand = expansion(mult = c(0, 0.3))) +
     facet_wrap(~NAME, scales = 'free') +
     theme(axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           axis.line.y = element_line(color = 'black'),
           legend.position = 'left',
           legend.title.align = 0,
           legend.direction = 'vertical',
           plot.title.position = 'plot',
           strip.text = element_blank(),
           legend.text = element_text(angle = 90),
           legend.title = element_text(angle = 90)) +
     labs(x = NULL, y = "Yearly incidence", fill = '', title = 'B') +
     guides(fill = guide_legend(title.position = 'left',
                                label.position = 'bottom',
                                label.vjust = 0.5,
                                nrow = 4))

fig_0 <- fig_0_1 + fig_0_2

## panel b&c -----------------------------------------------------------------

fill_color <- rev(c("grey50", '#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF'))

fig_1_m <- plot_map_col(DataAll$OutbreakSize2022, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       limits = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       na.translate = F)

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
                       breaks = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       limits = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "C", x = NULL, y = NULL, fill = 'Pertussis status in 2022')+
     guides(fill = guide_legend(nrow = 1))

fig_1 <- fig_1 + inset_element(fig_1_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

fig_2_m <- plot_map_col(DataAll$OutbreakSize2023, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       limits = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       na.translate = F)

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = factor(OutbreakSize2023))) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       limits = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable'),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.box = 'horizontal',
           plot.title.position = 'plot') +
     labs(title = "D", x = NULL, y = NULL, fill = 'Pertussis status')+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

# combine -----------------------------------------------------------------

design <- "
AAAB
AAAB
CCCC
CCCC
CCCC
DDDD
DDDD
DDDD
"

fig <- fig_0_1 + fig_0_2 + fig_1 + fig_2 + plot_layout(design = design)


ggsave("./Outcome/fig3.pdf",
       fig,
       width = 7,
       height = 9,
       device = cairo_pdf,
       family = "Helvetica")

# fig4 ----------------------------------------------------------------

## panel a -------------------------------------------------------------

DataRe <- DataInci |> 
     filter(NAME %in% DataAll$NAME[DataAll$OutbreakSize == 3]) |> 
     group_by(NAME, Period) |>
     summarise(Incidence_50 = median(Incidence, na.rm = T),
               Incidence_25 = quantile(Incidence, 0.25, na.rm = T),
               Incidence_75 = quantile(Incidence, 0.75, na.rm = T),
               .groups = 'drop')
DataRePre <- DataRe |> 
     filter(Period == 'Pre-epidemic') |> 
     select(-Period) |> 
     arrange(desc(Incidence_50))
DataReOut <- DataRe |> 
     filter(Period %in% c(2022, 2023)) |> 
     select(NAME, Incidence_50, Period) |> 
     pivot_wider(names_from = Period,
                 values_from = Incidence_50) |> 
     mutate(`2022` = case_when(NAME %in% DataAll$NAME[DataAll$OutbreakSize2022 == 'Resurgence'] ~ `2022`,
                               TRUE ~ NA_real_),
            `2023` = case_when(NAME %in% DataAll$NAME[DataAll$OutbreakSize2023 == 'Resurgence'] ~ `2023`,
                               TRUE ~ NA_real_)) |> 
     pivot_longer(cols = c(`2022`, `2023`),
                  names_to = 'Period',
                  values_drop_na = T,
                  values_to = 'Incidence_50')

fig_1 <- ggplot(data = DataRePre,
       mapping = aes(x = NAME, y = Incidence_50)) +
     geom_pointrange(mapping = aes(color = '2010-2019', ymin = Incidence_25, ymax = Incidence_75)) +
     geom_point(data = DataReOut,
                mapping = aes(x = NAME, y = Incidence_50, color = as.factor(Period)),
                shape = 3) +
     geom_text(data = DataReOut,
               mapping = aes(x = NAME, y = Incidence_50, label = Period),
               hjust = -0.5,
               vjust = 0.5,
               color = 'black',
               size = 2.5) +
     coord_flip() +
     scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)),
                        limits = c(0.1, NA),
                        breaks = c(0.1, 1, 10, 100, 1000),
                        labels = c(0.1, 1, 10, 100, 1000),
                        trans = 'log10') +
     scale_x_discrete(limits = DataRePre$NAME) +
     scale_color_manual(values = fill_color[-2],
                        breaks = c('2010-2019','2022', '2023'),
                        na.translate = F) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'right',
           plot.title.position = 'plot') +
     labs(x = NULL, y = "Yearly incidence", color = 'Period', title = 'A')
     

## panel B -------------------------------------------------------------

DataOut <- DataInci |> 
     filter(NAME %in% DataAll$NAME[DataAll$OutbreakSize == 2]) |> 
     group_by(NAME, Period) |>
     summarise(Incidence_50 = median(Incidence, na.rm = T),
               Incidence_25 = quantile(Incidence, 0.25, na.rm = T),
               Incidence_75 = quantile(Incidence, 0.75, na.rm = T),
               .groups = 'drop')
DataOutPre <- DataOut |> 
     filter(Period == 'Pre-epidemic') |> 
     select(-Period) |> 
     arrange(desc(Incidence_50))
DataOutOut <- DataOut |> 
     filter(Period %in% c(2022, 2023)) |> 
     select(NAME, Incidence_50, Period) |> 
     pivot_wider(names_from = Period,
                 values_from = Incidence_50) |> 
     mutate(`2022` = case_when(NAME %in% DataAll$NAME[DataAll$OutbreakSize2022 == 'High'] ~ `2022`,
                               TRUE ~ NA_real_),
            `2023` = case_when(NAME %in% DataAll$NAME[DataAll$OutbreakSize2023 == 'High'] ~ `2023`,
                               TRUE ~ NA_real_)) |> 
     pivot_longer(cols = c(`2022`, `2023`),
                  names_to = 'Period',
                  values_drop_na = T,
                  values_to = 'Incidence_50')

fig_2 <- ggplot(data = DataOutPre,
                mapping = aes(x = NAME, y = Incidence_50)) +
     geom_pointrange(mapping = aes(color = '2010-2019', ymin = Incidence_25, ymax = Incidence_75)) +
     geom_point(data = DataOutOut,
                mapping = aes(x = NAME, y = Incidence_50, color = as.factor(Period)),
                shape = 3) +
     geom_text(data = DataReOut,
               mapping = aes(x = NAME, y = Incidence_50, label = Period),
               hjust = -0.5,
               vjust = 0.5,
               color = 'black',
               size = 2.5) +
     coord_flip() +
     scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)),
                        limits = c(0.1, NA),
                        breaks = c(0.1, 1, 10, 100, 1000),
                        labels = c(0.1, 1, 10, 100, 1000),
                        trans = 'log10') +
     scale_x_discrete(limits = DataRePre$NAME) +
     scale_color_manual(values = fill_color[-2],
                        breaks = c('2010-2019','2022', '2023'),
                        limits = c('2010-2019','2022', '2023'),
                        na.translate = F) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'right',
           plot.title.position = 'plot') +
     labs(x = NULL, y = "Yearly incidence", color = 'Period', title = 'B')


fig <- fig_1 + fig_2 +
     plot_layout(guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 10,
       height = 8,
       device = cairo_pdf)

# table S4 ----------------------------------------------------------------

DataTable <- rbind(DataRePre, DataOutPre) |> 
     unique()
DataTable <- rbind(DataReOut, DataOutOut) |> 
     unique() |> 
     pivot_wider(names_from = Period,
                 values_from = Incidence_50) |> 
     right_join(DataTable, by = 'NAME') |> 
     rename(`2010-2019-Q2` = Incidence_50,
            `2010-2019-Q1` = Incidence_25,
            `2010-2019-Q3` = Incidence_75) |> 
     mutate(`2023-change` = `2023`/`2010-2019-Q2`,
            `2022-change` = `2022`/`2010-2019-Q2`)

write.csv(DataTable, "./Outcome/S table4.csv", row.names = F)     
     
     