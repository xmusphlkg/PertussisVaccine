library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)
library(purrr)

fill_color <- pal_npg()(3)

# Load data
df_raw <- read.csv('./report_pertussis.csv')

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# clean data --------------------------------------------------------------

df_epiweek <- data.frame(date = seq.Date(from = as.Date('2015-01-04'), to = as.Date('2024-03-09'), by = 'day')) |> 
     mutate(Year = epiyear(date),
            Week = epiweek(date)) |> 
     group_by(Year, Week) |>
     summarise(Date = last(date),
               .groups = 'drop')
df_jp <- df_raw |> 
     filter(Country == 'JP') |> 
     select(Year, Week, Cases)
df_jp <- left_join(df_epiweek, df_jp, by = c('Year', 'Week')) |> 
     arrange(Date) |> 
     mutate(Cases = as.integer(Cases),
            Country = 'JP') |> 
     arrange(Date) |> 
     select(Country, Date, Year, Week, Cases) |> 
     mutate(stage = case_when(
          Date < as.Date('2020-01-01') ~ '2015 Jan to 2019 Dec',
          Date >= as.Date('2020-01-01') & Date < as.Date('2023-07-01') ~ '2020 Jan to 2023 Jun',
          Date >= as.Date('2023-07-01') ~ '2023 Jun onwards'
     ))

# epidemic curve ----------------------------------------------------------

split_dates <- as.Date(c("2015/1/1", "2020/1/1", "2023/7/1", "2024/3/9"))
split_periods <- c("2015 Jan to 2019 Dec", "2020 Jan to 2023 Jun", "2023 Jun onwards")
datafile_rect <- data.frame(Period = split_periods,
                            start = split_dates[1:3],
                            end = split_dates[2:4])

fig_1 <- ggplot(df_jp)+
     geom_rect(data = datafile_rect,
               aes(xmin = start, xmax = end, fill = Period),
               ymin = -Inf,
               ymax = Inf,
               alpha = 0.2,
               show.legend = T) +
     geom_line(aes(x = Date, y = Cases), color = '#B09C85') +
     scale_x_date(date_labels = '%Y',
                  date_breaks = 'year',
                  limits = c(as.Date('2015-01-01'), as.Date('2024-03-09')),
                  expand = expansion(add = c(7, 7))) +
     scale_y_continuous(labels = scientific_10,
                        limits = c(0, NA),
                        expand = expansion(mult = c(0, 0))) +
     scale_fill_manual(values = fill_color) +
     theme_bw() +
     theme(
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 14, hjust = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_text(face = "bold", size = 12),
          legend.title = element_text(face = "bold", size = 12),
          legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
          legend.position = "bottom",
          legend.background = element_rect(fill = "transparent", colour = "transparent"),
          axis.title = element_text(face = "bold", size = 12, color = "black"),
          axis.text = element_text(size = 12, color = "black")
     ) +
     labs(title = 'a',
          y = "Weekly incidence",
          x = 'Date',
          fill = 'Stage')

data <- df_jp |> 
     group_by(Year) |> 
     filter(Year < 2024) |>
     summarise(Cases = sum(Cases))
plot_breaks <- pretty(c(0, max(data$Cases)))
plot_range <- range(plot_breaks) 

fig_2 <- ggplot(data)+
     geom_col(aes(x = Year, y = Cases), fill = '#B09C85') +
     scale_x_continuous(breaks = seq(2015, 2023, 2)) +
     scale_y_continuous(labels = scientific_10,
                        limits = plot_range,
                        breaks = plot_breaks,
                        expand = expansion(mult = c(0, 0))) +
     theme_bw() +
     theme(
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 14, hjust = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_text(face = "bold", size = 12),
          legend.title = element_text(face = "bold", size = 12),
          legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
          legend.background = element_rect(fill = "transparent", colour = "transparent"),
          axis.title = element_text(face = "bold", size = 12, color = "black"),
          axis.text = element_text(size = 12, color = "black")
     ) +
     labs(title = 'b',
          x = 'Year',
          y = 'Yearly incidence',
          fill = 'Stage')

fig1 <- fig_1 + fig_2 + plot_layout(widths = c(3, 1), guides = 'collect')&
     theme(legend.position = 'bottom')

# fixed effect model ------------------------------------------------------

library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)

data <- df_jp
df_clean <- df_jp |> 
     group_by(stage, Week) |> 
     summarise(median = median(Cases),
               Q1 = quantile(Cases, 0.25),
               Q3 = quantile(Cases, 0.75),
               .groups = 'drop')
data_2023 <- df_clean |>
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |> 
     filter(stage == '2023 Jun onwards')
data_2022 <- df_clean |>
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |>
     filter(stage != '2023 Jun onwards')

plot_breaks <- pretty(c(0, max(data$Cases)))
plot_range <- range(plot_breaks)

data$Year <- as.factor(data$Year)
data$Week <- as.factor(data$Week)
data$Stage <- factor(data$stage,
                     levels = unique(data$stage))
results <- lmer(Cases ~ Stage + (1|Week), data = filter(data, Stage != '2023 Jun onwards'))
emm <- emmeans(results, ~ Stage)
pairs <- pairs(emm)
pairs_summary <- summary(pairs, adjust = "bonferroni") |> 
     as.data.frame() |>
     separate(contrast, c('stage1', 'stage2'), sep = ' \\- ') |> 
     rename('group1' = 'stage1',
            'group2' = 'stage2') |> 
     mutate(y.position = plot_range[2] * 0.9,
            p.value = ifelse(p.value < 0.001, '***', format(round(p.value, 3), nsmall = 3)))

fig3_1 <- ggplot(data_2022, aes(x = week, y = median)) +
     geom_smooth(aes(color = stage, fill = stage),
                 method = 'gam',
                 se = T,
                 linewidth = 0.7) +
     geom_line(data = data_2023, aes(color = stage),
               linewidth = 0.7) +
     geom_ribbon(data = data_2023, aes(ymin = median, ymax = median, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = seq(201, 252, 4),
                        labels = c(26, 30, 34, 38, 42, 46, 50, 2, 6, 10, 14, 18, 22),
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'c', x = 'Epidemic week', y = 'Weekly incidence',
          fill = 'Stage', color = 'Stage') +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_text(face = "bold", size = 12),
           legend.background = element_rect(fill = "transparent"),
           legend.text = element_text(face = "bold", size = 12),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))

fig3_2 <- ggplot(data, aes(x = stage, y = Cases)) +
     geom_boxplot(aes(color = stage),
                  show.legend = F)+
     stat_pvalue_manual(pairs_summary,
                        hide.ns = F) +
     coord_cartesian(clip = "off")+
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'd',  x = 'Stage', y = NULL) +
     scale_color_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_blank(),
           legend.background = element_rect(fill = "transparent"),
           panel.background = element_rect(fill = "transparent", color = "transparent"),
           plot.background = element_rect(fill = "transparent", color = "transparent"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_blank())

fig3 <- fig3_1 + fig3_2 + plot_layout(widths = c(3, 1), guides = 'collect')&
     theme(legend.position = 'bottom')

remove(data, data_2022, data_2023,
       plot_breaks, plot_range,
       results, emm, pairs, pairs_summary)

ggsave('./appendix/S4.png', 
       cowplot::plot_grid(fig1, fig3, nrow = 2),
       width = 12, 
       height = 8, 
       dpi = 300)