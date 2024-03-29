
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)

library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)

fill_color <- pal_npg()(3)

# Load data
df_raw <- read.xlsx('./Fig Data/fig1.xlsx', detectDates = T) |> 
     arrange(Date)
df_clean <- df_raw |> 
     group_by(Country, stage, Month, Week) |> 
     summarise(median = median(Cases),
               Q1 = quantile(Cases, 0.25),
               Q3 = quantile(Cases, 0.75),
               .groups = 'drop')

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

plot_max <- c(4e3, 2e4, 5e2, 5e2)

# au ----------------------------------------------------------------------

data <- df_raw |> 
     filter(Country == 'AU')
data_2023 <- df_clean |> 
     filter(Country == 'AU') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month))) |> 
     filter(stage == '2023 Jun onwards')
data_2022 <- df_clean |> 
     filter(Country == 'AU') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month))) |>
     filter(stage != '2023 Jun onwards')

plot_breaks <- pretty(c(0, plot_max[1]))
plot_range <- range(plot_breaks)

data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)
data$Stage <- factor(data$stage,
                     levels = unique(data$stage))
results <- lmer(Cases ~ Stage + (1|Month), data = filter(data, Stage != '2023 Jun onwards'))
emm <- emmeans(results, ~ Stage)
pairs <- pairs(emm)
pairs_summary <- summary(pairs, adjust = "bonferroni") |> 
     as.data.frame() |>
     separate(contrast, c('stage1', 'stage2'), sep = ' \\- ') |> 
     rename('group1' = 'stage1',
            'group2' = 'stage2') |> 
     mutate(y.position = plot_range[2] * 0.9,
            p.value = ifelse(p.value < 0.001, '***', format(round(p.value, 3), nsmall = 3)))

fig1_1 <- ggplot(data_2022, aes(x = month, y = median)) +
     geom_smooth(aes(color = stage, fill = stage),
                 method = 'gam',
                 se = T,
                 linewidth = 0.7) +
     geom_line(data = data_2023, aes(color = stage),
               linewidth = 0.7) +
     geom_ribbon(data = data_2023, aes(ymin = median, ymax = median, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = 101:112,
                        labels = month.abb[c(7:12, 1:6)],
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'a', x = 'Month', y = 'Monthly incidence', 
          color = 'Stage', fill = 'Stage') +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.background = element_rect(fill = "transparent"),
           legend.text = element_text(face = "bold", size = 12),
           legend.title = element_text(face = "bold", size = 12),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))

fig1_2 <- ggplot(data, aes(x = stage, y = Cases)) +
     geom_boxplot(aes(color = stage),
                  show.legend = F)+
     stat_pvalue_manual(pairs_summary,
                        hide.ns = F) +
     coord_cartesian(clip = "off")+
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'b', x = 'Stage', y = NULL) +
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

remove(data, data_2022, data_2023,
       plot_breaks, plot_range,
       results, emm, pairs, pairs_summary)

# cn ----------------------------------------------------------------------

data <- df_raw |> 
     filter(Country == 'CN')
data_2023 <- df_clean |> 
     filter(Country == 'CN') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month))) |> 
     filter(stage == '2023 Jun onwards')
data_2022 <- df_clean |> 
     filter(Country == 'CN') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month))) |>
     filter(stage != '2023 Jun onwards')

plot_breaks <- pretty(c(0, plot_max[2]))
plot_range <- range(plot_breaks)

data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)
data$Stage <- factor(data$stage,
                     levels = unique(data$stage))
results <- lmer(Cases ~ Stage + (1|Month), data = filter(data, Stage != '2023 Jun onwards'))
emm <- emmeans(results, ~ Stage)
pairs <- pairs(emm)
pairs_summary <- summary(pairs, adjust = "bonferroni") |> 
     as.data.frame() |>
     separate(contrast, c('stage1', 'stage2'), sep = ' \\- ') |> 
     rename('group1' = 'stage1',
            'group2' = 'stage2') |> 
     mutate(y.position = plot_range[2] * 0.9,
            p.value = ifelse(p.value < 0.001, '***', format(round(p.value, 3), nsmall = 3)))

print(pairs_summary)

fig2_1 <- ggplot(data_2022, aes(x = month, y = median)) +
     geom_smooth(aes(color = stage, fill = stage),
                 method = 'gam',
                 se = T,
                 linewidth = 0.7) +
     geom_line(data = data_2023, aes(color = stage),
               linewidth = 0.7) +
     geom_ribbon(data = data_2023, aes(ymin = median, ymax = median, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = 101:112,
                        labels = month.abb[c(7:12, 1:6)],
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'c', x = 'Month', y = 'Monthly incidence', 
          color = 'Stage', fill = 'Stage') +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.background = element_rect(fill = "transparent"),
           legend.text = element_text(face = "bold", size = 12),
           legend.title = element_text(face = "bold", size = 12),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))
fig2_2 <- ggplot(data, aes(x = stage, y = Cases)) +
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

remove(data, data_2022, data_2023,
       plot_breaks, plot_range,
       results, emm, pairs, pairs_summary)

# uk ----------------------------------------------------------------------

data <- df_raw |> 
     filter(Country == 'UK')
data_2023 <- df_clean |>
     filter(Country == 'UK') |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |> 
     filter(stage == '2023 Jun onwards')
data_2022 <- df_clean |>
     filter(Country == 'UK') |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |>
     filter(stage != '2023 Jun onwards')

plot_breaks <- pretty(c(0, plot_max[3]))
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
     labs(title = 'e', x = 'Epidemic week', y = 'Weekly incidence',
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
     labs(title = 'f',  x = 'Stage', y = NULL) +
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

remove(data, data_2022, data_2023,
       plot_breaks, plot_range,
       results, emm, pairs, pairs_summary)

# us ----------------------------------------------------------------------

data <- df_raw |> 
     filter(Country == 'US')
data_2023 <- df_clean |> 
     filter(Country == 'US') |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |> 
     filter(stage == '2023 Jun onwards')
data_2022 <- df_clean |>
     filter(Country == 'US') |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week))) |>
     filter(stage != '2023 Jun onwards')

plot_breaks <- pretty(c(0, plot_max[4]))
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

fig4_1 <- ggplot(data_2022, aes(x = week, y = median)) +
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
     labs(title = 'g', x = 'Epidemic week', y = 'Weekly incidence',
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

fig4_2 <- ggplot(data, aes(x = stage, y = Cases)) +
     geom_boxplot(aes(color = stage),
                  show.legend = F)+
     stat_pvalue_manual(pairs_summary,
                        hide.ns = F) +
     coord_cartesian(clip = "off")+
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = plot_range) +
     labs(title = 'h', x = 'Stage', y = NULL) +
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

remove(data, data_2022, data_2023,
       plot_breaks, plot_range,
       results, emm, pairs, pairs_summary)

plot_panel <- "
AAABCCCD
EEEFGGGH
"

plot_all <- fig1_1 + fig1_2 + fig2_1 + fig2_2 + 
     fig3_1 + fig3_2 + fig4_1 + fig4_2 +
     plot_layout(design = plot_panel, guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './main/Fig3.pdf',
       plot = plot_all,
       width = 12,
       height = 6, 
       device = cairo_pdf,
       family = 'Times New Roman')

write.xlsx(df_clean,
           './Fig Data/fig3.xlsx')
