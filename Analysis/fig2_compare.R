
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)

fill_color <- pal_npg()(3)

# Load data
df_clean <- read.xlsx('./Fig Data/fig1.xlsx', detectDates = T) |> 
     arrange(Date) |> 
     group_by(Country, stage, Month, Week) |> 
     summarise(median = median(Cases),
               Q1 = quantile(Cases, 0.25),
               Q3 = quantile(Cases, 0.75),
               .groups = 'drop')

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# au ----------------------------------------------------------------------

data <- df_clean |> 
     filter(Country == 'AU') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month)))
data$Q1[data$stage == '2023 Jun. onwards'] <- NA
data$Q3[data$stage == '2023 Jun. onwards'] <- NA

fig1 <- ggplot(data, aes(x = month, y = median)) +
     geom_line(aes(color = stage)) +
     geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = 101:112,
                        labels = month.abb[c(7:12, 1:6)],
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, NA)) +
     labs(title = 'a', x = 'Month', y = 'Monthly incidence') +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_blank(),
           legend.background = element_rect(fill = "transparent"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))

# cn ----------------------------------------------------------------------

data <- df_clean |> 
     filter(Country == 'CN') |> 
     mutate(month = factor(Month,
                           levels = c(7:12, 1:6),
                           labels = 101:112),
            month = as.integer(as.character(month)))
data$Q1[data$stage == '2023 Jun. onwards'] <- NA
data$Q3[data$stage == '2023 Jun. onwards'] <- NA

fig2 <- ggplot(data, aes(x = month, y = median)) +
     geom_line(aes(color = stage)) +
     geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = 101:112,
                        labels = month.abb[c(7:12, 1:6)],
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, NA)) +
     labs(title = 'b', x = 'Month', y = 'Monthly incidence') +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_blank(),
           legend.background = element_rect(fill = "transparent"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))

# us ----------------------------------------------------------------------

data <- df_clean |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week)))
data$Q1[data$stage == '2023 Jun. onwards'] <- NA
data$Q3[data$stage == '2023 Jun. onwards'] <- NA

fig3 <- ggplot(data, aes(x = week, y = median)) +
     geom_line(aes(color = stage)) +
     geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = seq(201, 252, 4),
                        labels = c(26, 30, 34, 38, 42, 46, 50, 2, 6, 10, 14, 18, 22),
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, NA)) +
     labs(title = 'c', x = 'Epidemic week', y = 'Weekly incidence') +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_blank(),
           legend.background = element_rect(fill = "transparent"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))

# uk ----------------------------------------------------------------------

data <- df_clean |> 
     filter(Country == 'UK') |> 
     filter(Week <= 52) |>
     mutate(week = factor(Week,
                          levels = c(26:52, 1:25),
                          labels = 201:252),
            week = as.integer(as.character(week)))
data$Q1[data$stage == '2023 Jun. onwards'] <- NA
data$Q3[data$stage == '2023 Jun. onwards'] <- NA

fig4 <- ggplot(data, aes(x = week, y = median)) +
     geom_line(aes(color = stage)) +
     geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = stage), alpha = 0.3) +
     scale_x_continuous(breaks = seq(201, 252, 4),
                        labels = c(26, 30, 34, 38, 42, 46, 50, 2, 6, 10, 14, 18, 22),
                        expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, NA)) +
     labs(title = 'd', x = 'Epidemic week', y = 'Weekly incidence') +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = fill_color) +
     theme_classic() +
     theme(legend.box.just = "left",
           legend.margin = margin(0, 0, 0, 0),
           legend.title = element_blank(),
           legend.background = element_rect(fill = "transparent"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           axis.text = element_text(size = 12, color = "black"))


plot_all <- fig1 + fig2 + fig3 + fig4 +
     plot_layout(ncol = 2, byrow = T, guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './main/Fig2.pdf',
       plot = plot_all,
       width = 10,
       height = 6, 
       device = cairo_pdf,
       family = 'Times New Roman')

write.xlsx(df_clean,
           './Fig Data/fig2.xlsx')
