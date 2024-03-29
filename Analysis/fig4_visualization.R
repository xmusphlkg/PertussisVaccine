
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)
library(ggh4x)

Sys.setlocale(locale = "en")

fill_color <- pal_npg()(7)
area_color <- fill_color[4:5]
line_color_1 <- fill_color[1:3]
line_color_2 <- fill_color[6:7]
names(area_color) <- c('Decreased', 'Increased')

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# Load data
df_predict <- read.xlsx('./Fig Data/fig4.xlsx', sheet = 'Sheet 1', detectDates = T)
df_fit <- read.xlsx('./Fig Data/fig4.xlsx', sheet = 'Sheet 2', detectDates = T)

# plot --------------------------------------------------------------------

i <- 1
country_list <- sort(unique(df_predict$country))
ylabel_list <- c('Monthly incidence', 'Monthly incidence', 'Weekly incidence', 'Weekly incidence')
df_predict$mean[df_predict$mean < 0] <- 0

plot_max <- c(4e3, 2e4, 5e2, 5e2)

plot_data <- function(i){
     data_predict <- df_predict |> 
          filter(country == country_list[i]) |> 
          arrange(date)
     data_fit <- df_fit |>
          filter(country == country_list[i]) |>
          arrange(date)
     
     max_cases <- plot_max[i]
     plot_breaks <- pretty(c(max_cases, 0))
     
     fig1 <- ggplot(data = data_fit, aes(x = date)) +
          geom_line(aes(y = fit, color = 'Fitted')) +
          geom_line(aes(y = simu, color = 'Observed')) +
          geom_line(data = data_predict, aes(y = mean, color = 'Predicted')) +
          labs(title = letters[i*2-1], x = 'Date', y = ylabel_list[i]) +
          coord_cartesian(ylim = range(plot_breaks)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(min(data_fit$date), max(data_predict$date), by = "1 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = plot_breaks) +
          scale_color_manual(values = line_color_1) +
          theme_classic()+
          theme(legend.title = element_blank(),
                legend.background = element_rect(fill = "transparent"),
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text = element_text(size = 12, color = "black"),
                plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.key = element_blank())+
          labs(x = 'Date', y = ylabel_list[i], title = letters[i*2-1])
     
     fig2 <- ggplot(data = data_predict, aes(x = date)) +
          geom_line(aes(y = mean, color = 'Predicted')) +
          geom_line(aes(y = observed, color = 'Observed'))+
          stat_difference(aes(ymin = observed, ymax = mean),
                          alpha = 0.3,
                          levels = c("Decreased", "Increased"),
                          show.legend = i == 4)+
          coord_cartesian(xlim = as.Date(c('2023-7-1', '2024-4-30'))) +
          scale_color_manual(values = line_color_2) +
          scale_fill_manual(values = area_color) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y-%b",
                       breaks = seq(as.Date('2023-1-1'), as.Date('2024-4-1'), by = "3 month")) +
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          theme_classic()+
          theme(legend.title = element_blank(),
                legend.background = element_rect(fill = "transparent"),
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text = element_text(size = 12, color = "black"),
                plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.key = element_blank())+
          labs(x = 'Date', y = NULL, title = letters[i*2])
     
     fig1 + fig2 + plot_layout(widths = c(2, 1))
}

plot_all <- lapply(1:4, plot_data)
plot_all <- wrap_plots(plotlist = plot_all, ncol = 1)+
     plot_layout(guides = "collect")&
     theme(legend.position = 'bottom')

ggsave(filename = './main/Fig4.pdf',
       plot = plot_all,
       width = 12,
       height = 10, 
       device = cairo_pdf,
       family = 'Times New Roman')



# summary -----------------------------------------------------------------

df_predict |>
     mutate(diff = mean - observed) |>
     group_by(country) |>
     summarise(observed = sum(observed, na.rm = T),
               mean = sum(mean, na.rm = T),
               total = sum(diff, na.rm = T),
               total_per = paste(100 * round(total / sum(mean, na.rm = T), 4), "%"))
