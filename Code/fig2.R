
# This script generates Figure 2 in the manuscript.
# Based the data of weekly or monthly pertussis incidence in different countries,
# the figure shows the monthly incidence of pertussis in a country from 2015 to now.

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(openxlsx)
library(MMWRweek)
library(sf)

library(lme4)
library(lmerTest)

fill_color <- rev(c('#DD5129FF', '#FAB255FF', "grey50", '#0F7BA2FF', '#43B284FF'))

scientific_10 <- function(x) {
     x_f <- gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x)))
     x_f[x_f == "10^00"] <- "1"
     # replace 10^0 with 1
     x_f <- parse(text = x_f)
     return(x_f)
}

country_names <- c("AU", "CN", "GB", "JP", "NZ", "SE", "SG", "US")
country_labels <- c("Australia", "China", "United Kingdom", "Japan", "New Zealand",
                    "Sweden", "Singapore", "United States")

# map ---------------------------------------------------------------------

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA") |> 
     mutate(included = iso_a2 %in% country_names)

fig_1 <- ggplot(data = DataMap) +
     geom_sf(aes(fill = included)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     labs(title = "A", x = "", y = "", fill = '')+
     guides(fill = guide_legend(nrow = 1))

# glm plot ----------------------------------------------------------------

plot_glm <- function(i){
     data <- read.xlsx('./Data/Pertussis incidence week or month.xlsx',
                       sheet = country_names[i], detectDates = T) |> 
          filter(Date >= as.Date('2015-1-1')) |> 
          mutate(Year = year(Date),
                 Yday = yday(Date)) |> 
          select(-URL)
     
     # adjust incidence
     
     if (all(is.na(data$Month))) {
          data <- data |>
               mutate(Incidence = Incidence * 4.33)
     }
     start_year_data <- data |>
          group_by(Year) |>
          summarize(min_date = min(Date),
                    .groups = 'drop') |>
          inner_join(data, by = c("min_date" = "Date", 'Year' = 'Year')) |>
          transform(
               Year = Year - 1,
               Yday = Yday + 365 # Adjust Yday for continuity
          ) |> 
          rename(Date = 'min_date')
     
     full_data <- rbind(data, start_year_data) |> 
          mutate(Stage = case_when(
               Year < 2020 ~ '2015-2019',
               Year >= 2020 & Year <= 2021 ~ '2020-2021',
               TRUE ~ as.character(Year)))
     
     fig <- ggplot(filter(full_data, Stage %in% c('2015-2019', '2020-2021')), aes(x = Yday, y = Incidence)) +
          geom_smooth(aes(color = Stage, fill = Stage),
                      method = 'gam',
                      se = T,
                      linewidth = 0.7) +
          geom_line(data = filter(full_data, Stage %in% c('2022', '2023', '2024')), aes(color = Stage),
                    linewidth = 0.7) +
          coord_cartesian(xlim = c(1, 365),
                          ylim = c(0, NA))+
          scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                             labels = month.abb[1:12],
                             expand = c(0, 0)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
          labs(title = paste(LETTERS[i], country_labels[i], sep=": "),
               x = NULL, y = NULL, 
               color = 'Stage', fill = 'Stage') +
          scale_color_manual(values = fill_color) +
          scale_fill_manual(values = fill_color) +
          theme_bw() +
          theme(panel.grid = element_blank(),
                axis.text.y = element_text(color = 'black', face = 'plain'),
                axis.text.x = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                plot.title.position = "plot",
                plot.caption.position = "plot")+
          guides(fill = 'none',
                 color = guide_legend(title = 'Stage'))
     
     return(fig)
}

fig <- map(1:length(country_names), plot_glm)
fig <- fig |> 
     wrap_plots(ncol = 3) +
     plot_layout(guides = 'collect')

ggsave("./Outcome/fig2.pdf",
       fig,
       width = 12,
       height = 7,
       device = cairo_pdf,
       family = "Helvetica")
