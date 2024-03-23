
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)

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

df_us <- df_raw |> 
     filter(Country == 'US') |> 
     select(Year, Week, Cases, URL)
df_us <- left_join(df_epiweek, df_us, by = c('Year', 'Week')) |> 
     arrange(Date) |> 
     mutate(Cases = as.integer(Cases),
            Country = 'US')

df_uk <- df_raw |> 
     filter(Country == 'UK') |> 
     select(Year, Week, Cases, URL)
df_uk <- left_join(df_epiweek, df_uk, by = c('Year', 'Week')) |>
     arrange(Date) |> 
     mutate(Cases = as.integer(Cases),
            Country = 'UK')

df_cn <- df_raw |> 
     filter(Country == 'CN') |> 
     select(Date, Year, Month, Cases, URL) |> 
     mutate(Cases = as.integer(Cases),
            Date = as.Date(Date),
            Country = 'CN')
df_au <- df_raw |> 
     filter(Country == 'AU') |> 
     select(Date, Year, Month, Cases, URL) |> 
     mutate(Cases = as.integer(Cases),
            Date = as.Date(Date),
            Country = 'AU')

df_clean <- bind_rows(df_us, df_uk, df_cn, df_au) |> 
     arrange(Date) |> 
     select(Country, Date, Year, Month, Week, Cases) |> 
     mutate(stage = case_when(
          Date < as.Date('2020-01-01') ~ '2015 Jan. to 2019 Dec.',
          Date >= as.Date('2020-01-01') & Date < as.Date('2023-07-01') ~ '2020 Jan. to 2023 Jun.',
          Date >= as.Date('2023-07-01') ~ '2023 Jun. onwards'
     ))

rm(df_us, df_uk, df_cn, df_epiweek)

# plot ---------------------------------------------------------------------

ggplot(df_clean, aes(x = Date, y = Cases, color = Country)) +
     geom_line() +
     scale_x_date(date_labels = '%Y') +
     scale_y_continuous(labels = scales::comma) +
     facet_wrap(~Country, scales = 'free_y', nrow = 3) +
     labs(title = 'Epidemic curve of pertussis cases',
          x = 'Date',
          y = 'Cases',
          color = 'Country') +
     theme_bw() +
     theme(legend.position = 'none')

# figure 1 ----------------------------------------------------------------

i <- 1
country_list <- sort(unique(df_clean$Country))
ylabel_list <- c('Monthly incidence', 'Monthly incidence', 'Weekly incidence', 'Weekly incidence')

plot_epidemic <- function(i){
     data <- df_clean |> 
          filter(Country == country_list[i])
     plot_breaks <- pretty(c(0, max(data$Cases)))
     plot_range <- range(plot_breaks)
     
     ggplot(data)+
          geom_line(aes(x = Date, y = Cases, color= stage)) +
          scale_x_date(date_labels = '%Y',
                       date_breaks = 'year',
                       limits = c(as.Date('2015-01-01'), as.Date('2024-03-09')),
                       expand = expansion(add = c(7, 7))) +
          scale_y_continuous(labels = scientific_10,
                             limits = plot_range,
                             breaks = plot_breaks,
                             expand = expansion(mult = c(0, 0))) +
          scale_color_manual(values = fill_color) +
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
               legend.background = element_rect(fill = "transparent", colour = "transparent"),
               axis.title = element_text(face = "bold", size = 12, color = "black"),
               axis.text = element_text(size = 12, color = "black")
          ) +
          labs(title = letters[i],
               y = ylabel_list[i],
               x = 'Date',
               color = 'Stage')
}

outcome <- lapply(1:4, plot_epidemic) |> wrap_plots(ncol = 1, guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './main/Fig1.pdf',
       plot = outcome,
       width = 12,
       height = 10, 
       device = cairo_pdf,
       family = 'Times New Roman')

write.xlsx(df_clean,
          './Fig Data/fig1.xlsx')
