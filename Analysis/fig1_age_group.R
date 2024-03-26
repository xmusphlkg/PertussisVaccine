
library(tidyverse)
library(openxlsx)
library(patchwork)
library(ggsci)

breaks <- seq(1991, 2023, 2)
fill_color <- pal_npg()(5)
names(fill_color) <- c('00-04', '05-09', '10-14', '15+', 'Missing')

# AU ----------------------------------------------------------------------

df <- read.xlsx('./annual_us.xlsx', sheet = 'AU')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '00-04' ~ '00-04',
          age_group == '05-09' ~ '05-09',
          age_group == '10-14' ~ '10-14',
          # other age groups
          TRUE ~ '15+'
     ))
names(df)[1] <- 'year'
df <- df |> 
     filter(year <= 2023)

fig1 <- ggplot(df, aes(x = year, y = count, fill = age_group_10)) +
     geom_col(position = 'fill') +
     coord_cartesian(ylim = c(0, 1)) +
     scale_fill_manual(values = fill_color) +
     scale_x_continuous(breaks = breaks,
                        limits = c(1990.5, 2023.5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     theme_bw() +
     labs(title = 'a',
          x = 'Year',
          y = 'Percentage',
          fill = 'Age Group')

# CN ----------------------------------------------------------------------



# US ----------------------------------------------------------------------



# UK ----------------------------------------------------------------------

df <- read.xlsx('./annual_us.xlsx', sheet = 'UK')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '00-04' ~ '00-04',
          age_group == '05-09' ~ '05-09',
          age_group == '10-14' ~ '10-14',
          # other age groups
          TRUE ~ '15+'
     ))
names(df)[1] <- 'year'
df <- df |> 
     filter(year <= 2023)

fig4 <- ggplot(df, aes(x = year, y = count, fill = age_group_10)) +
     geom_col(position = 'fill', show.legend = F) +
     coord_cartesian(ylim = c(0, 1)) +
     scale_fill_manual(values = fill_color) +
     scale_x_continuous(breaks = breaks,
                        limits = c(1990.5, 2023.5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     theme_bw() +
     labs(title = 'd',
          x = 'Year',
          y = 'Percentage',
          fill = 'Age Group')

ggsave(filename = './appendix/S6.png',
       plot = fig1 + fig4 + plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom'),
       width = 12,
       height = 10) 
