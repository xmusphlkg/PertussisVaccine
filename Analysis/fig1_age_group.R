
library(tidyverse)
library(openxlsx)
library(patchwork)
library(ggsci)

breaks <- seq(1991, 2023, 2)
fill_color <- pal_npg()(4)
names(fill_color) <- c('00-04', '05-14', '15+', 'Missing')

# AU ----------------------------------------------------------------------

df <- read.xlsx('./annual.xlsx', sheet = 'AU')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '00-04' ~ '00-04',
          age_group == '05-09' ~ '05-14',
          age_group == '10-14' ~ '05-14',
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

df <- read.xlsx('./annual.xlsx', sheet = 'CN')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'year', values_to = 'count') |>
     rename(age_group = 'Age.Group') |>
     mutate(age_group_10 = case_when(
          age_group == '0-' ~ '00-04',
          age_group == '1-' ~ '00-04',
          age_group == '2-' ~ '00-04',
          age_group == '3-' ~ '00-04',
          age_group == '4-' ~ '00-04',
          age_group == '5-' ~ '05-14',
          age_group == '6-' ~ '05-14',
          age_group == '7-' ~ '05-14',
          age_group == '8-' ~ '05-14',
          age_group == '9-' ~ '05-14',
          age_group == '10-' ~ '05-14',
          age_group == '11-' ~ '05-14',
          age_group == '12-' ~ '05-14',
          age_group == '13-' ~ '05-14',
          age_group == '14-' ~ '05-14',
          # other age groups
          TRUE ~ '15+'),
          year = as.integer(year))
df <- df |> 
     filter(year <= 2023)

fig2 <- ggplot(df, aes(x = year, y = count, fill = age_group_10)) +
     geom_col(position = 'fill', show.legend = F) +
     coord_cartesian(ylim = c(0, 1)) +
     scale_fill_manual(values = fill_color) +
     scale_x_continuous(breaks = breaks,
                        limits = c(1990.5, 2023.5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     theme_bw() +
     labs(title = 'b',
          x = 'Year',
          y = 'Percentage',
          fill = 'Age Group')

# UK ----------------------------------------------------------------------

df <- read.xlsx('./annual.xlsx', sheet = 'UK')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '00-04' ~ '00-04',
          age_group == '05-09' ~ '05-14',
          age_group == '10-14' ~ '05-14',
          # other age groups
          TRUE ~ '15+'
     ))
names(df)[1] <- 'year'
df <- df |> 
     filter(year <= 2023)

fig3 <- ggplot(df, aes(x = year, y = count, fill = age_group_10)) +
     geom_col(position = 'fill', show.legend = F) +
     coord_cartesian(ylim = c(0, 1)) +
     scale_fill_manual(values = fill_color) +
     scale_x_continuous(breaks = breaks,
                        limits = c(1990.5, 2023.5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     theme_bw() +
     labs(title = 'c',
          x = 'Year',
          y = 'Percentage',
          fill = 'Age Group')

# US ----------------------------------------------------------------------

df <- read.xlsx('./annual.xlsx', sheet = 'US')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '00-01' ~ '00-04',
          age_group == '02-04' ~ '00-04',
          age_group == '05-14' ~ '05-14',
          age_group == '05-14' ~ '05-14',
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

fig_1 <- fig1 + fig2 + fig3 + fig4 +
     plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom')

# US ----------------------------------------------------------------------

fill_color <- pal_npg()(5)
names(fill_color) <- c('00-06', '07-10', '11-19', 'Missing', '20+')

df <- read.xlsx('./annual.xlsx', sheet = 'US_1')
df <- df |> 
     pivot_longer(cols = 2:ncol(df), names_to = 'age_group', values_to = 'count') |> 
     mutate(age_group_10 = case_when(
          age_group == "No.information.provided" ~ 'Missing',
          age_group == '<6mos' ~ '00-06',
          age_group == '6-11mos' ~ '00-06',
          age_group == '01-06' ~ '00-06',
          # other age groups
          TRUE ~ as.character(age_group)
     ))

names(df)[1] <- 'year'
df <- df |> 
     filter(year <= 2023)

fig_2 <- ggplot(df, aes(x = year, y = count, fill = age_group_10)) +
     geom_col(position = 'fill', show.legend = T) +
     coord_cartesian(ylim = c(0, 1)) +
     scale_fill_manual(values = fill_color) +
     scale_x_continuous(breaks = breaks,
                        limits = c(1990.5, 2023.5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     theme_bw() +
     theme(legend.position = 'bottom') +
     labs(x = 'Year',
          y = 'Percentage',
          title = 'e',
          fill = 'Age Group')

ggsave(filename = './appendix/S3.png',
       plot = cowplot::plot_grid(fig_1, fig_2,
                                 rel_heights = c(4, 1.15),
                                 ncol = 1),
       dpi = 300,
       width = 12,
       height = 12) 
