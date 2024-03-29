
library(openxlsx)
library(tidyverse)


# figure 1 ----------------------------------------------------------------

df_1 <- read.xlsx("Fig Data/fig1.xlsx",
                  detectDates = TRUE)

i <- 1
country_list <- sort(unique(df_1$Country))

fig <- list()

for (i in 1:length(country_list)) {
     country <- country_list[i]
     df_country <- df_1  |> 
          filter(Country == country) |> 
          select(-Country)
     fig[[paste('panel', letters[i*2-1])]] <- df_country
     
     df_country <- df_country |> 
          group_by(Year) |> 
          filter(Year < 2024) |>
          summarise(Cases = sum(Cases))
     fig[[paste('panel', letters[i*2])]] <- df_country
}

write.xlsx(fig, "appendix/fig data/fig 1.xlsx", asTable = TRUE, rowNames = FALSE)

remove(df_1, i, country_list, fig, country, df_country)

# figure 2 ----------------------------------------------------------------

df_1 <- read.xlsx("Fig Data/fig2.xlsx",
                  sheet = "Sheet 1",
                  detectDates = TRUE)
df_2 <- read.xlsx("Fig Data/fig2.xlsx",
                  sheet = "Sheet 2",
                  detectDates = TRUE)

i <- 1
country_list <- sort(unique(df_1$country))

fig <- list()

for (i in 1:length(country_list)) {
     country <- country_list[i]
     
     df_country <- df_2 |> 
          filter(country == country)  |> 
          select(-c(country, model))
     fig[[paste('panel', letters[i*2 - 1])]] <- df_country
     
     df_country <- df_1  |> 
          filter(country == country) |> 
          select(-c(country, model))
     fig[[paste('panel', letters[i*2])]] <- df_country
}

write.xlsx(fig, "appendix/fig data/fig 2.xlsx", asTable = TRUE, rowNames = FALSE)

remove(df_1, df_2, i, country_list, fig, country, df_country)

# figure 3 ----------------------------------------------------------------

df_1 <- read.xlsx("Fig Data/fig1.xlsx",
                  detectDates = TRUE)

i <- 1
country_list <- sort(unique(df_1$Country))

fig <- list()

for (i in 1:length(country_list)) {
     country <- country_list[i]
     
     df_country <- df_1 |> 
          filter(Country == country)  |> 
          select(-c(Country))
     fig[[paste('panel', letters[i*2 - 1])]] <- df_country
     
     df_country <- df_1 |> 
          filter(Country == country)  |> 
          select(-c(Country)) |> 
          filter(stage != '2023 Jun onwards') |> 
          group_by(stage) |>
          summarise(Max = max(Cases),
                    Q3 = quantile(Cases, 0.75),
                    Median = median(Cases),
                    Q1 = quantile(Cases, 0.25),
                    Min = min(Cases))
     fig[[paste('panel', letters[i*2])]] <- df_country
}

write.xlsx(fig, "appendix/fig data/fig 3.xlsx", asTable = TRUE, rowNames = FALSE)

remove(df_1, i, country_list, fig, country, df_country)

# figure 4 ----------------------------------------------------------------

df_1 <- read.xlsx("Fig Data/fig4.xlsx",
                  sheet = "Sheet 1",
                  detectDates = TRUE)
df_2 <- read.xlsx("Fig Data/fig4.xlsx",
                  sheet = "Sheet 2",
                  detectDates = TRUE)

i <- 1
country_list <- sort(unique(df_1$country))

fig <- list()

for (i in 1:length(country_list)) {
     country <- country_list[i]
     
     df_country <- df_2 |> 
          filter(country == country)  |> 
          select(-c(country, model))
     fig[[paste('panel', letters[i*2 - 1])]] <- df_country
     
     df_country <- df_1  |> 
          filter(country == country) |> 
          select(-c(country, model))
     fig[[paste('panel', letters[i*2])]] <- df_country
}

write.xlsx(fig, "appendix/fig data/fig 4.xlsx", asTable = TRUE, rowNames = FALSE)

remove(df_1, df_2, i, country_list, fig, country, df_country)
