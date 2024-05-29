
# This script generates Figure 1 in the manuscript.

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(openxlsx)
library(ggridges)
library(paletteer)
library(grid)
library(gtable)
library(sf)

fill_color <- paletteer_d("MexBrewer::Revolucion")

scientific_10 <- function(x) {
     x_f <- gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x)))
     x_f[x_f == "10^00"] <- "1"
     # replace 10^0 with 1
     x_f <- parse(text = x_f)
     return(x_f)
}


# data --------------------------------------------------------------------

country_names <- openxlsx::getSheetNames('./Data/Pertussis case year age.xlsx')

# EU data

data_EU <- read.xlsx('./Data/Pertussis case year age.xlsx', sheet = 'EU', detectDates = T)
names(data_EU) <- c('Year', 'Country', 'Age', 'Weight')
data_EU_all <- read.csv('./Data/ECDC_surveillance_data_Pertussis.csv') |> 
     select(Time, RegionCode, NumValue) |> 
     rename(Year = Time,
            Country = RegionCode,
            CasesAll = NumValue)
data_EU <- data_EU |> 
     mutate(Weight = as.numeric(Weight)/100,
            StartAge = case_when(grepl("-", Age) ~ as.numeric(sub("-.*", "", Age)),
                                 grepl("\\+", Age) ~ as.numeric(sub("\\+.*", "", Age)),
                                 TRUE ~ NA_real_),
            EndAge = case_when(grepl("-", Age) ~ as.numeric(sub(".*-", "", Age)),
                               grepl("\\+", Age) ~ 100,
                               TRUE ~ NA_real_)) |> 
     filter(Country != 'UK' & !is.na(Weight) & !str_detect(Country, 'EU')) |> 
     left_join(data_EU_all, by = c('Year', 'Country')) |> 
     mutate(Cases = round(Weight * CasesAll))
print(paste("EU data has", length(unique(data_EU$Country)), "countries", sep = " "))
print(unique(data_EU$Country))

# data without EU
country_names <- country_names[country_names != 'EU']
data_other <- lapply(country_names, function(x){
     data <- read.xlsx('./Data/Pertussis case year age.xlsx', sheet = x, detectDates = T)
     names(data)[1] <- 'Year'
     data[is.na(data)] <- 0
     data <- data |> 
          # if exist unknow column, drop it
          select(-contains("Unknow")) |>
          pivot_longer(cols = -Year, names_to = 'Age', values_to = 'Incidence') |> 
          filter(Year >= 2010)  |>
          mutate(StartAge = case_when(grepl("-", Age) ~ as.numeric(sub("-.*", "", Age)),
                                      grepl("\\+", Age) ~ as.numeric(sub("\\+.*", "", Age)),
                                      TRUE ~ NA_real_),
                 EndAge = case_when(grepl("-", Age) ~ as.numeric(sub(".*-", "", Age)),
                                    grepl("\\+", Age) ~ 100,
                                    TRUE ~ NA_real_),
                 Country = x) |> 
          rowwise() |> 
          rename(Cases = Incidence) |> 
          # standardize incidence by each country and year
          group_by(Country, Year) |>
          mutate(Weight = Cases/sum(Cases),
                 Weight = case_when(is.na(Weight) ~ 0,
                                    TRUE ~ Weight),
                 CasesAll = sum(Cases))
     
     return(data)
})
data_other <- do.call(rbind, data_other)

DataRaw <- rbind(data_EU, data_other) |> 
     mutate(EndAge = case_when(EndAge == 100 ~ 100,
                               TRUE ~ EndAge +0.9))

write.csv(DataRaw, './Outcome/fig1.csv', row.names = F)

remove(data_EU, data_other, data_EU_all)

country_names <- unique(DataRaw$Country) |> 
     sort() |> 
     as.character()

# panel a ---------------------------------------------------------------------

DataNorm <- DataRaw|>
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(Year, Country, AgeList) |>
     mutate(AverageCases = Cases / (EndAge - StartAge + 1)) |>
     ungroup() |>
     select(Country, Year, Age = AgeList, AverageCases) |>
     group_by(Country, Year) |>
     mutate(
          Weight = AverageCases / sum(AverageCases),
          Weight = case_when(
               is.na(Weight) ~ 0,
               TRUE ~ Weight
          )
     ) |>
     ungroup() |>
     group_by(Country, Year) |>
     summarise(
          Age_Density = list({
               dens <- density(Age, weights = Weight, adjust = 0.5, from = 0)
               data.frame(Age = dens$x, Density = dens$y)
          }),
          .groups = 'drop'
     ) |>
     unnest(cols = c(Age_Density))

Data2019 <- DataNorm |>
     filter(Year <= 2019) |>
     mutate(AgeGroup = case_when(Age < 5 ~ "0-4",
                                 Age < 10 ~ "5-9",
                                 Age < 15 ~ "10-14",
                                 Age >= 15 ~ "15+"),
            AgeGroup = factor(AgeGroup, levels = c("0-4", "5-9", "10-14", "15+"))) |> 
     group_by(Country, AgeGroup) |>
     summarise(Density = sum(Density), .groups = 'drop') |> 
     group_by(Country) |>
     filter(Density == max(Density)) |> 
     select(Country, AgeGroup)

# using 2010-2019 data find the main age group

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA") |> 
     left_join(Data2019, by = c("iso_a2" = "Country"))

fill_color <- c('#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF')

fig_1 <- ggplot(data = DataMap) +
     geom_sf(aes(fill = AgeGroup)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = c("0-4", "5-9", "10-14", "15+"),
                       limits = c("0-4", "5-9", "10-14", "15+"),
                       na.value = 'white',
                       na.translate = T)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0.01, 0.01),
           legend.justification = c(0, 0)) +
     labs(title = "A", x = NULL, y = NULL, fill = 'Age group')+
     guides(fill = guide_legend(nrow = 1))

# appendix figure --------------------------------------------------------------------

country_names <- c('AT', 'AU', 'BE', 'BG', 'BR', 'CA', 'CN', 'CY', 'CZ', 'DE',
                   'DK', 'EE', 'EL', 'ES', 'FI', 'FR', 'GB', 'HR', 'HU', 'IE', 
                   'IS', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'NZ', 'PL', 
                   'PT', 'RO', 'SE', 'SG', 'SI', 'SK', 'US')
country_labels <- c('Austria', 'Australia', 'Belgium', 'Bulgaria', 'Brazil', 'Canada', 'China', 'Cyprus', 'Czech Republic', 'Germany',
                    'Denmark', 'Estonia', 'Greece', 'Spain', 'Finland', 'France', 'United Kingdom', 'Croatia', 'Hungary', 'Ireland', 
                    'Iceland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 'Netherlands', 'Norway', 'New Zealand', 'Poland', 
                    'Portugal', 'Romania', 'Sweden', 'Singapore', 'Slovenia', 'Slovakia', 'United States')
fill_color <- rev(paletteer_d("awtools::a_palette"))

plot_ridges <- function(i){
     df <- DataNorm |>
          filter(Country == country_names[i])
     
     fig <- ggplot(df) +
          geom_density_ridges_gradient(mapping = aes(x = Age,
                                                     y = Year,
                                                     group = Year,
                                                     fill = Age,
                                                     height = Density),
                                       scale = 1.2,
                                       stat = "identity",
                                       rel_min_height = 0.01) +
          scale_x_continuous(limits = c(0, 100),
                             expand = c(0, 0),
                             breaks = seq(0, 100, 10)) +
          scale_y_continuous(breaks = seq(2010, 2023, 3),
                             expand = expansion(mult = c(0, 0.04)))+
          scale_fill_gradientn(colours = fill_color,
                               limits = c(0, 100))+
          labs(title = country_labels[i],
               x = NULL,
               y = NULL,
               fill = "") +
          theme_bw()+
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.background = element_rect(color = 'black', fill = 'white'),
                axis.text.y = element_text(color = 'black', face = 'plain'),
                axis.text.x = element_text(color = 'black', face = 'plain', hjust = 1),
                axis.title = element_text(color = 'black', face = 'plain'),
                legend.box = 'horizontal',
                plot.title.position = 'plot',
                legend.position = "none")
     ggsave(fig,
            filename = paste0("./Outcome/S fig1_", i, ".png"),
            dpi = 300,
            width = 6,
            height = 4)
     
     return(country_names[i])
}

fig_s <- map(1:length(country_names), plot_ridges)

# panel b -----------------------------------------------------------------

DataPeriod <- DataRaw |> 
     mutate(Period = case_when(Year < 2020 ~ "Before 2020",
                               TRUE ~ as.character(Year)),
            Period = factor(Period, levels = c("Before 2020", "2020", "2021", "2022", "2023"))) |> 
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(Period, Country, AgeList) |>
     mutate(AverageCases = Cases / (EndAge - StartAge + 1)) |>
     ungroup() |>
     select(Country, Period, Age = AgeList, AverageCases) |>
     group_by(Country, Period) |>
     mutate(
          Weight = AverageCases / sum(AverageCases),
          Weight = case_when(
               is.na(Weight) ~ 0,
               TRUE ~ Weight
          )
     ) |>
     ungroup() |>
     group_by(Country, Period) |>
     summarise(
          Age_Density = list({
               dens <- density(Age, weights = Weight, adjust = 0.5, from = 0)
               data.frame(Age = dens$x, Density = dens$y)
          }),
          .groups = 'drop'
     ) |>
     unnest(cols = c(Age_Density))

fig_2 <- DataPeriod |> 
     mutate(Period = factor(Period,
                            levels = c("Before 2020", "2020", "2021", "2022", "2023"),
                            labels = LETTERS[2:6])) |> 
     ggplot()+
     geom_density_ridges_gradient(mapping = aes(x = Age,
                                                y = Country,
                                                group = Country,
                                                fill = Age,
                                                height = Density),
                                  color = 'white',
                                  scale = 1.2,
                                  stat = "identity",
                                  rel_min_height = 0.01)+
     facet_wrap(~Period, scales = 'free_y', ncol = 2)+
     coord_flip()+
     scale_x_continuous(limits = c(0, 100),
                        expand = c(0, 0),
                        breaks = seq(0, 100, 10))+
     scale_fill_gradientn(colours = fill_color,
                          limits = c(0, 100))+
     scale_y_discrete(limits = rev(country_names),
                      labels = rev(country_labels))+
     labs(title = NULL,
          y = NULL,
          x = 'Age',
          fill = "Age")+
     theme_bw()+
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.background = element_blank(),
           strip.text = element_text(size = 14, hjust = 0, face = 'plain'),
           strip.placement = 'outside',
           axis.text.y = element_text(color = 'black', face = 'plain'),
           axis.text.x = element_text(color = 'black', face = 'plain', hjust = 1, angle = 90, vjust = 0.5),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.direction = 'horizontal',
           plot.title.position = 'plot',
           legend.position = c(0.80, 0.15))+
     guides(fill = guide_colorbar(barwidth = 20))

# generate legend ---------------------------------------------------------

design <- "
AAACCC
BBBBBB
"

fig <- fig_1 + fig_2 +
     plot_layout(design = design, heights = c(1, 2.7))

ggsave("./Outcome/fig1.pdf",
       fig,
       width = 14,
       height = 14.3,
       device = cairo_pdf,
       family = "Helvetica")
