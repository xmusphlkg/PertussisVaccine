
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
library(ggh4x)
library(randomForest)

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

data_EU <- read.xlsx('./Data/Pertussis case year age.xlsx', sheet = 'EU', detectDates = T) |> 
     filter(Distribution == 'Distribution by age' &
                 !str_detect(RegionCode, 'EU')) |> 
     select(Time, RegionCode, Category, Value) |> 
     mutate(RegionCode = case_when(RegionCode == 'UK' ~ 'GB',
                                   RegionCode == 'EL' ~ 'GR',
                                   TRUE ~ RegionCode)) |> 
     rename(Year = Time,
            Country = RegionCode,
            Age = Category,
            Weight = Value) |> 
     mutate(Age = case_when(Age == "<1" ~ "00-00",
                           TRUE ~ Age))
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
     filter(Country != 'GB' & Country != 'FR' & !is.na(Weight) & !str_detect(Country, 'EU')) |> 
     left_join(data_EU_all, by = c('Year', 'Country')) |> 
     mutate(Cases = round(Weight * CasesAll))

# find the countries only one group data
data_EU |> 
     filter(Cases != 0) |>
     select(Country, Age) |>
     group_by(Country) |> 
     summarise(Count = length(unique(Age)))

print(paste("EU data has", length(unique(data_EU$Country)), "countries", sep = " "))
print(sort(unique(data_EU$Country)))

# data without EU
country_names <- country_names[country_names != 'EU']
data_other <- lapply(country_names, function(x){
     data <- read.xlsx('./Data/Pertussis case year age.xlsx', sheet = x, detectDates = T)
     names(data)[1] <- 'Year'
     data <- data |> 
          # if exist unknow column, drop it
          select(-contains("Unknow")) |>
          pivot_longer(cols = -Year, names_to = 'Age', values_to = 'Incidence') |> 
          filter(!is.na(Incidence)) |>
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
                 CasesAll = sum(Cases))
     
     return(data)
})
data_other <- do.call(rbind, data_other)

DataRaw <- rbind(data_EU, data_other) |> 
     mutate(EndAge = case_when(EndAge == 100 ~ 100,
                               TRUE ~ EndAge +0.9)) |> 
     filter(Year >= 2010)

remove(data_EU, data_other, data_EU_all)

# print the date range for each country
DataRaw |> 
     group_by(Country) |> 
     summarise(Start = min(Year),
               End = max(Year),
               Case = paste(min(CasesAll), max(CasesAll), sep = '-')) |> 
     arrange(Start) |> 
     print(n = Inf)

# Vaccination strategy data

DataVac <- read.csv('./Outcome/S table2.csv')
DataVac <- DataVac |> 
     select(CODE, NAME, TimeLastShot,
            VaccineAdult, VaccineRisk, VaccinePregnant, 
            CoverageDTP1, CoverageDTP3)

# appendix figure --------------------------------------------------------------------

DataNorm <- DataRaw|>
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.1)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(Year, Country, AgeList) |>
     mutate(AverageCases = Cases / ((EndAge - StartAge)*10 + 1)) |>
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
               dens <- density(Age, weights = Weight, kernel = "epanechnikov", adjust = 0.3, from = 0)
               data.frame(Age = dens$x, Density = dens$y)
          }),
          .groups = 'drop'
     ) |>
     unnest(cols = c(Age_Density))

country_names <- c('AT', 'AU', 'BE', 'BG', 'BR', 'CA', 'CN', 'CY', 'CZ', 'DE',
                   'DK', 'EE', 'GR', 'ES', 'FI', 'GB', 'HR', 'HU', 'IE', 
                   'IS', 'IT', 'JP', 'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'NZ', 'PL', 
                   'PT', 'RO', 'SE', 'SG', 'SI', 'SK', 'TH', 'US')
country_labels <- c('Austria', 'Australia', 'Belgium', 'Bulgaria', 'Brazil', 'Canada', 'China', 'Cyprus', 'Czech Republic', 'Germany',
                    'Denmark', 'Estonia', 'Greece', 'Spain', 'Finland', 'United Kingdom', 'Croatia', 'Hungary', 'Ireland', 
                    'Iceland', 'Italy', 'Japan', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 'Netherlands', 'Norway', 'New Zealand', 'Poland', 
                    'Portugal', 'Romania', 'Sweden', 'Singapore', 'Slovenia', 'Slovakia', 'Thailand', 'United States')
fill_color <- rev(c("#1E313EFF", "#4E475FFF", "#8B5975FF", "#C86C7CFF", "#FA8975FF"))

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
          scale_y_continuous(breaks = seq(2010, 2023, 2),
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

fill_color_1 <- rev(c("#1D3141FF", "#096168FF", "#209478FF", "#75C56EFF", "#E2EE5EFF"))

DataYear <- DataNorm |> 
     group_by(Country, Year) |>
     arrange(Age) |>
     mutate(cum_weight = cumsum(Density)) |>
     summarise(MedianAge = Age[min(which(cum_weight >= sum(Density) / 2))],
               .groups = 'drop')

fig_2 <- DataYear |> 
     mutate(Country = factor(Country,
                             levels = country_names,
                             labels = country_labels)) |>
     ggplot(aes(x = as.numeric(Year),
                y = MedianAge,
                color = MedianAge,
                group = Country))+
     geom_line()+
     geom_point()+
     facet_wrap(~Country, scales = 'free_y', ncol = 8)+
     scale_y_continuous(expand = expansion(mult = c(0.15, 0.15)))+
     scale_x_continuous(breaks = seq(2010, 2023, 5),
                        expand = expansion(mult = c(0.02, 0.02)))+
     scale_color_gradientn(colours = fill_color,
                          limits = c(0, 100))+
     theme_bw()+
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text.y = element_text(color = 'black', face = 'plain'),
           axis.text.x = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'inside',
           legend.position.inside = c(1, 0),
           legend.justification.inside = c(1, 0),
           legend.title.position = 'top',
           plot.title.position = 'plot')+
     labs(x = 'Year',
          title = 'B',
          y = 'Estimated median age')+
     guides(color = guide_colorbar(barwidth = 12,
                                   direction = 'horizontal'))

# appendix figure --------------------------------------------------------------------

fig <- DataYear |> 
     mutate(Year = as.character(Year)) |> 
     ggplot()+
     geom_tile(aes(x = Year,
                   y = Country,
                   fill = MedianAge))+
     geom_text(aes(x = Year,
                   y = Country,
                   label = formatC(MedianAge, format = 'f', digits = 1)),
               color = 'white',
               size = 5)+
     scale_fill_gradientn(colours = fill_color_1,
                          limits = c(1, 100),
                          breaks = c(1, 2, 5, 10, 20, 30, 100),
                          trans = 'log10',
                          na.value = 'white')+
     scale_y_discrete(limits = country_names,
                      labels = country_labels)+
     labs(x = 'Year',
          y = NULL,
          fill = 'Median age')+
     theme_bw()+
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           plot.title.position = 'plot')+
     guides(fill = guide_colorbar(barwidth = 20))

ggsave(fig,
       filename = "./Outcome/S fig1_39.png",
       dpi = 300,
       width = 12,
       height = 15)

# panel a -----------------------------------------------------------------

DataAll <- DataRaw|>
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(Country, AgeList) |>
     mutate(AverageCases = Cases / (EndAge - StartAge + 1)) |>
     ungroup() |>
     select(Country, Age = AgeList, AverageCases) |>
     group_by(Country) |>
     mutate(
          Weight = AverageCases / sum(AverageCases),
          Weight = case_when(
               is.na(Weight) ~ 0,
               TRUE ~ Weight
          )
     ) |>
     summarise(
          Age_Density = list({
               dens <- density(Age, weights = Weight, kernel = "epanechnikov", adjust = 0.3, from = 0)
               data.frame(Age = dens$x, Density = dens$y)
          }),
          .groups = 'drop'
     ) |>
     unnest(cols = c(Age_Density))

DataMedian <- DataAll |> 
     group_by(Country) |>
     arrange(Age) |>
     mutate(cum_weight = cumsum(Density)) |>
     summarise(MedianAge = Age[min(which(cum_weight >= sum(Density) / 2))],
               .groups = 'drop')

fig_1 <- DataAll |> 
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
     geom_point(data = DataMedian,
                aes(x = MedianAge,
                    y = Country),
                shape = 3,
                show.legend = F,
                size = 1)+
     scale_x_continuous(limits = c(0, 100),
                        expand = c(0, 0),
                        breaks = seq(0, 100, 10))+
     scale_fill_gradientn(colours = fill_color,
                          limits = c(0, 100))+
     scale_y_discrete(limits = rev(country_names),
                      labels = rev(country_labels))+
     labs(title = 'A',
          y = NULL,
          x = 'Age',
          fill = "Age")+
     theme_bw()+
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.background = element_blank(),
           strip.text = element_text(size = 14, hjust = 0, face = 'plain'),
           strip.placement = 'outside',
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.direction = 'horizontal',
           plot.title.position = 'plot',
           legend.position = 'none')+
     guides(fill = guide_colorbar(barwidth = 20))


data <- list(
     'B' = DataMedian |>
          mutate(Country = factor(Country,
                                  levels = country_names,
                                  labels = country_labels)),
     'A' = DataYear |> 
                  mutate(Country = factor(Country,
                                          levels = country_names,
                                          labels = country_labels)) |>
                  pivot_wider(names_from = Country, values_from = MedianAge) |>
                  mutate(Year = as.numeric(Year)))

write.xlsx(data, './Outcome/fig data/fig1.xlsx')

# outcome -----------------------------------------------------------------

fig <- cowplot::plot_grid(fig_1, fig_2, nrow = 1, rel_widths = c(1, 3.5))

ggsave("./Outcome/fig1.pdf",
       fig,
       width = 15,
       height = 8,
       device = cairo_pdf)

ggsave("./Outcome/fig1.png",
       fig,
       width = 15,
       height = 8)
