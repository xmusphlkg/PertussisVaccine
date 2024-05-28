
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

fill_color <- paletteer_d("MexBrewer::Revolucion")

scientific_10 <- function(x) {
     x_f <- gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x)))
     x_f[x_f == "10^00"] <- "1"
     # replace 10^0 with 1
     x_f <- parse(text = x_f)
     return(x_f)
}

# map ---------------------------------------------------------------------

country_names <- c('CA', 'GB', 'US', 'SE', 'AU', 'CN')
country_labels <- c('Canada', 'United Kingdom', 'United States', 'Sweden', 'Australia', 'China')
DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA") |> 
     mutate(included = iso_a2 %in% country_names)
DataMap$included[!DataMap$included] <- NA

fig_1 <- ggplot(data = DataMap) +
     geom_sf(aes(fill = included)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = '#FAB255FF',
                       na.value = 'grey50',
                       na.translate = T)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none') +
     labs(title = "A", x = NULL, y = NULL, fill = '')+
     guides(fill = guide_legend(nrow = 1))

# figure --------------------------------------------------------------------

plot_ridges <- function(i){
     data <- read.xlsx('./Data/Pertussis age year.xlsx',
                       sheet = country_names[i], detectDates = T)
     names(data)[1] <- 'Year'
     data[is.na(data)] <- 0
     data <- data |> 
          pivot_longer(cols = -Year, names_to = 'Age', values_to = 'Incidence') |> 
          filter(Year >= 2010)  |>
          mutate(StartAge = case_when(
               grepl("-", Age) ~ as.numeric(sub("-.*", "", Age)),
               grepl("\\+", Age) ~ as.numeric(sub("\\+.*", "", Age)),
               TRUE ~ NA_real_),
               EndAge = case_when(
                    grepl("-", Age) ~ as.numeric(sub(".*-", "", Age)),
                    grepl("\\+", Age) ~ 100,
                    TRUE ~ NA_real_)) |> 
          rowwise() |> 
          mutate(AgeList = if (is.na(StartAge)) list(NA_real_) else list(seq(StartAge, EndAge))) |> 
          unnest(AgeList) |> 
          filter(!is.na(AgeList)) |> 
          group_by(Year, AgeList) |> 
          mutate(AverageIncidence = Incidence / (EndAge - StartAge + 1)) |> 
          ungroup() %>%
          select(Year, Age = AgeList, AverageIncidence) |> 
          group_by(Year) |>
          mutate(Weight = AverageIncidence/sum(AverageIncidence),
                 Weight = case_when(
                      is.na(Weight) ~ 0,
                      TRUE ~ Weight
                 )) |> 
          group_by(Year) |> 
          do({
               dens <- density(.$Age, weights = .$Weight, adjust = 0.5, from = 0)
               data.frame(Age = dens$x, Density = dens$y, Year = unique(.$Year))
          })  |> 
          ungroup()
     
     fig <- ggplot(data) +
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
          scale_fill_gradientn(colours = fill_color[c(1:4, 7:10)],
                               limits = c(0, 100))+
          labs(title = paste(LETTERS[i+1], country_labels[i], sep = ": "),
               x = NULL,
               y = NULL,
               fill = "") +
          theme_bw()+
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.border = element_rect(color = 'black', fill = NA),
                plot.background = element_rect(color = 'black', fill = 'white'),
                axis.text.y = element_text(color = 'black', face = 'plain'),
                axis.text.x = element_text(color = 'black', face = 'plain', hjust = 1),
                axis.title = element_text(color = 'black', face = 'plain'),
                legend.box = 'horizontal',
                plot.title.position = 'plot',
                legend.position = "none")
     fig
}

fig_min <- map(1:length(country_names), plot_ridges)

# generate legend ---------------------------------------------------------

data <- data.frame(
     x = 1:10,
     y = runif(10, 0, 100),
     category = factor(rep(1:2, each = 5))
)

p <- ggplot(data, aes(x, y, fill = y)) +
     geom_col(show.legend = TRUE) +
     scale_fill_gradientn(colours = fill_color[c(1:4, 7:10)],
                          limits = c(0, 100),
                          name = "Age")+
     theme(legend.position = "bottom",
           legend.margin = ggplot2::margin(10, 10, 10, 10),
           legend.background = element_rect(fill = "white", color = "black"))+
     guides(fill = guide_colorbar(title.position = "top", title.hjust = 0, barwidth = 20))

g <- ggplotGrob(p)
legends <- gtable_filter(g, "guide-box")

fig <- fig_1 + inset_element(fig_min[[2]], left = 0.27, bottom = 0.66, right = 0.47, top = 0.99) +
     inset_element(fig_min[[1]], left = 0.01, bottom = 0.37, right = 0.21, top = 0.7) +
     inset_element(fig_min[[3]], left = 0.24, bottom = 0.25, right = 0.44, top = 0.58) +
     inset_element(fig_min[[4]], left = 0.50, bottom = 0.42, right = 0.70, top = 0.75) +
     inset_element(fig_min[[5]], left = 0.60, bottom = 0.01, right = 0.80, top = 0.34) +
     inset_element(fig_min[[6]], left = 0.79, bottom = 0.35, right = 0.99, top = 0.68) +
     inset_element(legends, left = 0.03, bottom = 0.01, right = 0.25, top = 0.14)

ggsave("./Outcome/fig1.pdf",
       fig,
       width = 18,
       height = 8,
       device = cairo_pdf,
       family = "Helvetica")
