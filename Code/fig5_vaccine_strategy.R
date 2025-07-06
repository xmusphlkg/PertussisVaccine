
# This script generates Figure 5 in the manuscript.
# To analyze the relationship between the pertussis resurgence and vaccination strategy in different countries,
# we first cluster the countries based on the incidence of pertussis from 2010 to 2019.
# We then examine the importance of different variables in predicting the disease burden in each cluster 
# using random forest analysis.


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(openxlsx)
library(sf)
library(cowplot)
library(paletteer)
library(Cairo)
library(factoextra)
library(randomForest)
library(edarf)
library(caret)
library(GGally)
library(scales)

## resampling
library(caret)
library(DMwR)

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table2.csv") |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineAP = as.factor(VaccineCode %in% c('aP', 'Both')),
            VaccineWP = as.factor(VaccineCode %in% c('wP', 'Both')),
            OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Low', 'Normal', 'High', 'Resurgence')),
            OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Low', 'Normal', 'High', 'Resurgence')))
DataAll <- DataAll[,names(DataAll) != 'Cluster']

ggsave("./Outcome/S fig5_1.png",
       DataAll |>
            select(CoverageDTP1:TimeFirstShot) |>
            ggpairs(columnLabels = c('DTP1 coverage', 'DTP3 coverage',
                                     'Vaccine for pregnant', 'Vaccine for adult', 'Vaccine for risk',
                                     'Time of\nlast general vaccine shot (months)',
                                     'Time of\nfirst general vaccine shot (months)'),
                    bins = 30,
                    title = 'Correlation plot'),
       width = 15,
       height = 15,
       dpi = 300)

# map data ----------------------------------------------------------------

DataMap <- st_read("./Data/world.zh.json", quiet = TRUE) |> 
     filter(iso_a3  != "ATA")
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataCountry <- read.csv('./Data/GBD/iso_code.csv') |> 
     select(-location_name)

DataInciRaw <- read.csv("./Data/GBD/IHME-GBD_2021_DATA-a5bddb9a-1.csv") |> 
     filter(measure_name == 'Incidence',
            location_name != 'Global',
            year %in% 2010:2019,
            metric_name == 'Rate',
            age_name == 'All ages') |>
     select(location_id, year, val) |> 
     left_join(DataCountry, by = c('location_id'))
DataInciRaw <- DataInciRaw |> 
     rename(NAME = ISO3) |> 
     select(NAME, year, val) |>
     pivot_wider(names_from = year, values_from = val) |>
     mutate(across('2019':'2010', ~as.numeric(.)))

DataInciRaw |> 
     select(NAME, `2019`:`2010`) |> 
     pivot_longer(cols = `2019`:`2010`, names_to = 'Year', values_to = 'Incidence') |>
     group_by(NAME) |>
     summarise(Incidence = median(Incidence, na.rm = T)) |> 
     arrange(desc(Incidence)) |> 
     head(10) |> 
     as.data.frame()

# incidence cluster -------------------------------------------------------

set.seed(20240521)

fill_color_disease <- rev(c('#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF' ))

DataInci <- DataInciRaw |>
     select('2019':'2010') |> 
     as.data.frame()
names(DataInci) <- paste0("X", names(DataInci))
rownames(DataInci) <- DataInciRaw$NAME

# random forest imputation
DataMatInci <-DataInci|>
     log10() |>
     as.matrix()
rownames(DataMatInci) <- DataInciRaw$NAME

# hierarchical clustering
hcdata <- DataMatInci |>  
     hkmeans(3)
DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     mutate(Cluster = as.factor(hcdata$cluster),
            ClusterG = fct_recode(Cluster, 'Low' = '3', 'Moderate' = '1', 'High' = '2'),
            ClusterG = factor(ClusterG, levels = c('Low', 'Moderate', 'High'))) |>  
     select(Cluster, ClusterG) |> 
     rownames_to_column('NAME')
DataAll <- DataAll |> 
     left_join(DataCluster, by = c('CODE' = 'NAME'))

# merge data
DataInci$NAME <- rownames(DataInci)

DataCluster <- DataCluster|> 
     left_join(DataInci, by = c('NAME')) |>
     pivot_longer(cols = -c(NAME, Cluster, ClusterG), names_to = 'Year', values_to = 'Incidence') |> 
     mutate(Year = as.numeric(gsub('X', '', Year)))

DataCluster |> 
     group_by(Cluster, ClusterG, NAME) |>
     summarise(Incidence = median(Incidence, na.rm = T)) |>
     group_by(Cluster, ClusterG) |>
     summarise(IncidenceMax = max(Incidence, na.rm = T),
               IncidenceMin = min(Incidence, na.rm = T)) |> 
     as.data.frame() |> 
     print()

# panel a -----------------------------------------------------------------

table(hcdata$cluster)

fig_1 <- fviz_cluster(hcdata,
                      data = scale(DataMatInci),
                      geom = 'point',
                      palette = c("#DD5129FF", "#FAB255FF", "#43B284FF"),
                      ggtheme = theme_bw(),
                      main = 'A')+
     theme(legend.position = 'none',
           plot.title.position = 'plot')

write.csv(DataAll, "./Outcome/S table2.csv", row.names = F)

# panel b -----------------------------------------------------------------

DataMapPlot <- DataMap |> 
     left_join(DataAll[,c('CODE', 'ClusterG')], by = c('iso_a3' = 'CODE'))
table(DataAll$ClusterG)

fill_color <- rev(c('#DD5129FF', '#FAB255FF', '#43B284FF'))

fig_2_m <- plot_map_col(DataAll$ClusterG, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$ClusterG),
                       limits = levels(DataAll$ClusterG))+
     scale_x_discrete(limits = levels(DataAll$ClusterG))+
     labs(title = 'Disease burden')

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = ClusterG)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$ClusterG),
                       limits = levels(DataAll$ClusterG),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none',
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL, fill = 'Disease burden')+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.55)

# panel d -----------------------------------------------------------------

DataLabel <- data.frame(
     Variable = c("CoverageDTP1", "CoverageDTP3",
                  "VaccinePregnant", "VaccineAdult", "VaccineRisk",
                  'VaccineAP, VaccineWP',
                  "TimeLastShot", "TimeFirstShot",
                  "VaccineAP", "VaccineWP"),
     text = c("DTP1 coverage", "DTP3 coverage", 
              "Maternal immunization", "Vaccine for adult", "Vaccine for risk",
              "aP vaccine, wP vaccine",
              "Time of last shot", "Time of first shot",
              "aP vaccine", "wP vaccine")
)

plot_rf <- function(i){
     # Filter data for the specific cluster
     cl <- levels(DataAll$ClusterG)[i]
     print(paste0('########## Cluster ', cl, ' ##########'))
     Data <- DataAll |> 
          filter(ClusterG %in% cl)  |>
          mutate(VaccineAP = as.numeric(VaccineAP == 'TRUE'),
                 VaccineWP = as.numeric(VaccineWP == 'TRUE'))  |>
          mutate_at(vars(VaccinePregnant, VaccineAdult, VaccineRisk, VaccineAP, VaccineWP), as.character) |>
          filter(!is.na(OutbreakSize) & !is.infinite(OutbreakSize))  |>
          select(-c(VaccineCode)) |> 
          na.omit()  |>
          select(CoverageDTP1, CoverageDTP3,
                 TimeLastShot, TimeFirstShot,
                 VaccinePregnant, VaccineAdult,
                 VaccineRisk, VaccineAP, VaccineWP,
                 OutbreakSize) |> 
          # scale the data
          mutate(across(CoverageDTP1:TimeFirstShot, scale),
                 OutbreakSize = OutbreakSize == 3)
     
     # print the sample size
     sample_size <- nrow(Data)
     print(paste0("Sample size: ", sample_size))
     
     # Bootstrap resampling (if the sample size is too small)
     if (sample_size < 100) {
          print("Bootstrap resampling...")
          Data <- Data[sample(1:nrow(Data), size = 100, replace = TRUE), ]
     }
     
     # SMOOTE oversampling (if the sample size is too small)
     # if (length(unique(Data$OutbreakSize)) > 1) {
     #      print("SMOTE oversampling...")
     #      Data <- SMOTE(OutbreakSize ~ ., data = Data, perc.over = 200, perc.under = 100)
     # }
     
     # Prepare the model matrix
     y <- Data$OutbreakSize
     x <- Data |> select(-OutbreakSize)
     
     # Fit random forest model
     set.seed(20250218)
     rf_model <- randomForest(x, y, importance = TRUE, ntree = 100000)
     print(rf_model)
     
     # Obtain and order variable importance
     importance_data <- importance(rf_model, scale = TRUE, type = 1)
     importance_df <- data.frame(Variable = rownames(importance_data), Importance = importance_data[,1]) |> 
          arrange(desc(Importance)) |> 
          left_join(DataLabel, by = 'Variable') |> 
          mutate(cluster = cl)
     
     # Plotting the importance of each variable
     fig1 <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = '#0F7BA2FF') +
          geom_hline(yintercept = 0, color = 'black') +
          scale_x_discrete(labels = importance_df$text,
                           breaks = importance_df$Variable) +
          coord_flip()+
          theme_bw() +
          theme(panel.grid = element_blank(),
                axis.text = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                plot.title.position = 'plot') +
          labs(title = LETTERS[i+2], 
               x = NULL, y = "Mean decrease in accuracy")
     
     # # Plotting the partial dependence plot
     # pd_df <- partial_dependence(fit = rf_model,
     #                             vars = importance_df$Variable,
     #                             data = Data,
     #                             n = c(10, 1)) |> 
     #      mutate_all(as.numeric) |>
     #      pivot_longer(cols = -prediction,
     #                   names_to = 'centile',
     #                   values_to = 'value') |> 
     #      drop_na() |> 
     #      left_join(DataLabel, by = c(centile = 'Variable')) |> 
     #      mutate(cluster = cl)
     # 
     # # Creat the plot
     # fig2 <- ggplot(pd_df, aes(x = value, y = prediction)) +
     #      geom_line(color = fill_color[3])+
     #      facet_wrap(~text, scale = "free_x") +
     #      scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
     #      theme_bw()+
     #      theme(panel.grid = element_blank(),
     #            axis.text = element_text(color = 'black', face = 'plain'),
     #            axis.title = element_text(color = 'black', face = 'plain'),
     #            plot.title.position = 'plot') +
     #      labs(y = "Proportion",
     #           x = NULL)
     # 
     # ggsave(paste0('./Outcome/S fig4_', i[1]+1, '.png'),
     #        fig2,
     #        width = 8,
     #        height = 6,
     #        dpi = 300)
     # 
     # write.csv(pd_df, paste0('./Outcome/S table4_', i[1], '.csv'), row.names = F)
     
     return(fig1)
}

fig_4 <- plot_rf(1)
fig_5 <- plot_rf(2)
fig_6 <- plot_rf(3)

# combine figures ----------------------------------------------------------

fig1 <- fig_1 + fig_2 + plot_layout(widths = c(1, 2.5))

fig2 <- fig_4 + fig_5 + fig_6 + plot_layout(widths = c(1, 1, 1))

fig <- plot_grid(fig1, fig2, ncol = 1, rel_heights = c(1, 1))

ggsave("./Outcome/fig5.pdf",
       fig,
       width = 11.5,
       height = 7.5,
       device = cairo_pdf,
       family = "Helvetica")

# data --------------------------------------------------------------------

data <- rbind(fig_4$data, fig_5$data, fig_6$data)

write.csv(data, "./Outcome/fig data/fig5.csv", row.names = F)

# data <- map(1:3, ~read.csv(paste0('./Outcome/S table4_', .x, '.csv')))
# data <- bind_rows(data, .id = 'Cluster')
# write.csv(data, "./Outcome/S table5.csv", row.names = F)
# # remove the temporary files
# file.remove(paste0('./Outcome/S table4_', 1:3, '.csv'))
