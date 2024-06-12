
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
library(ggdendroplot)
library(caret)
library(mice)
library(randomForest)
library(edarf)
library(GGally)
library(scales)

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table1.csv") |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineGeneral = as.factor(VaccineGeneral),
            GENERALY = as.factor(GENERALY),
            GENERALM = as.factor(GENERALM),
            VaccineAdult = if_else(VaccineAdult == 1, 'Yes', 'No'),
            VaccinePregnant = if_else(VaccinePregnant == 1, 'Yes', 'No'),
            VaccineRisk = if_else(VaccineRisk == 1, 'Yes', 'No'),
            VaccineAP = if_else(VaccineCode %in% c('aP', 'Both'), 1, 0),
            VaccineWP = if_else(VaccineCode %in% c('wP', 'Both'), 1, 0),
            OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Low', 'Normal', 'High', 'Resurgence')),
            OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Low', 'Normal', 'High', 'Resurgence')))

ggsave("./Outcome/S fig2_1.png",
       DataAll |>
            select(CoverageDTP1:VaccineGeneral, -TimeFirstShot) |>
            ggpairs(columnLabels = c('DTP1 coverage', 'DTP3 coverage',
                                     'Vaccine for pregnant', 'Vaccine for adult', 'Vaccine for risk',
                                     'Time of\nlast general vaccine shot (months)',
                                     'General vaccine recommendation\nfor toddlers, -3yr',
                                     'General vaccine recommendation\nfor children, 3+yr',
                                     'General vaccine recommendation'),
                    bins = 30,
                    title = 'Correlation plot'),
       width = 15,
       height = 15,
       dpi = 300)

# map data ----------------------------------------------------------------

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataInciRaw <- read.xlsx("./Data/Pertussis incidence.xlsx") |> 
     select(1:3, `2023`:`2010`)
DataInciRaw <- DataInciRaw |> 
     filter(!is.na(Disease)) |> 
     select(-c(Denominator, Disease)) |> 
     rename(NAME = `Country./.Region`) |> 
     filter(NAME %in% DataAll$NAME) |>
     # trance 2023:2010 to numeric
     mutate(across('2023':'2010', ~as.numeric(.)))

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
     select('2019':'2010') 
names(DataInci) <- paste0("X", names(DataInci))
rownames(DataInci) <- DataInciRaw$NAME

# random forest imputation
DataMatInci <-DataInci|>
     mice(method = 'rf', m = 1) |> 
     complete() |>
     as.matrix()
DataMatInci <- log(DataMatInci + 0.001)

# hierarchical clustering
hcdata <- scale(DataMatInci) |>  
     hkmeans(3)
DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     mutate(Cluster = as.factor(hcdata$cluster),
            Cluster = fct_recode(Cluster, 'Low' = '3', 'Mild' = '1', 'High' = '2'),
            Cluster = factor(Cluster, levels = c('Low', 'Mild', 'High'))) |>  
     select(Cluster) |> 
     rownames_to_column('NAME')
DataAll <- DataAll |> 
     left_join(DataCluster, by = 'NAME')
DataCluster <- cbind(DataCluster, exp(DataMatInci)) |> 
     pivot_longer(cols = -c(NAME, Cluster), names_to = 'Year', values_to = 'IncidenceImput') |> 
     mutate(Year = as.numeric(gsub('X', '', Year)))

table(hcdata$cluster)

DataAll |> 
     group_by(Cluster) |>
     summarise(Incidence = mean(IncidencePre, na.rm = T))

write.csv(DataAll, "./Outcome/S table1.csv", row.names = F)

# merge data
DataInci$NAME <- rownames(DataInci)
DataInci <- DataInci |> 
     pivot_longer(cols = -c(NAME), names_to = 'Year', values_to = 'Incidence') |> 
     mutate(Year = as.numeric(gsub('X', '', Year)))
DataCluster <- DataCluster |> 
     left_join(DataInci, by = c('NAME', 'Year')) |> 
     pivot_longer(cols = c(IncidenceImput, Incidence), names_to = 'Type', values_to = 'Incidence') |> 
     mutate(Type = factor(Type, levels = c('Incidence', 'IncidenceImput'),
                          labels = c('Observed', 'Imputation')))

# panel a -----------------------------------------------------------------

fill_color <- c('#43B284FF', '#FAB255FF', '#DD5129FF')

fig_1 <- ggplot(DataAll, aes(x = Cluster, y = IncidencePre + 0.001, fill = Cluster)) +
     geom_boxplot(show.legend = F) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$Cluster),
                       limits = levels(DataAll$Cluster)) +
     scale_x_discrete(limits = levels(DataAll$Cluster)) +
     scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                        limits = c(0.001, NA),
                        breaks = c(0, 0.1, 1, 10, 100, 1000) + 0.001,
                        labels = c(0, 0.1, 1, 10, 100, 1000),
                        trans = 'log10') +
     theme_bw() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           plot.title.position = 'plot') +
     labs(title = "A", x = NULL, y = "Median incidence (2010-2019)")

# panel b -----------------------------------------------------------------

DataMapPlot <- DataMap |> 
     left_join(DataAll[,c('CODE', 'Cluster')], by = c('iso_a3' = 'CODE'))
table(DataAll$Cluster)

fig_2_m <- plot_map_col(DataAll$Cluster, fill_color) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$Cluster),
                       limits = levels(DataAll$Cluster))+
     scale_x_discrete(limits = levels(DataAll$Cluster))

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = Cluster)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = fill_color,
                       breaks = levels(DataAll$Cluster),
                       limits = levels(DataAll$Cluster),
                       na.translate = F)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0.5, 0.05),
           legend.justification = c(0.25, 0),
           legend.background = element_rect(fill = "#C1CDCD"),
           legend.direction = 'horizontal',
           # legend title on right
           legend.title = element_text(hjust = 0.5),
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL, fill = 'Disease burden')+
     guides(fill = guide_legend(nrow = 1))

fig_2 <- fig_2 + inset_element(fig_2_m, left = 0.01, bottom = 0.01, right = 0.25, top = 0.45)

# panel d -----------------------------------------------------------------

DataLabel <- data.frame(
     Variable = c("CoverageDTP1", "CoverageDTP3",
                  "VaccinePregnant", "VaccineAdult", "VaccineRisk",
                  'VaccineAP, VaccineWP',
                  "TimeLastShot", "TimeFirstShot",
                  "GENERALM", "GENERALY", "VaccineGeneral", "VaccineAP", "VaccineWP"),
     text = c("DTP1 coverage", "DTP3 coverage", 
              "Maternal immunization", "Vaccine for adult", "Vaccine for risk",
              "aP vaccine, wP vaccine",
              "Time of last shot", "Time of first shot",
              "Vaccine dose (<3 years) ", "Vaccine dose (3 years+)",
              "General vaccine schedule",
              "aP vaccine", "wP vaccine")
)

plot_rf <- function(i){
     # Filter data for the specific cluster
     cl <- levels(DataAll$Cluster)[i]
     print(paste0('########## Cluster ', cl, ' ##########'))
     Data <- DataAll |> 
          filter(Cluster %in% cl)  |>
          select(CoverageDTP1:VaccineCode, VaccineAP, VaccineWP, OutbreakSize)  |>
          mutate(
               VaccineGeneral = as.numeric(str_extract(as.character(VaccineGeneral), '\\d+')),
               TimeLastShot = log10(TimeLastShot),
               GENERALM = as.numeric(str_extract(as.character(GENERALM), '\\d+')),
               GENERALY = as.numeric(str_extract(as.character(GENERALY), '\\d+')),
               GENERALY = case_when(is.na(GENERALY) ~ 0,
                                    TRUE ~ GENERALY),
               VaccinePregnant = factor(VaccinePregnant),
               VaccineAdult = factor(VaccineAdult),
               VaccineRisk = factor(VaccineRisk),
               VaccineGeneral = factor(VaccineGeneral),
               GENERALM = factor(GENERALM),
               GENERALY = factor(GENERALY)
          )  |>
          filter(!is.na(OutbreakSize) & !is.infinite(OutbreakSize))  |>
          select(-c(VaccineCode, VaccineGeneral, TimeFirstShot)) |> 
          na.omit()
     
     # Prepare the model matrix
     y <- Data$OutbreakSize
     x <- Data |> select(-OutbreakSize)
     
     # Fit random forest model
     rf_model <- randomForest(x, y, importance = TRUE, ntree = 500)
     print(rf_model)
     
     # Obtain and order variable importance
     importance_data <- importance(rf_model, scale = FALSE, type = 1)
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
          labs(title = LETTERS[i+3], 
               x = NULL, y = "Mean decrease in accuracy")
     
     # Plotting the partial dependence plot
     pd_df <- partial_dependence(fit = rf_model,
                                 vars = importance_df$Variable,
                                 data = Data,
                                 n = c(10, 1)) |> 
          mutate_all(as.numeric) |>
          pivot_longer(cols = -prediction,
                       names_to = 'centile',
                       values_to = 'value') |> 
          drop_na() |> 
          left_join(DataLabel, by = c(centile = 'Variable'))
     
     # Creat the plot
     fig2 <- ggplot(pd_df, aes(x = value, y = prediction)) +
          geom_line(color = fill_color[3])+
          facet_wrap(~text, scale = "free_x") +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
          theme_bw()+
          theme(panel.grid = element_blank(),
                axis.text = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                plot.title.position = 'plot') +
          labs(y = "Proportion",
               x = NULL)
     
     ggsave(paste0('./Outcome/S fig2_', i[1]+1, '.png'),
            fig2,
            width = 8,
            height = 6,
            dpi = 300)
     
     return(fig1)
}

fig_4 <- plot_rf(1)
fig_5 <- plot_rf(2)
fig_6 <- plot_rf(3)

# combine figures ----------------------------------------------------------

fig1 <- fig_1 + fig_2 + plot_layout(widths = c(1, 2.5))

fig2 <- fig_4 + fig_5 + fig_6 + plot_layout(widths = c(1, 1, 1))

fig <- plot_grid(fig1, fig2, ncol = 1, rel_heights = c(1, 1))

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 11.5,
       height = 7.5,
       device = cairo_pdf,
       family = "Helvetica")

# data --------------------------------------------------------------------

data <- rbind(fig_4$data, fig_5$data, fig_6$data)

write.csv(data, "./Outcome/fig data/fig4.csv", row.names = F)
