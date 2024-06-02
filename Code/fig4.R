
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

source('./Code/function.R')

# Data --------------------------------------------------------------------

DataAll <- read.csv("./Outcome/S table1.csv") |> 
     mutate(CoverageDTP1 = CoverageDTP1/100,
            CoverageDTP3 = CoverageDTP3/100,
            VaccineGeneral = factor(VaccineGeneral,
                                    levels = c(1:6),
                                    labels = c('1 dose', paste(c(2:6), 'doses'))),
            GENERALY = case_when(GENERALY > 1 ~ paste0(GENERALY, ' doses'),
                                 GENERALY == 1 ~ '1 dose',
                                 GENERALY == 0 ~ 'Not provided',
                                 TRUE ~ 'Unavailable'),
            GENERALY = factor(GENERALY,
                              levels = c('Not provided', '1 dose', '2 doses', '3 doses')),
            GENERALM = case_when(GENERALM > 1 ~ paste0(GENERALM, ' doses'),
                                 GENERALM == 1 ~ '1 dose',
                                 GENERALM == 0 ~ 'Not provided',
                                 TRUE ~ 'Unavailable'),
            GENERALM = factor(GENERALM,
                              levels = c('1 dose', '2 doses', '3 doses', '4 doses', '5 doses', '6 doses')),
            VaccineAP = if_else(VaccineCode %in% c('aP', 'Both'), 1, 0),
            VaccineWP = if_else(VaccineCode %in% c('wP', 'Both'), 1, 0),
            OutbreakSize2022 = factor(OutbreakSize2022, levels = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable')),
            OutbreakSize2023 = factor(OutbreakSize2023, levels = c('Low', 'Normal', 'High', 'Resurgence', 'Unavailable')),
            .before = 'VaccineCode'
     ) |> 
     select(-c(VaccineAP, VaccineWP))

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
DataAll[!DataAll$CODE %in% DataMap$iso_a3, 'NAME']

DataInciRaw <- read.xlsx("./Data/Pertussis incidence.xlsx") |> 
     select(1:3, `2023`:`2015`)
DataInciRaw <- DataInciRaw |> 
     filter(!is.na(Disease)) |> 
     select(-c(Denominator, Disease)) |> 
     rename(NAME = `Country./.Region`) |> 
     filter(NAME %in% DataAll$NAME) |>
     # trance 2023:2010 to numeric
     mutate(across('2023':'2015', ~as.numeric(.)),
            across(where(is.numeric), ~replace(., . == 0, NA)))

DataInciRaw |> 
     select(NAME, `2019`:`2015`) |> 
     pivot_longer(cols = `2019`:`2015`, names_to = 'Year', values_to = 'Incidence') |>
     group_by(NAME) |>
     summarise(Incidence = median(Incidence, na.rm = T)) |> 
     arrange(desc(Incidence)) |> 
     head(10) |> 
     as.data.frame()

# incidence cluster -------------------------------------------------------

set.seed(20240521)

fill_color_disease <- rev(c('#DD5129FF', '#FAB255FF', '#0F7BA2FF', '#43B284FF' ))

DataInci <- DataInciRaw |>
     select('2019':'2015') 
names(DataInci) <- paste0("X", names(DataInci))
rownames(DataInci) <- DataInciRaw$NAME

# random forest imputation
DataMatInci <-DataInci|>
     mice(method = 'rf', m = 1) |> 
     complete() |>
     as.matrix() |> 
     log()

# hierarchical clustering
hcdata <- scale(DataMatInci) |>  
     hkmeans(3)
DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     mutate(Cluster = as.factor(hcdata$cluster),
            Cluster = fct_recode(Cluster, 'Low' = '2', 'Mild' = '1', 'High' = '3'),
            Cluster = factor(Cluster, levels = c('Low', 'Mild', 'High'))) |>  
     select(Cluster) |> 
     rownames_to_column('NAME')
DataAll <- DataAll |> 
     left_join(DataCluster, by = 'NAME')
DataCluster <- cbind(DataCluster, exp(DataMatInci)) |> 
     pivot_longer(cols = -c(NAME, Cluster), names_to = 'Year', values_to = 'IncidenceImput') |> 
     mutate(Year = as.numeric(gsub('X', '', Year)))

table(DataCluster$Cluster)/5

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

fill_color <- c('#DD5129FF', '#FAB255FF')

scientific_10 <- function(x) {
     x_f <- gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x)))
     x_f[x_f == "10^00"] <- "1"
     # replace 10^0 with 1
     x_f <- parse(text = x_f)
     return(x_f)
}

fig_1 <- ggplot(data = DataCluster) +
     geom_violin(aes(x = Cluster, y = Incidence,
                     fill = Type,
                     group = interaction(Cluster, Type)),
                 alpha = 0.8)+
     scale_y_continuous(trans = 'log10',
                        labels = scientific_10)+
     scale_x_discrete(breaks = levels(DataAll$Cluster),
                      limits = levels(DataAll$Cluster),
                      labels = c('Low burden', 'Mild burden', 'High burden'))+
     scale_fill_manual(values = fill_color)+
     theme_bw() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.background = element_blank(),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           plot.title.position = 'plot') +
     labs(title = "A", x = NULL, y = "Incidence (per million population)")

# panel c -----------------------------------------------------------------

fill_color <- c('#0F7BA2FF', '#43B284FF', '#DD5129FF')

# visualize cluster

fig_3 <- fviz_cluster(hcdata,
                      data = scale(DataMatInci),
                      main = 'C',
                      repel = TRUE,
                      ggtheme = theme_bw(),
                      palette = fill_color,
                      geom = 'point',
                      show.clust.cent = F) +
     theme(legend.position = "none",
           panel.grid = element_blank(),
           plot.title.position = 'plot')


# panel b -----------------------------------------------------------------

fill_color <- c('#43B284FF', '#0F7BA2FF', '#DD5129FF')

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
                  "TimeLastShot", "TimeFirstShot",
                  "GENERALM", "GENERALY", "VaccineGeneral", "VaccineAP", "VaccineWP"),
     text = c("DTP1 coverage", "DTP3 coverage", 
              "Vaccine for pregnant", "Vaccine for adult", "Vaccine for risk",
              "Time of last shot", "Time of first shot",
              "Vaccine dose (<3 years) ", "Vaccine dose (3 years+)",
              "General vaccine schedule",
              "aP vaccine", "wP vaccine")
)

plot_rf <- function(i){
     # Filter data for the specific cluster
     cl <- unique(DataAll$Cluster)[i]
     Data <- DataAll |> 
          filter(Cluster %in% cl)  |>
          select(CoverageDTP1:VaccineCode, OutbreakSize)  |>
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
     rf_model <- randomForest(x, y, importance = TRUE, ntree = 50)
     print(rf_model)
     
     # Obtain and order variable importance
     importance_data <- importance(rf_model, scale = FALSE, type = 1)
     importance_df <- data.frame(Variable = rownames(importance_data), Importance = importance_data[,1]) |> 
          arrange(desc(Importance)) |> 
          left_join(DataLabel, by = 'Variable')
     print(importance_df)
     
     # Plotting the importance of each variable
     fig1 <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = '#3A507FFF') +
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
               x = NULL, y = "Mean Decrease in Accuracy")
     
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
     
     ggsave(paste0('./Outcome/S fig', i[1]+1, '.png'),
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

fig2 <- fig_3 + fig_4 + fig_5 + fig_6 + plot_layout(widths = c(2, 1, 1, 1))

fig <- plot_grid(fig1, fig2, ncol = 1, rel_heights = c(1, 1))

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 11.5,
       height = 7.5,
       device = cairo_pdf,
       family = "Helvetica")
