
library(tidyverse)

df <- read.csv("../CleanData/report_pertussis.csv")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$Cases <- as.integer(df$Cases)

ggplot(df)+
     geom_line(mapping = aes(x = Date, y = Cases, color = Country, group = Country))+
     facet_wrap(~Country, scales = "free_y")


ggsave("../preview.png", width = 10, height = 5, dpi = 300)

# source("./pre-analysis.R")