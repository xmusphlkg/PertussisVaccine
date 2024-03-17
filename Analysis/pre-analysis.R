
library(tidyverse)

df <- read.csv("../CleanData/pertussis.csv")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$Cases <- as.integer(df$Cases)

ggplot(df)+
     geom_line(mapping = aes(x = Date, y = Cases, color = Country, group = Country))


ggsave("../preview.png", width = 10, height = 5, dpi = 300)
