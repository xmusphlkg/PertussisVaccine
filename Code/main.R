

# Load the data
source('./Code/data.R')
remove(list = ls())

# figure 1: vaccination strategy
source('./Code/fig1.R')
remove(list = ls())

# figure 2: analysis the age distribution
source('./Code/fig2.R')
remove(list = ls())

# figure 3: resurgence distribution
source('./Code/fig3.R')
remove(list = ls())

# figure 4: the relationship between vaccination strategy and resurgence
source('./Code/fig4.R')
remove(list = ls())

# table 1
knitr::knit('./Code/table1.Rmd',
            output = './Outcome/table1.md')
