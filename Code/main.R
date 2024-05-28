

# Load the data
source('./Code/data.R')
remove(list = ls())

# figure 1
source('./Code/fig1.R')
remove(list = ls())

# figure 2
source('./Code/fig2.R')
remove(list = ls())

# figure 3
source('./Code/fig3.R')
remove(list = ls())

# table 1
knitr::knit('./Code/table1.Rmd',
            output = './Outcome/table1.md')
