
# fig1 --------------------------------------------------------------------

# Function to convert values in Y/M/W format to months
convert_to_months <- function(value) {
     num <- as.numeric(str_extract(value, "\\d+"))  # Extract the numeric part
     unit <- str_extract(value, "[YMWМ]")  # Extract the unit (Y, M, W, Cyrillic M)
     
     # Convert based on the unit to weeks
     switch(unit,
            "Y" = num * 12,    # Years to months: 1 year = 12 months
            "M" = num ,
            "М" = num ,
            "W" = num/4.35)  # Weeks to months: 1 month = 4.35 weeks
}

# Updated function to process entire vectors
process_date_column <- function(date_vector, type) {
     unlist(lapply(date_vector, function(date_str) {
          if (str_detect(date_str, "-")) {
               # Handle a range of values
               parts <- str_split(date_str, "-", simplify = TRUE)
               weeks1 <- convert_to_months(parts[1])
               weeks2 <- convert_to_months(parts[2])
               # Calculate the value of the range
               if (type == "min") {
                    return(min(weeks1, weeks2))
               } else if (type == "max") {
                    return(max(weeks1, weeks2))
               } else {
                    return((weeks1 + weeks2) / 2)
               }
          } else {
               # Handle a single value
               convert_to_months(date_str)
          }
     }))
}

resolve_ageadministered <- function(ageadministered) {
     age_points <- unlist(strsplit(ageadministered, "-"))
     return(unique(age_points))
}

# generate minor figure in map

plot_map_density <- function(x, fill_color) {
     # remove NA
     x <- x[!is.na(x)]
     y <- density(x, n = 2^12)
     data <- data.frame(x = y$x, y = y$y)
     
     ggplot(data = data, aes(x, y)) +
          geom_line() + 
          geom_segment(aes(xend = x, yend = 0, colour = x))+
          geom_hline(yintercept = 0, linetype = 'dashed')+
          scale_x_continuous(limits = c(0.3, 1),
                             labels = scales::percent_format(accuracy = 1),
                             expand = c(0, 0))+
          scale_y_continuous(limits = c(0, NA),
                             expand = expansion(mult = c(0, 0.2)))+
          scale_color_gradientn(colors = fill_color,
                                limits = c(0.3, 1),
                                breaks = c(0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 1))+
          theme_minimal()+
          theme(panel.grid = element_blank(),
                axis.text.x = element_text(color = 'black', face = 'plain'),
                axis.title = element_blank(),
                axis.text.y = element_blank(),
                panel.border = element_rect(color = 'black', fill = NA),
                plot.title = element_text(size = 11),
                plot.title.position = 'plot',
                legend.position = 'none')+
          labs(colour = NULL)+
          guides(colour = guide_colorbar(barwidth = 1))
}

plot_map_col <- function(x, fill_color) {
     data <- data.frame(x = x) |> 
          group_by(x) |>
          summarise(y = n(),
                    .groups = 'drop') |> 
          drop_na()
     # add level is missing
     if (!all(unique(data$x) %in% levels(x))) {
          data <- data |> 
               rbind(data.frame(x = setdiff(levels(x), data$x), y = 0))
     }
     
     ggplot(data = data, aes(x, y, fill = x)) +
          geom_col()+
          geom_text(aes(label = y), vjust = -0.1, size = 3)+
          scale_y_continuous(limits = c(0, NA),
                             expand = expansion(mult = c(0, 0.2)))+
          scale_x_discrete(limits = levels(x))+
          theme_minimal()+
          theme(panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_text(color = 'black', face = 'plain'),
                axis.text.y = element_blank(),
                panel.border = element_rect(color = 'black', fill = NA),
                plot.title.position = 'plot',
                plot.title = element_text(size = 11),
                legend.position = 'none')+
          labs(colour = NULL)
}
