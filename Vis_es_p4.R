## HEADER ####
## who: Susan Colahan
## what: Visual essay part 4
## last edited: 2024-03-03

## CONTENT ####
## 00 setup
## 01 Data wrangling 
## 02 Plotting

## 00 setup ####

# libraries
library(tidyverse)
library(ggplot2)
library(gganimate)


# working directory
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# oilcrop productivity data
crop_eff <- read.csv("oil_productivity.csv")

## 01 Data wrangling ####
# none required



## 02 Graphing ####
eff_p <- ggplot(crop_eff, aes(x = efficiency, y = crop_oil)) +
  geom_col(aes(x = thoery), fill = "#78B7C5") +
  geom_col(width = 0.5, fill = "#E1AF00") +
  labs(x = "% Land use efficiency", y = NULL, 
       title = "Land use efficiency - specifically production per hectare") +
  theme(
    plot.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
    plot.subtitle = element_text(hjust = 0.5, face = "italic",size = 10), # Center and italicise the subtitle)
  )

vis_4_plot <- eff_p +
  annotate("text", x = 75, y = "Palm", label = "4.1 tonne per hectare", colour = "white") +
  annotate("text", x = 75, y = "Rapeseed", label = "1.4 tonne per hectare", colour = "white") +
  annotate("text", x = 75, y = "Soy", label = "0.375 tonne per hectare", colour = "white")

# Export the plot as an .svg file to maintain quality
# Open the SVG graphics device
svg("vis_4_plot.svg", width = 8.27, height = 5.83)

# Print the plot
print(vis_4_plot)

# Close the SVG graphics device
dev.off()
