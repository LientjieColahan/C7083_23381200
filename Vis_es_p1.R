## HEADER ####
## who: Susan Colahan
## what: Visual essay part 1
## last edited: 2024-03-03

## CONTENT ####
## 00 setup
## 01 Data wrangling 
## 02 Plotting

## 00 setup ####

# libraries
library(tidyverse)
library(gganimate)
library(plotly)
library(gganimate)


# working directory
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

original_veg_oil <- tuesdata$vegetable_oil

write.csv(original_veg_oil, "original_veg_oil.csv") # write a csv of the dataset for reproducibility 

## 01 Data wrangling ####

# create dataset with only the world data
veg_oil_world <- original_veg_oil %>% 
  filter(code == "OWID_WRL")

# 
tot_veg_oil_world <-veg_oil_world %>% 
  group_by(year) %>% 
  summarise(all_oil_prod = sum(production)) %>% 
  mutate(mil_ton_prod = round(all_oil_prod/1000000, digits = 2))




## 02 Plotting ####
# Plotting the line graph with dots
plot(tot_veg_oil_world$year, tot_veg_oil_world$mil_ton_prod, 
     type = "b", # Both points and lines
     col = ifelse(tot_veg_oil_world$year == 2006, "#f24f26", "#0097c3"), # Change color conditionally
     pch = 19, # Point type (solid circle)
     lwd = 2, # Line width
     xlab = "Year", # X-axis label
     ylab = "Production (Million Tonnes)",cex = 1.5)# Y-axis label
     
    

# Customize axes and titles
title(main = "Vegetable Oil Production Over Time", 
      col.main = "black", # Title color
      font.main = 4, # Bold and italic
      cex.main = 1.5) # Title size

# Add an annotation
# Original annotation text
annotation_text <- "In 2006 the increase in annual production broke the 10 million tonne milestone for the first time!"

# Use strwrap to automatically wrap the text
wrapped_text <- strwrap(annotation_text, width = 36) # Adjust width as needed

# Combine the wrapped text into a single string with line breaks
formatted_text <- paste(wrapped_text, collapse = "\n")
text(x = 1995, y = 130, # Coordinates for the annotation
     labels = formatted_text, 
     col = "#f24f26", # Text color
     cex = 1) # Text size

# Re-plot the line to ensure it is not broken by the conditional point coloring
lines(tot_veg_oil_world$year, tot_veg_oil_world$mil_ton_prod, col = "#0097c3", lwd = 2)

# Store the plot in an object called vis_1
vis_1 <- recordPlot()

# Export the plot as an .svg file to maintain quality
# Open the SVG device with specified dimensions
svg("vis_1_plot.svg", width = 8.27, height = 5.83)

# Replay the recorded plot
replayPlot(vis_1)

# Close the SVG device
dev.off()
