## HEADER ####
## who: Susan Colahan
## what: vis 3
## last edited: 2024-03-04

## CONTENT ####
## 00 setup
## 01 Data wrangling
## 02 Graphs
## 03

## 00 setup ####
# libraries
library(tidyverse)
library(gganimate)
library(plotly)
library(cowplot)


# working directory
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forest <- tuesdata$forest

## 01 Data wrangling ####
glimpse(forest)

# show total forest lost sine 1990 - 2015

forest_summary <- forest %>%
  group_by(entity) %>%
  summarise(total_forest_conversion = sum(net_forest_conversion, na.rm = TRUE)) %>% 
  filter(total_forest_conversion != 0) %>% 
  arrange(total_forest_conversion)

forest_lost <- forest_summary %>% 
  filter(total_forest_conversion < 0)
 
top5_world_loss <- forest_summary %>% 
  filter(total_forest_conversion < 0) %>% 
  filter(entity %in% c("World", "Brazil", "Indonesia","Tanzania","Myanmar","Paraguay" ))

other_world_loss <- forest_lost %>% 
  filter(entity != c("World", "Brazil", "Indonesia","Tanzania","Myanmar","Paraguay" )) %>% 
  summarise(total_forest_conversion = sum(total_forest_conversion)) %>% 
  mutate(entity = "Other", .before = 1)

summary_loss <- bind_rows(top5_world_loss, other_world_loss)


## 02 Graphing ####

# For example, to create a new column for the absolute values of total_forest_conversion
summary_loss <- summary_loss %>%
  mutate(bubble_size = abs(total_forest_conversion))

# Create the bubble plot
fig <- plot_ly(summary_loss, x = ~entity, y = ~total_forest_conversion,
               text = ~entity, size = ~bubble_size, type = 'scatter', mode = 'markers',
               marker = list(opacity = 0.5))

# Customize the layout if needed
fig <- fig %>% layout(title = 'Forest Conversion by Country',
                      xaxis = list(title = 'Country'),
                      yaxis = list(title = 'Total Forest Conversion'),
                      showlegend = FALSE)

# Render the plot
fig
         
            