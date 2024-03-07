## HEADER ####
## who: Susan Colahan
## what: Visual essay part 6
## last edited: 2024-03-03

## CONTENT ####
## 00 setup
## 01 Data wrangling 
## 02 Plotting

## 00 setup ####

# libraries
library(tidyverse)
library(gganimate)
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(plotly)
library(wesanderson)

# working directory
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forest <- tuesdata$forest

## 01 Data wrangling ####
# filter data to only contain the most recent data:
summary_loss_for_2015 <- forest %>% 
  filter(year %in% "2015") %>% 
  arrange(net_forest_conversion) %>% 
  select(-year, -code)

## 02 Graphing ####

# Choose a Wes Anderson color palette
color_palette <- wes_palette("Zissou1Continuous", type = "continuous")

# Create a jittered x-axis position for each entity
summary_loss_for_2015$jittered_x <- jitter(as.numeric(as.factor(summary_loss_for_2015$entity)))

# Create the interactive beeswarm plot using plotly
interactive_beeswarm <- plot_ly(summary_loss_for_2015, 
                                x = ~jittered_x, 
                                y = ~net_forest_conversion, 
                                type = 'scatter', 
                                mode = 'markers', 
                                marker = list(color = color_palette, size = 10),
                                hoverinfo = 'text',
                                text = ~paste("Country/Area:", entity, "<br>Net Forest Conversion:", net_forest_conversion)) %>%
  layout(title = 'Change in Forest Area (2015)',
         xaxis = list(title = '', showticklabels = FALSE), # Remove x-axis label and tick labels
         yaxis = list(title = 'Net Forest Conversion in hectares'),
         hovermode = 'closest')

# Render the plot in your R environment
interactive_beeswarm


