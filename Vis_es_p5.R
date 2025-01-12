## HEADER ####
## who: Susan Colahan
## what: Visual essay part 5
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
library(wesanderson)


# working directory
# adapt to github repository if needed:
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

brazil_loss <- tuesdata$brazil_loss

## 01 Data wrangling ####

data_2013 <- brazil_loss %>% 
  filter(year == 2013) %>% 
  gather(key = categories, value = "loss") %>% 
  filter(!categories %in% c("entity", "code", "year", "flooding_due_to_dams")) %>% 
  mutate(longitude = 1:10) %>% 
  mutate(latitude = 1:10) 

write.csv(data_2013, "data_2013.csv")

data_2013_adapted <- read.csv("adapted_data_2013.csv")

# Convert the 'categories' column in 'data_2013' to a factor and loss to a number
  data_2013$categories <- as.factor(data_2013$categories)
  data_2013$loss <- as.numeric(data_2013$loss)

## 02 Plotting ####

 loss_plot <-  ggplot(data_2013_adapted, aes(x = longitude, y = latitude, size = loss, color = categories,
                                             text = paste("Loss:", loss, "<br>Category:", categories))) +
    geom_point(alpha = 0.6) + # Use alpha to adjust the transparency of the points
    scale_size_continuous(range = c(3, 12)) + # Adjust the size range for the points
    theme_minimal() + # Use a minimal theme
    labs(title = "Loss by Category",
         x = "Longitude",
         y = "Latitude",
         size = "Loss",
         color = "Category") +
    scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_2013$categories)), type = "continuous")) + # Apply Wes Anderson color scheme
    labs(x = NULL, y = NULL, title = "Forest loss in Brazil for different categories", legend = NULL, subtitle = "measured in 2014 in thousand hectares") +
    theme(
      plot.background = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
      plot.subtitle = element_text(hjust = 0.5, face = "italic",size = 10), # Center and italicise the subtitle)
      legend.position = "none", 
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
    
vis_es_p5 <- ggplotly(loss_plot, tooltip = "text")

vis_es_p5
  
static_loss_plot <-  ggplot(data_2013_adapted, aes(x = longitude, y = latitude, size = loss, color = categories,
                                                    text = paste("Loss:", loss, "<br>Category:", categories))) +
  geom_point(alpha = 0.6) + # Use alpha to adjust the transparency of the points
  scale_size_continuous(range = c(3, 12)) + # Adjust the size range for the points
  theme_minimal() + # Use a minimal theme
  labs(title = "Loss by Category",
       x = "Longitude",
       y = "Latitude",
       size = "Loss",
       color = "Category") +
  scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_2013$categories)), type = "continuous")) + # Apply Wes Anderson color scheme
  labs(x = NULL, y = NULL, title = "Forest loss in Brazil for different categories", legend = NULL, subtitle = "measured in 2014 in thousand hectares") +
  theme(
    plot.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
    plot.subtitle = element_text(hjust = 0.5, face = "italic",size = 10), # Center and italicise the subtitle)
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )
