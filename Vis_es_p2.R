## HEADER ####
## who: Susan Colahan
## what: Visual essay part 2
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
library(cowplot)
library(wesanderson)


# working directory
setwd("C:/Users/colah/OneDrive/Desktop/MSc Data science/Data_Visualisation_and_Analytics_C7083/Assignment/Visual essay")


# read data
# tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forest_area <- tuesdata$forest_area


## 01 Data wrangling ####

# Filter out the percentage of global forest area for the 3 countries where deforestation is most severe
forest_area_summary <- forest_area %>% 
  filter(entity %in% c("Brazil", "Democratic Republic of Congo", "Indonesia"))  %>% 
  filter((year %in% c(1990, 2000, 2010, 2020)))

# create a subset of data for each country
brazil_area <- forest_area_summary %>% 
  filter(entity == "Brazil") %>% 
  mutate(forest_area = round(forest_area,2))

# Convert the 'year' column to a factor
brazil_area$year <- factor(brazil_area$year)

congo_area <- forest_area_summary %>% 
  filter(entity == "Democratic Republic of Congo") %>% 
  mutate(forest_area = round(forest_area,2))

# Convert the 'year' column to a factor
congo_area$year <- factor(congo_area$year)

indonesia_area <- forest_area_summary %>% 
  filter(entity == "Indonesia") %>% 
  mutate(forest_area = round(forest_area,2))

# Convert the 'year' column to a factor
indonesia_area$year <- factor(indonesia_area$year)

## 02 Plotting ####

# barplots using ggplot:

brazil_plot <- ggplot(brazil_area, aes(year, forest_area, fill = year, label = forest_area)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 2, size = 5, colour = "white") +
  scale_fill_manual(values = wes_palette("Zissou1Continuous", n = 4, type = "discrete")) +
  labs(x = "Year", y = NULL, title = "Brazil", subtitle = "Decline in the percentage of the world's 
       forest found in Brazil") +
  theme(legend.position = "none", 
        axis.text.x = element_text(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10), # Center and italicise the subtitle
        plot.background = element_blank(),
        panel.grid = element_blank()
  ) 
brazil_plot


congo_plot <- ggplot(congo_area, aes(year, forest_area, fill = year, label = forest_area)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 2, size = 5, colour = "white") +
  scale_fill_manual(values = wes_palette("Zissou1Continuous", n = 4, type = "discrete")) +
  labs(x = "Year", y = NULL, title = "The Democratic Republic of Congo", subtitle = "Decline in the percentage of the world's 
       forest found in the DOC") +
  theme(legend.position = "none", 
        axis.text.x = element_text(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, face = "italic",size = 10), # Center and italicise the subtitle
        plot.background = element_blank(),
        panel.grid = element_blank()
  )
congo_plot

indonesia_plot <- ggplot(indonesia_area, aes(year, forest_area, fill = year, label = forest_area)) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 2, size = 5, colour = "white") +
  scale_fill_manual(values = wes_palette("Zissou1Continuous", n = 4, type = "discrete")) +
  labs(x = "Year", y = NULL, title = "Indonesia", subtitle = "Decline in the percentage of the world's 
       forest found in Indonesia") +
  theme(legend.position = "none", 
        axis.text.x = element_text(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10), # Center italicise the subtitle
        plot.background = element_blank(),
        panel.grid = element_blank()
  )
indonesia_plot

vis_2 <- plot_grid(brazil_plot, congo_plot, indonesia_plot,
          ncol = 3,
          scale = TRUE
          )
vis_2
# Export the plot as an .svg file to maintain quality
# Open the SVG graphics device
svg("vis_2_plot.svg", width = 8.27, height = 5.83)

# Print the plot
print(vis_2)

# Close the SVG graphics device
dev.off()
