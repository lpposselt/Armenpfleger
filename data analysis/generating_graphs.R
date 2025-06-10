# Load necessary libraries
library(ggplot2)

#install.packages("paletteer")
library(paletteer)
library(tidyverse)


theme_set(theme_minimal())
options(ggplot2.discrete.colour= paletteer_d("ggsci::default_nejm"))
options(ggplot2.discrete.fill= paletteer_d("ggsci::default_nejm"))
figure_width <-  10.5
figure_height <- 6
dpi <- 300


# Read the CSV file
topics_over_time <- read.csv('/Users/luki/Desktop/AP_ALL/code/topics_over_time.csv')

head(topics_over_time)

# Assuming 'topics_over_time' DataFrame is already loaded

# Filter the data for specific topics
filtered_data <- topics_over_time[topics_over_time$Topic %in% c(2, 4, 5, 22), ]

# Create the plot
p <- ggplot(data = filtered_data, 
            aes(x = Timestamp, y = Frequency, color = Topic.Label)) +
  geom_line() +
  labs(title = 'Development of selected topics over time',
       x = 'Time',
       y = 'Frequency',
       color = 'Topic') +  # Correctly set the legend title
  theme_minimal() +
  theme(legend.title = element_text(size = 12))  # Additional styling if needed

# Save the plot with specified dimensions
ggsave("/Users/luki/Desktop/AP_ALL/figures/topics_over_time_plot.pdf", plot = p, 
       width = figure_width,
       height = figure_height, 
       dpi = dpi)


