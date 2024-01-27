# install.packages("gridExtra")
# Load required libraries
library(ggplot2)
library(gridExtra)

# Dataset
data = Basic_graphs_analysis_forest_elevations
# data <- data.frame(
#   E = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"),
#   Species_richness = c(41, 40, 47, 36, 28, 37, 29, 36),
#   Abundance = c(11.7, 12, 8.55, 4.72, 4.19, 7.65, 5.18, 7.87),
#   Diversity = c(3.33208, 3.23762, 3.45185, 3.35747, 2.91287, 3.21401, 3.06604, 2.94402),
#   Evenness = c(0.897271, 0.87767, 0.89655, 0.93692, 0.874157, 0.890081, 0.910535, 0.821544)
# )

# Set up a 1x4 layout for four plots in one row
par(mfrow = c(1, 4), mar = c(4, 4, 2, 1))
n=nrow(data)

# Plot Species Richness
p1 <- ggplot(data, aes(x = 1:n, y = Species_richness)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Species Richness", x = "Elevation", y = "Community parameters values") +
  scale_x_continuous(breaks = 1:n, labels = data$Elevation) +
  geom_smooth(lty = 2, col = "black", se = FALSE)
   

# Plot Abundance
p2 <- ggplot(data, aes(x = 1:n, y = Abundance)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Abundance", x = "Elevation", y = "Community parameters values")+
  scale_x_continuous(breaks = 1:n, labels = data$Elevation) +
  geom_smooth(lty = 2, col = "black", se = FALSE)

# Plot Diversity
p3 <- ggplot(data, aes(x = 1:n, y = Diversity)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Diversity", x = "Elevation", y = "Community parameters values")+
  scale_x_continuous(breaks = 1:n, labels = data$Elevation) +
  geom_smooth(lty = 2, col = "black", se = FALSE)

# Plot Evenness
p4 <- ggplot(data, aes(x = 1:n, y = Evenness)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Evenness", x = "Elevation", y = "Community parameters values")+
  scale_x_continuous(breaks = 1:n, labels = data$Elevation) +
  geom_smooth(lty = 2, col = "black", se = FALSE)

# Combine all plots into a single row
grid.arrange(p1, p2, p3, p4, nrow = 1)

# Save the combined plot to a PNG file
ggsave("combined_plots.png", height = 5, width = 30)
