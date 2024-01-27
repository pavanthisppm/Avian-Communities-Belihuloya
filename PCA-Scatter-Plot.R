library(dplyr)
data = PCA_New_abr
sum(is.na(data))
colnames(data)

colnames(data)
data_subset = data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )]

add_var = data.frame( Species = data$Species, Habitat_type = data$Habitat_type)


data = cbind( data_subset,   add_var)
colnames(data)

dim(data)

calcmode <- function(a) {
  vector <- unique(a)
  vector[which.max(tabulate(match(a, vector)))]
}


result <- data %>%
  group_by(Species) %>%
  summarise(
    Habitat_type =  calcmode(Habitat_type),
    Transect_Elevation = mean(Transect_Elevation),
    Average_Humidity = mean(Average_Humidity),
    Average_Light_intensity = mean(Average_Light_intensity),
    Average_Temperature = mean(Average_Temperature),
    Average_Wind_speed = mean(Average_Wind_speed)
  )

result$Habitat_type = as.numeric(result$Habitat_type)*2
result$Transect_Elevation = result$Transect_Elevation*3


data = result
colnames(data)
dim(data)
head(data)


# pca_result <- prcomp(data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )] , scale. = TRUE)

pca_scores <- as.data.frame(pca_result$x[, 1:3])
# png("PCA-Scatter-Plot.png", height = 1000, width = 1400)
# 
# plot(jitter(pca_scores$PC1), jitter(pca_scores$PC2), main = "PCA Plot", xlab = "PCA1", ylab = "PCA2", pch = 16, col = "black", cex = 1.5)
# 
# text(pca_scores$PC1, pca_scores$PC2, labels = data$Species, pos = 3)
# 
# dev.off()



# Load necessary packages
library(ggplot2)
library(ggrepel)
png("PCA-Scatter-Plot.png", height = 700, width = 900)
# Create the ggplot with labeled points (using ggrepel for label repelling)
ggplot(data = pca_scores, aes(x = PC1, y = PC2, label = data$Species)) +
  geom_point(size = 4, color = "black") +
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = 50) +  # Use geom_text_repel for label repelling
  labs(title = "", x = "PC1", y = "PC2") +
  theme_minimal()
dev.off()

plot3d(pca_scores[,1], pca_scores[,2], pca_scores[,3], type="s", size=1, lit=TRUE, main = "")

# Add data labels using text3d
text3d(pc_scores[, 1], pc_scores[, 2], pc_scores[, 3], texts = rownames(pc_scores), cex = 0.7, adj = c(-0.5,0), col="black")

