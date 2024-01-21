library(dplyr)
data = PCA_New
colnames(data)
data_subset = data[,c("Transect_Elevation", "Average_Humidity", "Average_Light_intensity", "Average_Temperature", "Average_Wind_speed" )] 

data_standardized =  scale(data_subset)

add_var = data.frame( Species = data$Species, Habitat_type = data$Habitat_type)


data = cbind( data_standardized,   add_var)
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

# result$Habitat_type = as.numeric(result$Habitat_type)*2
# result$Transect_Elevation = result$Transect_Elevation*3

 
data = result
colnames(data)
dim(data)
head(data)
 

# Hierarchical clustering
hc <- hclust(dist(data[-1]), method='average')
png("dendrogram.png", height = 700, width = 1400)
par(mar=c(1,5,5,1))
plot(hc,main = "", labels=data$Species,cex=1, xlab="", ylab="")
dev.off()
# # Convert to dendrogram
# dend <- as.dendrogram(hc)
# 
# plot(dend,main = "Avian community Clustering", labels= data$Species,cex=0.8)
# 
# # Plot the dendrogram
# plot(dend, main = "Dendrogram for 97 Species (Standardized)", xlab = "Species", ylab = "Distance",horiz = TRUE)
# 
# # You can also save the plot as an image file (e.g., PNG)
# # ggsave("standardized_dendrogram_plot.png", plot = plot(dend), width = 10, height = 8, dpi = 300)
# 
# 
# # Example with pruned dendrogram
# pruned_dend <- cut(dend, h = 0.8)  # Adjust the height threshold as needed
# plot(pruned_dend, main = "Pruned Dendrogram", xlab = "Species", ylab = "Distance")
# 
# 
# # Example with increased plot size
# pdf("dendrogram_plot.pdf", width = 12, height = 10)  # Adjust width and height as needed
# plot(dend, main = "Dendrogram for 97 Species", xlab = "Species", ylab = "Distance")
# dev.off()  # Save the plot and close the PDF device
# 
# 
# 
# 
# 
# 
# # Install and load necessary packages
# # install.packages("dplyr")
# library(dplyr)
# 
# # Assuming your dataset is named 'your_dataset'
# # Replace 'your_dataset' with the actual name of your dataset
# 
# # Example dataset with duplicate species
# your_dataset <- data.frame(
#   Species = c("A", "B", "A", "B", "C"),
#   Value1 = c(1, 2, 3, 4, 5),
#   Value2 = c(6, 7, 8, 9, 10)
# )
# 
# # Use dplyr to group by species and calculate the mean for other values
# result <- your_dataset %>%
#   group_by(Species) %>%
#   summarise(across(where(is.numeric), mean))
# 
# # Print the result
# print(result)



# Install and load necessary packages
# install.packages("dplyr")
# library(dplyr)
# 
# # Define a function to calculate the mode
# calculate_mode <- function(x) {
#   unique_values <- unique(x)
#   counts <- table(x)
#   mode_value <- unique_values[which.max(counts)]
#   return(mode_value)
# }
# 
# # Assuming your dataset is named 'your_dataset'
# # Replace 'your_dataset' with the actual name of your dataset
# 
# # Example dataset with duplicate species
# your_dataset <- data.frame(
#   Species = c("A", "B", "A", "B", "C", "C","A","C","C"),
#   Category = c("Y", "Y", "X", "Y", "Y", "Z","Y","Y","Y"),
#   Value1 = c(1, 2, 3, 4, 5, 6,7,8,9),
#   Value2 = c(6, 7, 8, 9, 10, 11,12,13,14)
# )
# 
# # Use dplyr to group by Species, and calculate the mean for other values and the mode for the categorical variable
# result <- your_dataset %>%
#   group_by(Species) %>%
#   summarise(
#     Category_Mode =  calcmode(Category),
#     Value1_Mean = mean(Value1),
#     Value2_Mean = mean(Value2)
#   )
# 
# # Print the result
# print(result)
# 
# 
# mode(1,2,2,3)
# 
# 
# calculate_mode <- function(x) {
#   tbl <- table(x)
#   mode_values <- tbl[tbl == max(tbl)]
#   return(as.character(mode_values))
# }
# 
# # Example data with a categorical variable
# categories <- c("1", "1", "2", "2", "3", "3", "4", "4", "4")
# 
# # Calculate mode using the custom function
# mode_result <- calculate_mode(categories)
# 
# # Print the result
# cat("Mode:", mode_result, "\n")
# 
# summary(data)
# 
# 
# 
# calcmode <- function(a) {  
#   vector <- unique(a)  
#   vector[which.max(tabulate(match(a, vector)))]  
# }  
# 
# calcmode(data$Habitat_type)
