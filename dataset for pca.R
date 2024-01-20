library(openxlsx)

data = pca_and_cluster_dataset
dim(data)
df1 = data[,colnames(data[1,1:7])] 
head(df1)
colnames(data[1,1:7])

df2 = data[,colnames(data[1,8:103])]
dim(df2)
 
df2$ID <- 1:189
 
df2 <- df2[c("ID", names(df2)[-which(names(df2) == "ID")])]
 
tail(df2)
summary(df2)

df2$Species <- apply(df2[, -c(1, ncol(df2))], 1, function(x) names(x[!is.na(x)]))
df = cbind(df1,df2)

df3 = PCA
data <- df3 %>%
  separate_rows(Species, sep = ",\\s*")

result <- data %>%
  slice(rep(row_number(), each = 1))


write.xlsx(result,"PCA-New.xlsx", rowNames = FALSE)

# 
# # Assuming 'your_data' is your dataframe
# # Create example data with 97 variables
# set.seed(123)
# your_data <- data.frame(
#   ID = 1:10,
#   var1 = c(NA, 2, NA, 4, 5, 6, NA, 8, NA, 10),
#   var2 = c(1, 2, 3, 4, 5, NA, 7, 8, 9, 10),
#   var3 = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10),
#   var4 = c(NA, 2, NA, 4, 5, 6, NA, 8, NA, 10),
#   var5 = c(1, 2, 3, 4, 5, NA, 7, 8, 9, 10),
#   var6 = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
# )
# 
# # Create a new variable with variable names having values
# your_data$variable_names <- apply(your_data[, -c(1, ncol(your_data)+1)], 1, function(x) names(x[!is.na(x)]))
# 
# # Print the resulting dataframe
# print(your_data)
#  
# 
# # Assuming 'df' is your existing data frame
# # Create a sample data frame
# df <- data.frame(
#   ID = c(1, 2, 3),
#   species = I(list(c("A", "B"), c("B", "C", "D"), c("A")))
# )
# 
# # Install and load the tidyverse package if not already installed
# # install.packages("tidyverse")
# library(tidyverse)
# 
# # Your data frame (replace this with your actual data)
# your_data <- data.frame(
#   species = c("SA, SB, SC", "SA,SD,SE", "SW,SC,SF,SE,ST"),
#   variable1 = c(1, 2,3),
#   variable2 = c(4, 5,6)
# )
# 
# # Split the values in the "species" column
# your_data <- your_data %>%
#   separate_rows(species, sep = ",\\s*")
# 
# # Duplicate rows based on the "species" column
# result <- your_data %>%
#   slice(rep(row_number(), each = 1))
# 
# # Print the result
# print(result)
# 
