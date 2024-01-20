data = Species_Elevation
sum(is.na(data))
dim(data)

png("species-elevation.png", height = 1400, width = 1200)
par(mar=c(5,20,1,1))

 
plot(1, type='n', xlim=c(480, 1420), ylim=c(1, 97), xlab="Elevation", ylab = "" , yaxt="n", xaxt="n")
mtext("Species", side = 2, line = 18, at = 50, las = 0)
 
segments(data$x_start, data$y_values, data$x_end, data$y_values, col="blue", lwd=7)

axis(2, at=data$y_values, labels=data$y_labels, las=2)

axis(1, at=seq(480, 1420, by=20), labels=seq(480, 1420, by=20))

abline(v = c(480, 580, 700, 780, 870, 1000, 1100, 1250, 1360, 1420), col = "red", lty = 2)

dev.off()



# library(openxlsx)
# write.xlsx(df,"species-elevation.xlsx", rowNames = FALSE)
# x_start <- c(480,480, 480, 480, 480,480,480,480,480,480,480,1000,480,480,480,480,480,1420,480,1420,480,1100,480,1000,480,480,480,480,480,480,480,480,480,1420,480,480,
#              480,480,480,480,480,480,480,480,480,480,480,480,480,480,480,480,480,580,580,580,580,580,1420,580,1420,580,580,580)
# x_end <- c(1420, 1420,1420, 1420, 1420,1420,1420,1420,1420,1420,480,1360,1250,1250,1250,1250,1000,1420,1000,1420,700,1420,480,1420,1100,1100,1100,1000,1000,1000,1000,1000,870,1420,870,870,
#            780,780,780,780,780,700,700,700,700,700,870,580,580,480,480,480,480,1420,1250,1100,1100,870,1420,780,1420,780,780,780)
# y_values <- c(1,2,3,4,5,6,7,8,9,10,11,11,12,13,14,15,16,16,17,17,18,18,19,19,20,21,22,23,24,25,26,27,28,28,29,30,
#               31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,52,53,54,55,56,57)
# y_labels <- c("Spurfowl", "Jungle fowl", "Crested Serpant Eagle", "Indian swiftlet", "Yellow fronted Barbet",
#               "Brown capped Babbler","Scimitter Babbler","Tickell's blue Flycatcher","Pale billed Flowerpecker","Spot winged Thrush",
#               "Blue-tailed Bee-eater","Blue-tailed Bee-eater","Banded bay Cuckoo","Common Iora","Black capped Bulbul Asian brown","Flycatcher",
#               "Brown headed barbet", "Brown headed barbet","Brown breasted Flycatcher","Brown breasted Flycatcher","Orange minivet","Orange minivet","Barn Swallow","Barn Swallow","Sri Lanka Hanging Parrot",
#               "Red vented Bulbul","Square tailed bulbul","Crimson fronted Barbet","Asian Paradise Flycatcher","Loten's sunbird",
#               "Oriental white eye","Velvet fronted nuthatch","Sri Lanka Lesser Flameback","Sri Lanka Lesser Flameback","Indian Cuckoo","White browed Bulbul",
#               "Greater coucal","Black hooded Oriole","Common Tailorbird","Dark fronted Babbler","Purple rumped Sunbird",
#               "Spotted Dove","Emerald Dove","Dwaft kingfisher","White bellied drongo","Yellow billed Babbler",
#               "Lesser hill myna","Oriental magpie Robin","Indian Blue Robin","Indian peafowl","Common Kingfisher",
#               "Jerdon's leafbird","Common mynah","Large billed leaf warbler","Brown shrike","Common Hawk Cuckoo",
#               "Ashy Prinia","White rumped Shama","White rumped Shama","Chestnut backed Owlet","Chestnut backed Owlet","Layerd's Parakeet",
#               "Sri Lanka Grey Hornbill","Purple sunbird")
# 
# df = data.frame(x_start, x_end, y_values, y_labels)
# 
# par(mar=c(2,14,1,1))
# 
# # Create a blank plot with no automatic y-axis labels
# plot(1, type='n', xlim=c(480, 1420), ylim=c(45, 50), xlab="", ylab = "" , yaxt="n", xaxt="n")
# 
# # Draw horizontal lines
# segments(x_start, y_values, x_end, y_values, col="blue", lwd=7)
# 
# 
# # Customize y-axis labels with new labels
# axis(2, at=y_values, labels=y_labels, las=2)
# 
# # Set custom breaks and labels for the x-axis
# axis(1, at=seq(480, 1420, by=20), labels=seq(480, 1420, by=20))
# 




# 
# 
# 
# # Example data
# x_start <- c(1, 3, 2)
# x_end <- c(4, 6, 5)
# y_values <- c(2, 4, 6)
# y_labels <- c("Label A", "Label B", "Label C")
# 
# # Enlarge the plotting area
# par(pin=c(5, 4), mai=c(1, 1, 1.5, 1))
# 
# # Create a blank plot with no automatic y-axis labels
# plot(1, type='n', xlim=c(0, 7), ylim=c(0, 8), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines", yaxt="n")
# 
# # Draw horizontal lines
# segments(x_start, y_values, x_end, y_values, col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(x_start, y_values, pch=16, col="red", cex=1.5)
# points(x_end, y_values, pch=17, col="green", cex=1.5)
# 
# # Customize y-axis labels with new labels and adjust orientation
# axis(2, at=y_values, labels=y_labels, las=2, pos=0, padj=1)

# # Example data
# x_start <- c(1, 3, 2)
# x_end <- c(4, 6, 5)
# y_values <- c(2, 4, 6)
# y_labels <- c("Label A", "Label B", "Label C")
# 
# # Increase margin at the bottom for y-axis labels
# par(mar = c(5, 4, 4, 2) + 0.1)
# 
# # Create a blank plot with no automatic y-axis labels
# plot(1, type='n', xlim=c(0, 7), ylim=c(0, 8), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines", yaxt="n")
# 
# # Draw horizontal lines
# segments(x_start, y_values, x_end, y_values, col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(x_start, y_values, pch=16, col="red", cex=1.5)
# points(x_end, y_values, pch=17, col="green", cex=1.5)
# 
# # Customize y-axis labels with new labels
# axis(2, at=y_values, labels=y_labels, las=2, pos = 0, padj = 1)


# # Create a blank plot
# plot(1, type='n', xlim=c(0, 1420), ylim=c(0, 50), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines")
# 
# # Create a blank plot
# plot(1, type='n', xlim=c(0, max(counts) + 1), ylim=c(0, length(categories) + 1),
#      xlab="Counts", ylab="Categories", main="Horizontal Lines with String Y-axis")
# 
# # Draw barplot
# barplot(counts, names.arg=categories, horiz=TRUE, col="lightblue", border="black", las=1)
# 
# 
# # Draw horizontal lines
# segments(x_start, y_values, x_end, y_values, col="blue", lwd=2)
# 
# # # Add points to indicate the starting and ending points
# # points(x_start, y_values, pch=16, col="red", cex=1.5)
# # points(x_end, y_values, pch=17, col="green", cex=1.5)
# 
# # Example data
# x_start <- c(1, 3, 2)
# x_end <- c(4, 6, 5)
# y_values <- c(2, 4, 6)
# 
# # Create a blank plot
# plot(1, type='n', xlim=c(0, 7), ylim=c(0, 8), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines")
# 
# # Draw horizontal lines
# abline(h = y_values, col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(x_start, y_values, pch=16, col="red", cex=1.5)
# points(x_end, y_values, pch=17, col="green", cex=1.5)
# 
# # Example data
# start_points <- c("A", "B", "C")
# end_points <- c("D", "E", "F")
# x_values <- c(1, 3, 2)
# 
# # Create a blank plot
# plot(1, type='n', xlim=c(0, 4), ylim=c(0, 7), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines with String Y-axis")
# 
# # Convert string values to numerical positions on the y-axis
# y_start <- match(start_points, unique(c(start_points, end_points)))
# y_end <- match(end_points, unique(c(start_points, end_points)))
# 
# # Draw horizontal lines
# segments(x_values, y_start, x_values, y_end, col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(rep(x_values, each=2), c(y_start, y_end), pch=16, col="red", cex=1.5)
# 
# # Example data
# categories <- c("A", "B", "C", "D", "E")
# counts <- c(3, 5, 2, 4, 6)
# 
# # Create a blank plot
# plot(1, type='n', xlim=c(0, max(counts) + 1), ylim=c(0, length(categories) + 1),
#      xlab="Counts", ylab="Categories", main="Horizontal Lines with String Y-axis")
# 
# # Draw barplot
# barplot(counts, names.arg=categories, horiz=TRUE, col="lightblue", border="black", las=1)
# 
# # Draw horizontal lines
# segments(0, 1:length(categories), counts, 1:length(categories), col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(counts, 1:length(categories), pch=16, col="red", cex=1.5)
# 
# 
# 
# # Example data with text labels for y-axis
# x_start <- c(1, 3, 2)
# x_end <- c(4, 6, 5)
# y_labels <- c("Label A", "Label B", "Label C")
# 
# # Create a blank plot
# plot(1, type='n', xlim=c(0, 7), ylim=c(0, 8), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines")
# 
# # Draw horizontal lines
# segments(x_start, seq_along(y_labels), x_end, seq_along(y_labels), col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(x_start, seq_along(y_labels), pch=16, col="red", cex=1.5)
# points(x_end, seq_along(y_labels), pch=17, col="green", cex=1.5)
# 
# # Customize y-axis labels
# axis(2, at=seq_along(y_labels), labels=y_labels)
# axis(2, at=seq_along(y_labels), labels=y_labels)
# 
# 
# 
# 
# 
# # Example data
# x_start <- c(1, 3, 2)
# x_end <- c(4, 6, 5)
# y_values <- c(2, 4, 6)
# y_labels <- c("Label A", "Label B", "Label C")
# 
# # Create a blank plot with no automatic y-axis labels
# plot(1, type='n', xlim=c(0, 7), ylim=c(0, 8), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines", yaxt="n")
# 
# # Draw horizontal lines
# segments(x_start, y_values, x_end, y_values, col="blue", lwd=2)
# 
# # Add points to indicate the starting and ending points
# points(x_start, y_values, pch=16, col="red", cex=1.5)
# points(x_end, y_values, pch=17, col="green", cex=1.5)
# 
# # Customize y-axis labels with new labels
# axis(2, at=y_values, labels=y_labels, las=2)
# 
# 
# 
