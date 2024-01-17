# Example data
x_start <- c(480,480, 480, 480)
x_end <- c(1420, 1420,1420, 1420)
y_values <- c(1,2,3,4)
y_labels <- c("Spurfowl", "Jungle fowl", "Crested Serpant Eagle", "Indian swiftlet")

# Create a blank plot with no automatic y-axis labels
plot(1, type='n', xlim=c(0, 1420), ylim=c(0, 4), xlab="X-axis", ylab="Y-axis", main="Horizontal Lines", yaxt="n")

# Draw horizontal lines
segments(x_start, y_values, x_end, y_values, col="blue", lwd=2)

 
# Customize y-axis labels with new labels
axis(2, at=y_values, labels=y_labels, las=2)


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
