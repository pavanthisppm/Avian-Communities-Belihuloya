data = ToPawanthi_Master_data_set_new_3_
table(data)
data1 = as.data.frame(table(data))
data1

data <- data1[data1$Freq != 0, , drop = FALSE]
data

class(data$Cloudline)

data$Cloudline = as.numeric(as.character(data$Cloudline))

library(ggplot2)

png("Cloudline-Month.png", height = 500, width = 700)

ggplot(data, aes(x = Sampling_Session, y = Cloudline, size = Freq)) +
  geom_point()+ 
  geom_text(aes(label = paste(Cloudline,"(",Freq,")")), size = 4, vjust = 2.2) +
  scale_size(name = "Size", range = c(5, 12)) +
  theme(legend.position = "none")+
  labs(x = "Month", y = "Elevation of the cloudline", title = " ") +
  scale_x_discrete(breaks = 1:6, labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))+
  scale_y_continuous(name = "Cloudline Elevation", limits = c(0, 2000), breaks = seq(0, 2000, by = 500))

dev.off()

 


