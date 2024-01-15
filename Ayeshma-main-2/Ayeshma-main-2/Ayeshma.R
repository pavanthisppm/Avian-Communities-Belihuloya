library(MASS)
library(AER)
 
# # Nov
# 
# data_nov = data[1:17,]
# data_dec = data[18:51,]
# data_jan = data[52:77,]
# data_feb = data[78:101,]
# data_mar = data[102:123,]
# data_apr = data[124:133,]
# 
# par(mfrow = c(1, 6))
#  
# hist(data_nov$Average_Cloud_cover, col='red', probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))
# hist(data_dec$Average_Cloud_cover, col='blue',  probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))
# hist(data_jan$Average_Cloud_cover, col='green',  probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))
# hist(data_feb$Average_Cloud_cover, col='orange',  probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))
# hist(data_mar$Average_Cloud_cover, col='black',  probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))
# hist(data_apr$Average_Cloud_cover, col='brown',  probability = TRUE)
# lines(density(data_nov$Average_Cloud_cover))



# load dataset
data = ToPawanthi_Master_data_set
head(data)
colnames(data)
sum(is.na(data))

# Transect & Elevation
data$Extracted_Transect_Elevation=substr(data$`Transect_&_Elevation`,6,11)
data$Transect_Elevation=gsub('m','',data$Extracted_Transect_Elevation)
class(data$`Transect_Elevation`) = "Numeric"
data$Transect_Elevation = as.numeric(data$Transect_Elevation)
mode(data$Transect_Elevation)

# check mean & variance
# Abundance
hist(data$Abundance)
mean(data$Abundance)
var(data$Abundance)

# SpeciesRichness
hist(data$Species_richness)
mean(data$Species_richness)
var(data$Species_richness)


#NB - Abundance
null = glm.nb(Abundance ~ 1, data=data)
full = glm.nb(formula = Abundance ~ Average_Humidity + Average_Temperature+Average_Cloud_cover+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed+Visibility_Percentage, data = data, link = log)

## forward
Ab_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
Ab_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
Ab_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")



#NB - SR
#NB
null = glm.nb(Species_richness ~ 1, data=data)
full = glm.nb(formula = Species_richness ~ Average_Humidity + Average_Temperature+Average_Cloud_cover+Habitat_type+Transect_Elevation+Average_Light_intensity+Average_Wind_speed+Visibility_Percentage, data = data, link = log)


## forward
SR_fwd = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="forward")

## backward
SR_bwd = stepAIC(full, scope = list(lower=null, upper=full), data=data, direction="backward")

## both
SR_both = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")


# MODEL_BUILDING
new = glm.nb(formula = Species_richness ~ Average_Humidity +Average_Cloud_cover+Habitat_type+Transect_Elevation+ Average_Wind_speed+Visibility_Percentage, data = data, link = log)
summary(new)




step_model = stepAIC(null, scope = list(lower=null, upper=full), data=data, direction="both")
sum(is.na(data))

summary(full)
dispersiontest(full, trafo=2)
summary(step_model)   
summary(data)
write.csv(data)


 
data = Sheet3
