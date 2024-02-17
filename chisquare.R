data = Belihuloya_recorded_species_list
head(data)

# data1 = ToPawanthi_Master_data_set_new
# data1
# data1$Diff[2]
# length(data1$Diff)
# length(data$Species)

data$`Highland specialists`[is.na(data$`Highland specialists`)]=0
data$`Mid-elevation`[is.na(data$`Mid-elevation`)]=0
data$`Lowland specialist`[is.na(data$`Lowland specialist`)]=0

data[5,][2]

Low_land =  c()
Mid_land = c()
High_land = c()

j = 0
data$`Highland specialists`[5]
data$Species[5]

for (i in 1:length(data$Species)){
  if (data$`Highland specialists`[i]=="X" ) {
    High_land = append(High_land, data$Species[i])
  }
}
    
for (i in 1:length(data$Species)){
  if (data$`Mid-elevation`[i]=="X") {
    Mid_land  = append(Mid_land, data$Species[i])
  }
}
    
for (i in 1:length(data$Species)){
  if (data$`Lowland specialist`[i]=="X") {
    Low_land = append(Low_land, data$Species[i])
  }
}
    
  
#   }else if (data$`Mid-elevation`[i]=="X"){
#     Mid_land  = append(Mid_land, data$Species[i])
#   }else if (data$`Lowland specialist`[i]=="X"){
#     Low_land = append(Low_land, data$Species[i])
#   }
# }

data$Species[1]
Low_land
Mid_land 
High_land

Low_land = unique(Low_land)
Mid_land = unique(Mid_land)
High_land = unique(High_land)  

Low_land
Mid_land
High_land

################################### Chi Square 1###############################################

data1 = ToPawanthi_Master_data_set_new_2_
data1

# Low_land
col_no_low = which(colnames(data1) %in% Low_land)
col_no_mid = which(colnames(data1) %in% Mid_land)
col_no_high = which(colnames(data1) %in% High_land)


# a = c(1,1,2,3,4,4,5,5,6)
# b = c(2,2,3,4,4,5)
# duplicated(a,b)
# 
# unique(a[which(a %in% b)])

a = c()
for (i in col_no){
  a = append(a, data1$Diff[is.na(data1[,i])== FALSE])
}
a


LG100 = 0
LL100 = 0

for (i in 1:length(a)){
  if (a[i]>=100){
    LG100 = LG100+1
  }else if (a[i]<100){
    LL100 = LL100+1
  }
}

LG100 #335
LL100 #75


# Mid_land
col_no = which(colnames(data1) %in% Mid_land)

a = c()
for (i in col_no){
  a = append(a, data1$Diff[is.na(data1[,i])== FALSE])
}
a


MG100 = 0
ML100 = 0

for (i in 1:length(a)){
  if (a[i]>=100){
    MG100 = MG100+1
  }else if (a[i]<100){
    ML100 = ML100+1
  }
}

MG100 #396
ML100 #84



# High_land
col_no = which(colnames(data1) %in% High_land)

a = c()
for (i in col_no){
  a = append(a, data1$Diff[is.na(data1[,i])== FALSE])
}
a


HG100 = 0
HL100 = 0

for (i in 1:length(a)){
  if (a[i]>=100){
    HG100 = HG100+1
  }else if (a[i]<100){
    HL100 = HL100+1
  }
}

HG100 #211
HL100 #46


# Create the contingency table

df = data.frame(less_than_100=c(75,84, 46),greater_than_100=c(335, 396, 211), row.names=c("Lowland","Midland", "Highland"))
df 

# Create a matrix with the data
data_matrix <- matrix(c(75, 335, 84, 396, 46, 211), nrow = 3, byrow = TRUE,
                      dimnames = list(c("Lowland", "Midland", "Highland"),
                                      c("less_than_100", "greater_than_100")))

# Perform chi-square test
chi_square_result <- chisq.test(data_matrix)

# Print the result
print(chi_square_result)


 
# fisher test
fisher.test(data_matrix)


################################## Chi Sqaure 2###################################################

data1 = ToPawanthi_Master_data_set_new_4_


# create the variable transect

# 480-700
data1$transect = NA

for (i in 1:length(data1$Sampling_Session)){
  if (480<=data1$`Transect_&_Elevation`[i] & data1$`Transect_&_Elevation`[i]<=700){
    data1$transect[i] = "Low"
  }
}


# 780-1000

for (i in 1:length(data1$Sampling_Session)){
  if (780<=data1$`Transect_&_Elevation`[i] & data1$`Transect_&_Elevation`[i]<=1000){
    data1$transect[i] = "Mid"
  }
}

# 1100 - 1420

for (i in 1:length(data1$Sampling_Session)){
  if (1100<=data1$`Transect_&_Elevation`[i] & data1$`Transect_&_Elevation`[i]<=1420){
    data1$transect[i] = "High"
  }
}

data1$transect[34]
data1$`Transect_&_Elevation`[34]





which(!(is.na(data1$`Emerald Dove`)))
data2$transect[which(!(is.na(data2$`Emerald Dove`)))]
d1_col_names = colnames(data1)[16:112]
data2$transect
data2 = data1[16:113]
colnames(data2)



for (i in 1:97){
    cat(d1_col_names[i] ,  table(data2$transect[which(!(is.na(data2[i])))]), "\n")
}
 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

a = c()
for (i in 1:97){
  a[i] = getmode(data2$transect[!is.na(data2[[i]])])
}
a

 
LG100 = 0
LL100 = 0
MG100 = 0
ML100 = 0
HG100 = 0
HL100 = 0

data1$Diff = abs(data1$`Transect_&_Elevation`-data1$Cloudline)

for (i in 1:length(a)){
  if (data1$Diff[1]>=100 & data1$transect == "Low"){
    LG100 = LG100+1
  }else if (data1$Diff[i]<100 & data1$transect == "Low"){
    LL100 = LL100+1
  }else if(data1$Diff[i]>=100 & data1$transect == "Mid"){
    MG100 = MG100+1
  }else if (data1$Diff[i]<100 & data1$transect == "Mid"){
    ML100 = ML100+1
  }else if (data1$Diff[i]>=100 & data1$transect == "High"){
    HG100 = HG100+1
  }else if (data1$Diff[i]<100 & data1$transect == "High"){
    HL100 = HL100+1
  }
}

# for (i in 1:97){
#  
#    print(table(data2$transect[which(!(is.na(data2[i])))]) )
# }


 
