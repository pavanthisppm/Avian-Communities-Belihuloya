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


data1 = ToPawanthi_Master_data_set_new_2_
data1

# Low_land
col_no = which(colnames(data1) %in% Low_land)

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

