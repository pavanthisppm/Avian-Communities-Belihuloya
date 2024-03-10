data1 = ToPawanthi_Master_data_set_new_8_



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


data1$transect


d1_col_names = colnames(data1)[17:113]
dim(data1)
data2 = data1[17:114]
colnames(data2)
nrow(data2)
dim(data2)
data2


for (i in 1:96){
  cat(d1_col_names[i] ,  table(data2$transect[which(!(is.na(data2[97])))]), "\n")
}

colnames(data2)

table(data2$transect[which(!(is.na(data2$`King quail`)))])

## no data for elevation
for (i in 1:97) {
  if((sum(is.na(data2[[i]]))==153)){
    print(colnames(data2[i]))
  }
}
# "Plain prinia"


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

a = c()
for (i in 1:96){
  a[i] =  list(unique(data2$transect[!is.na(data2[[i]])]))
}
a
a = a[!is.na(a)]
print(a)
data2$transect
# a[2] = "Mid"
# a[22] = "Mid"
# a[23] = "Low"
# a[28] = "Mid"
# a[38] = "Low"
# a[40] = "Low"
# a[54] = "Mid"
# a[61] = "Mid"
# a[71] = "Low"
# a[87] = "Mid"
# a[93] = "Mid"
# print(a)
# length(a)
# colnames(data2)

table(data2$transect[which(!(is.na(data2$`Velvet fronted nuthatch`)))])




data = General_Elevation_Ranges_of_Birds_corrected
head(data)


data$Low[is.na(data$Low)]=0
data$`Low-Mid`[is.na(data$`Low-Mid`)]=0
data$Mid[is.na(data$Mid)]=0
data$`Mid-High`[is.na(data$`Mid-High`)]=0
data$High[is.na(data$High)] =0
data$`High-Mid-Low`[is.na(data$`High-Mid-Low`)]=0


Low =  c()
Mid = c()
High = c()
Low_Mid = c()
Mid_High = c()
High_Mid_Low =c()



for (i in 1:length(data$Species)){
  if (data$High[i]=="X" ) {
    High  = append(High , data$Species[i])
  }
}

for (i in 1:length(data$Species)){
  if (data$Mid[i]=="X") {
    Mid  = append(Mid , data$Species[i])
  }
}

for (i in 1:length(data$Species)){
  if (data$Low[i]=="X") {
    Low  = append(Low , data$Species[i])
  }
}

for (i in 1:length(data$Species)){
  if (data$`Low-Mid`[i]=="X") {
    Low_Mid  = append(Low_Mid , data$Species[i])
  }
}

for (i in 1:length(data$Species)){
  if (data$`Mid-High`[i]=="X") {
    Mid_High  = append(Mid_High , data$Species[i])
  }
}

for (i in 1:length(data$Species)){
  if (data$`High-Mid-Low`[i]=="X") {
    High_Mid_Low  = append(High_Mid_Low , data$Species[i])
  }
}


Low #10
Mid #3
High #9
Low_Mid #43
Mid_High #13
High_Mid_Low #18



################################### Chi Square 1###############################################

data1 = ToPawanthi_Master_data_set_new_8_
data1


col_no_low = which(colnames(data1) %in% Low)
col_no_mid = which(colnames(data1) %in% Mid)
col_no_high = which(colnames(data1) %in% High)
col_no_low_mid = which(colnames(data1) %in% Low_Mid)
col_no_mid_high = which(colnames(data1) %in% Mid_High)
col_no_high_mid_low = which(colnames(data1) %in% High_Mid_Low)


# b = c(col_no_high, col_no_high_mid_low, col_no_low, col_no_low_mid, col_no_mid, col_no_mid_high)
# length(b)

# Low_land
a = c()
b = c()

for (i in col_no_low) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

LL100 = c()
LG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    LG100[i] = b[i]
  }else if(a[i]<100){
    LL100[i] = b[i]
  }
}

sum(LG100, na.rm = TRUE) #72
sum(LL100, na.rm = TRUE) #33


# Mid_land
a = c()
b = c()

for (i in col_no_mid) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

ML100 = c()
MG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    MG100[i] = b[i]
  }else if(a[i]<100){
    ML100[i] = b[i]
  }
}

sum(MG100, na.rm = TRUE) #6
sum(ML100, na.rm = TRUE) #2



# High_land
a = c()
b = c()

for (i in col_no_high) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

HL100 = c()
HG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    HG100[i] = b[i]
  }else if(a[i]<100){
    HL100[i] = b[i]
  }
}

sum(HG100, na.rm = TRUE) #24
sum(HL100, na.rm = TRUE) #7

#low-mid

a = c()
b = c()

for (i in col_no_low_mid) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

LML100 = c()
LMG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    LMG100[i] = b[i]
  }else if(a[i]<100){
    LML100[i] = b[i]
  }
}

sum(LMG100, na.rm = TRUE) #117
sum(LML100, na.rm = TRUE) #26

#mid-high

a = c()
b = c()

for (i in col_no_mid_high) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

MHL100 = c()
MHG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    MHG100[i] = b[i]
  }else if(a[i]<100){
    MHL100[i] = b[i]
  }
}

sum(MHG100, na.rm = TRUE) #84
sum(MHL100, na.rm = TRUE) #19

#high-mid-low

a = c()
b = c()

for (i in col_no_high_mid_low) {
  a = c(a, data1$Diff[!is.na(data1[, i])])
  b = c(b, data1[[i]][!is.na(data1[, i])])
}

a
b

HMLL100 = c()
HMLG100 = c()
for (i in 1:length(a)) {
  if (a[i]>=100){
    HMLG100[i] = b[i]
  }else if(a[i]<100){
    HMLL100[i] = b[i]
  }
}

sum(HMLG100, na.rm = TRUE) #452
sum(HMLL100, na.rm = TRUE) #91






# Create the contingency table

# df = data.frame(less_than_100=c(75,84, 46),greater_than_100=c(335, 396, 211), row.names=c("Low","Mid", "High", "Low-Mid", "Mid-High", "High-Mid-Low"))
# df

# Create a matrix with the data
data_matrix <- matrix(c(72,33,6,2,24,7,117,26,84,19,452,91), nrow = 6, byrow = TRUE,
                      dimnames = list(c("Low","Mid", "High", "Low-Mid", "Mid-High", "High-Mid-Low"),
                                      c("greater_than_100", "less_than_100")))

data_matrix
# Perform chi-square test
chi_square_result <- chisq.test(data_matrix, simulate.p.value = TRUE)

# Print the result
print(chi_square_result)



# fisher test
fisher.test(data_matrix)


