usedCar.org <- read.csv("C:/Users/jinru/Desktop/R/First_project_usedCare/vehicles.csv", header = T, na.strings = c(" ", "", "other", "NA", "missing")) 
head(usedCar.org)
colnames(usedCar.org)
###########Useful Data -Select valuable columns for prediction on price 
unique(usedCar.org$region)  #decide not to be included
unique(usedCar.org$model)   #Warning: very dirty 
unique(usedCar.org$drive)
unique(usedCar.org$size)
unique(usedCar.org$state) 
###Install packages
library(ggplot2) 
install.packages("usmap")
library(usmap)
install.packages("sf")
library(sf)
install.packages("mapview")
library(mapview)
install.packages("ggmap")
library(ggmap)
library(tidyverse)
###Data.Frame structure of selected columns
usedCar.inUse <- usedCar.org[c("price", "year", "manufacturer", "condition", "cylinders", "fuel", "odometer", "title_status", "transmission", "drive", "size", "type","paint_color","state", "lat", "long")]

###Visualization the raw data
#register a Google map: 告诉R 我要用Google map
register_google("AIzaSyDT_CEMxmMUj9y6sG6FrZtQY87KJoqxw6c")
#retrieve the us (long, lat) 
geocode("us")
#create a map
us_map <- get_googlemap(center = c(-95.7, 37.1), zoom = 4, scale = 2)
#                                     zoom放大倍率，需要与scale一起调

#ggplot
ggmap(us_map) + geom_point(data = usedCar.inUse, aes(x = long, y = lat, color = state), size = 0.2)

###############################------------------
library(usmap)
unique(usedCar.inUse$state)

v = usedCar.inUse$paint_color
table(v)
sort(table(v))


unique.state <- unique(usedCar.inUse$state)
state.df <- as.data.frame(cbind(unique.state, c(1:51)))
head(state.df)
colnames(state.df) <- c("state", "testing")
#             data: The columns of data must be fips or state and the value of the 'values' parameter.
head(state.df)

?plot_usmap
plot_usmap(regions = c("states"), labels = TRUE, data = state.df, values = "testing")


#### Hint: switcher 
## mode ~ count the most in categorical 
## Function goal:
# For categorical data, only need to return mode / pct
# For numerical data, need to return mode, pct, mean
## Function parameter:
## Trim: for numerical data, need to consider of outlier for mean calculation and by default, we want to remove top 5% and bottom 5%
## mean_check ~ is a switcher for differentiating categorical data and numerical data
## Input: "v", array/list 
## Output: "mode of array, pct of mode, mean of array (for numerical col)"
getmode_mean_pct <- function(v, mean_check = FALSE, trim = 0.1){ 
  math_mode <- names(na.omit(sort(-table(v))))[1]  
  pct <- length(na.omit(v[v == math_mode]))/length(na.omit(v))
  if (!mean_check){
    result <- list("mode" = math_mode, "percent" = pct)
    return(result)
  }else{
    # Mean outlier remove
    v_remove_na <- na.omit(v)
    v_outlier_remove <- sort(v_remove_na)[-c((1:round(length(v_remove_na)*trim/2)),
                                             ((length(v_remove_na)-round(length(v_remove_na)*trim/2)) :length(v_remove_na)))]
    result <- list("mode" = math_mode, "mean" = mean(v_outlier_remove), "percent" = pct)
    return(result)
  }
}

year_mean <- c()
year_mode <- c()
year_pct <- c()

manu_mode <- c()
manu_pct <- c()

odometer_mean <- c()

type_mode <- c()  
type_pct <- c()

paint_mode <- c()
paint_pct <- c()

for (state in unique.state){
  state_data <- usedCar.inUse[usedCar.inUse$state == state, ]
  year_mean <- c(year_mean, round(getmode_mean_pct(state_data$year, mean_check = TRUE)$mean,0))
  year_mode <- c(year_mode, getmode_mean_pct(state_data$year)$mode)
  year_pct  <- c(year_pct,  getmode_mean_pct(state_data$year)$percent)
  
  manu_mode <- c(manu_mode, getmode_mean_pct(state_data$manufacturer)$mode)
  manu_pct <- c(manu_pct, getmode_mean_pct(state_data$manufacturer)$percent)
  
  odometer_mean <- c(odometer_mean, round(getmode_mean_pct(state_data$odometer, mean_check = TRUE)$mean,0))
  
  type_mode <- c(type_mode, getmode_mean_pct(state_data$type)$mode)
  type_pct <- c(type_pct, getmode_mean_pct(state_data$type)$percent)
  
  paint_mode <- c(paint_mode, getmode_mean_pct(state_data$paint_color)$mode)
  paint_pct <- c(paint_pct, getmode_mean_pct(state_data$paint_color)$percent)
  
}

##  Plot
unique.state <- unique(usedCar.inUse$state)
state.df.manu <- as.data.frame(cbind(unique.state, manu_mode))
head(state.df)
colnames(state.df.manu) <- c("state", "manu_mode")
head(state.df)

plot_usmap(regions = c("states"), labels = TRUE, data = state.df.manu, values = "manu_mode") + guides(fill=guide_legend(title = "Car Manufacture")) + theme(legend.position = "right") + theme(legend.title = element_text(size = 10)) + labs(title = "The most popular Manufacture of used cars sold in each State")


unique.state <- unique(usedCar.inUse$state)
state.df.color <- as.data.frame(cbind(unique.state, paint_mode))
head(state.df.color)
colnames(state.df.color) <- c("state", "paint_mode")
head(state.df.color)

plot_usmap(regions = c("states"), labels = TRUE, data = state.df.color, values = "paint_mode") + guides(fill=guide_legend(title = "Car Color")) + theme(legend.position = "right") + theme(legend.title = element_text(size = 10)) + labs(title = "The most popular Color of used cars sold in each State")                                                                                            

unique.state <- unique(usedCar.inUse$state)
state.df.type <- as.data.frame(cbind(unique.state, type_mode))
head(state.df.type)
colnames(state.df.type) <- c("state", "type_mode")
head(state.df.type)

plot_usmap(regions = c("states"), labels = TRUE, data = state.df.type, values = "type_mode") + guides(fill=guide_legend(title = "Car Type")) + theme(legend.position = "right") + theme(legend.title = element_text(size = 10)) + labs(title = "The most popular Type of used cars sold in each State") 



unique.state <- unique(usedCar.inUse$state)
state.df.odometer <- as.data.frame(cbind(unique.state, odometer_mean))
head(sstate.df.odometer)
colnames(state.df.odometer) <- c("state", "odometer_mean")
head(state.df.odometer)
state.df.odometer$odometer_mean <- paste(as.character(round(as.numeric(state.df.odometer$odometer_mean)/10000)),"0k",sep = "")
plot_usmap(regions = c("states"), labels = TRUE, data = state.df.odometer, values = "odometer_mean") + guides(fill=guide_legend(title = "Car Odometer")) + theme(legend.position = "right") + theme(legend.title = element_text(size = 10)) + labs(title = "The average odometer of used cars sold in each State") 


#-----------------------------------Data  Cleansing
######    Null value
head(usedCar.inUse, n=20)
dim(usedCar.inUse) #return integer * integer np.shape return tuple

###  Column price
uniq_price = sort(unique(usedCar.inUse$price, na.rm = TRUE))
uniq_price
#logical: sort(small - large) - check abnormal price for a car

##Remove price to be NA
useful_data_price <- usedCar.inUse[is.na(usedCar.inUse$price) == FALSE,]
dim(useful_data_price) 
#conclusion: NO N/A  Warning: value = $0 is not NA but not included in analysis

##Create a table for defining a reasonable price range
min_list = c(0,100,500,1000,1500,2000)
max_list = c(50000, 100000, 250000, 500000, 750000, 1000000)

min_list_pct <- c()
for (p in min_list) {
  filter<-usedCar.inUse [,"price"] <= p 
  percentage_count = nrow(usedCar.inUse[filter, ])/dim(usedCar.inUse)[1] * 100
  min_list_pct <- c(min_list_pct, percentage_count) #Python: append()
}
print(min_list_pct)

max_list_pct <- c()
for (q in max_list){
  filter1 <- usedCar.inUse[,"price"] >= q
  percentage_count = nrow(usedCar.inUse[filter1, ])/dim(usedCar.inUse)[1] * 100
  max_list_pct <- c(max_list_pct, percentage_count)
}
print(max_list_pct)


data.frame(minium <-min_list,
           min_pct <- min_list_pct,
           maximum <- max_list,
           max_pct <- max_list_pct)
##conclusion: a reasonable price range is [$500,$250,000], cut off data points occupies 10.59%+0.022%

##Updated_useful_data by column price
usedCar.inUse <- useful_data_price[useful_data_price$price > 500 & useful_data_price$price < 250000, ]
dim(usedCar.inUse)



###  Column Year
mean(usedCar.inUse$year, na.rm = TRUE)
median(usedCar.inUse$year, na.rm = TRUE)
min(usedCar.inUse$year, na.rm = TRUE)
max(usedCar.inUse$year, na.rm = TRUE)
sort(unique(usedCar.inUse$year, na.rm = TRUE))

#remove NA values from year in rows
usedCar.inUse<- usedCar.inUse[is.na(usedCar.inUse$year) == FALSE,]
dim(usedCar.inUse)

#Bar_chart_original:  
barplot(prop.table(table(usedCar.inUse$year)))
##Conclusion: the year of model in market is clustered around 2000

##Thoughts: based on Bar_chart_original, range of model year is 1990 ~ 2020
usedCar.inUse <- usedCar.inUse[usedCar.inUse$year >= 1990 & usedCar.inUse$year<=2020, ]
dim(usedCar.inUse)
#Bar_Chart_refined:
barplot(prop.table(table(usedCar.inUse$year)))



###   Column: Odometer
# Remove NA from column Odometer
usedCar.inUse<- usedCar.inUse[is.na(usedCar.inUse$odometer) == FALSE,]
dim(usedCar.inUse)


##------------Problem: Important parameters about a used car are NA, delete or not?
#usedCar.inUse[usedCar.inUse$year >= 2021,]
##Conclusion: DELETE if 2 of parameters of manufacture, odometer, condition and size are NA  
colnames(usedCar.inUse)
#Hint: filter - boolean  each observation has 2 TRUE return TRUE

filter.manufa <- is.na(usedCar.inUse$manufacturer)
filter.manufa 
filter1.condi <-  is.na(usedCar.inUse$condition)
filter1.condi
filter2.odom <- is.na(usedCar.inUse$odometer)
filter2.odom
filter3.size <- is.na(usedCar.inUse$size)
filter3.size
#create dataframe
mydf <- data.frame(filter.manufa, filter1.condi, filter2.odom, filter3.size)
head(mydf)


#R iterate each row if 2 of four columns returns TRUE, then return FALSE and return row number
#IF DELETE, RETURN FALSE
#Where TRUE means NA
#list: output ~
filter_ult = c()
for (i in 1:dim(mydf)[1]){
  temp <- mydf[i,]
  if (sum(as.numeric(mydf[i, ])) > 1){
    filter_ult <- c(filter_ult, FALSE)
  }else {
    filter_ult <- c(filter_ult, TRUE)
  }
}   
print(head(filter_ult))

#Update data set from remove NA rows
usedCar.inUse<- usedCar.inUse[filter_ult,]
dim(usedCar.inUse)

#scale = 4 columns 少写了计数器  move to next step but forget last step
#如何记录之前的结果 记录整行的信息  而不是单一一个 如何计数
#Delete : 如何利用返回的count.grester2, 还是应该利用sum(mydf[i,j])
#--------------------------------------------------------------------------


###   Columns: STATE, long, lat -  DELETE
usedCar.inUse <- usedCar.inUse[,-c(14:16)]
dim(usedCar.inUse)


############ Import data as UsedCar.inUse without binary transformation
write.csv(usedCar.inUse, file = "/Users/Milkywave/Desktop/Group_project/usedCarUse.csv")
#size: 15.9MB

#-------------------------------- Data Transformation





###  Column - manufacturer: 
head(usedCar.inUse, n = 20)
##Fill N/A to empty cell
head(usedCar.inUse$manufacturer, n = 20)



mytransform_categorical <- function(data, ){}
##Binary variables
uniq_manufacturer = sort(unique(usedCar.inUse$manufacturer, na.rm = FALSE))
uniq_manufacturer
#2: 找出unique value, set as column
manufacturer_data <- usedCar.inUse$manufacturer
manufacturer_UniqList = c()
for (j in 1:length(manufacturer_data)){
  if(!(manufacturer_data[j] %in% manufacturer_UniqList)){
    manufacturer_UniqList = c(manufacturer_UniqList, manufacturer_data[j])
  }
}
print(manufacturer_UniqList)
#3: convert original data to binary matrix
#ncol * nrow???
manufacturer.mtx = matrix(rep(0, length(manufacturer_UniqList)*length(manufacturer_data)), ncol=length(manufacturer_UniqList))
for (i in 1:dim(manufacturer.mtx)[1]){
  for (j in 1:dim(manufacturer.mtx)[2]){
    current_obs = manufacturer_data[i]
    current_category = manufacturer_UniqList[j]
    if (current_category %in% current_obs){
      manufacturer.mtx[i,j] <- 1
    }
  }
}
colnames(manufacturer.mtx) <- manufacturer_UniqList
#rownames(manufacturer.mtx) <- manufacturer_data
print(head(manufacturer.mtx))
colnames(manufacturer.mtx) <- paste("manufa_", colnames(manufacturer.mtx), sep = "")
print(head(manufacturer.mtx))


### Column Condition
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$condition, n = 20)
##Binary variables
uniq_condition = sort(unique(usedCar.inUse$condition, na.rm = FALSE))
uniq_condition
#2: 找出unique value, set as column
condition_data <- usedCar.inUse$condition
head(condition_data)
condition_UniqList = c()
for (j in 1:length(condition_data)){
  if(!(condition_data[j] %in% condition_UniqList)){
    condition_UniqList = c(condition_UniqList, condition_data[j])
  }
}
print(condition_UniqList)
#3: convert original data to binary matrix
condition_mtx <- matrix(rep(0,length(condition_UniqList)*length(condition_data)), ncol = length(condition_UniqList))
condition_mtx

colnames(condition_mtx) = condition_UniqList
for (i in 1:dim(condition_mtx)[1]) {
  for (j in 1:dim(condition_mtx)[2]){
    current_obs = condition_data[i]
    current_catagory = condition_UniqList[j]
    if (current_catagory %in% current_obs){
      condition_mtx[i,j] <-1
    }
  }
}
print(condition_mtx)
colnames(condition_mtx) <- paste("condi_", colnames(condition_mtx), sep = "")
print(head(condition_mtx))




### Column Cylinders
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$cylinders, n = 20)
##Binary variables
uniq_cylinders = sort(unique(usedCar.inUse$cylinders, na.rm = FALSE))
uniq_cylinders
#2: 找出unique value, set as column
cylinders_data <- usedCar.inUse$cylinders
head(cylinders_data)
cylinders_UniqList = c()
for (j in 1:length(cylinders_data)){
  if(!(cylinders_data[j] %in% cylinders_UniqList)){
    cylinders_UniqList = c(cylinders_UniqList, cylinders_data[j])
  }
}
print(cylinders_UniqList)
#3: convert original data to binary matrix
cylinders_mtx <- matrix(rep(0,length(cylinders_UniqList)*length(cylinders_data)), ncol = length(cylinders_UniqList))
cylinders_mtx 

colnames(cylinders_mtx ) = cylinders_UniqList
for (i in 1:dim(cylinders_mtx)[1]) {
  for (j in 1:dim(cylinders_mtx)[2]){
    current_obs = cylinders_data[i]
    current_catagory = cylinders_UniqList[j]
    if (current_catagory %in% current_obs){
      cylinders_mtx[i,j] <-1
    }
  }
}
head(print(cylinders_mtx))
colnames(cylinders_mtx) <- paste("cylin_", colnames(cylinders_mtx), sep = "")
print(head(cylinders_mtx))




###  Column fuel
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$fuel, n = 20)
##Binary variables
uniq_fuel = sort(unique(usedCar.inUse$fuel, na.rm = FALSE))
uniq_fuel
#2: 找出unique value, set as column
fuel_data <- usedCar.inUse$fuel
head(fuel_data)
fuel_UniqList = c()
for (j in 1:length(fuel_data)){
  if(!(fuel_data[j] %in% fuel_UniqList)){
    fuel_UniqList = c(fuel_UniqList, fuel_data[j])
  }
}
print(fuel_UniqList)
#3: convert original data to binary matrix
fuel_mtx <- matrix(rep(0,length(fuel_UniqList)*length(fuel_data)), ncol = length(fuel_UniqList))
fuel_mtx 

colnames(fuel_mtx ) = fuel_UniqList
for (i in 1:dim(fuel_mtx)[1]) {
  for (j in 1:dim(fuel_mtx)[2]){
    current_obs = fuel_data[i]
    current_catagory = fuel_UniqList[j]
    if (current_catagory %in% current_obs){
      fuel_mtx[i,j] <-1
    }
  }
}
head(print(fuel_mtx))
colnames(fuel_mtx) <- paste("fuel_", colnames(fuel_mtx), sep = "")
print(head(fuel_mtx))





###  Column title_status
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$title_status, n = 20)
##Binary variables
uniq_title = sort(unique(usedCar.inUse$title_status, na.rm = FALSE))
uniq_title
#2: 找出unique value, set as column
title_data <- usedCar.inUse$title_status
head(title_data)
title_UniqList = c()
for (j in 1:length(title_data)){
  if(!(title_data[j] %in% title_UniqList)){
    title_UniqList = c(title_UniqList, title_data[j])
  }
}
print(title_UniqList)
#3: convert original data to binary matrix
title_mtx <- matrix(rep(0,length(title_UniqList)*length(title_data)), ncol = length(title_UniqList))
title_mtx 

colnames(title_mtx ) = title_UniqList
for (i in 1:dim(title_mtx)[1]) {
  for (j in 1:dim(title_mtx)[2]){
    current_obs = title_data[i]
    current_catagory = title_UniqList[j]
    if (current_catagory %in% current_obs){
      title_mtx[i,j] <-1
    }
  }
}
head(print(title_mtx))
colnames(title_mtx) <- paste("title_", colnames(title_mtx), sep = "")
print(head(title_mtx))


### Column transmission
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$transmission, n = 20)
##Binary variables
uniq_transmission = sort(unique(usedCar.inUse$transmission, na.rm = FALSE))
uniq_transmission
#2: 找出unique value, set as column
transmission_data <- usedCar.inUse$transmission
head(transmission_data)
transmission_UniqList = c()
for (j in 1:length(transmission_data)){
  if(!(transmission_data[j] %in% transmission_UniqList)){
    transmission_UniqList = c(transmission_UniqList, transmission_data[j])
  }
}
print(transmission_UniqList)
#3: convert original data to binary matrix
transmission_mtx <- matrix(rep(0,length(transmission_UniqList)*length(transmission_data)), ncol = length(transmission_UniqList))
head(transmission_mtx)

colnames(transmission_mtx ) = transmission_UniqList
for (i in 1:dim(transmission_mtx)[1]) {
  for (j in 1:dim(transmission_mtx)[2]){
    current_obs = transmission_data[i]
    current_catagory = transmission_UniqList[j]
    if (current_catagory %in% current_obs){
      transmission_mtx[i,j] <-1
    }
  }
}
head(print(transmission_mtx))
colnames(transmission_mtx) <- paste("transm_", colnames(transmission_mtx), sep = "")
print(head(transmission_mtx))



### Column Drive
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$drive, n = 20)
##Binary variables
uniq_drive = sort(unique(usedCar.inUse$drive, na.rm = FALSE))
uniq_drive
#2: 找出unique value, set as column
drive_data <- usedCar.inUse$drive
head(drive_data)
drive_UniqList = c()
for (j in 1:length(drive_data)){
  if(!(drive_data[j] %in% drive_UniqList)){
    drive_UniqList = c(drive_UniqList, drive_data[j])
  }
}
print(drive_UniqList)
#3: convert original data to binary matrix
drive_mtx <- matrix(rep(0,length(drive_UniqList)*length(drive_data)), ncol = length(drive_UniqList))
head(drive_mtx)

colnames(drive_mtx ) = drive_UniqList
for (i in 1:dim(drive_mtx)[1]) {
  for (j in 1:dim(drive_mtx)[2]){
    current_obs = drive_data[i]
    current_catagory = drive_UniqList[j]
    if (current_catagory %in% current_obs){
      drive_mtx[i,j] <-1
    }
  }
}
head(print(drive_mtx))
colnames(drive_mtx) <- paste("drive_", colnames(drive_mtx), sep = "")
print(head(drive_mtx))



### Column Size
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$size, n = 20)
##Binary variables
uniq_size = sort(unique(usedCar.inUse$size, na.rm = FALSE))
head(uniq_size)
#2: 找出unique value, set as column
size_data <- usedCar.inUse$size
head(size_data)
size_UniqList = c()
for (j in 1:length(size_data)){
  if(!(size_data[j] %in% size_UniqList)){
    size_UniqList = c(size_UniqList, size_data[j])
  }
}
print(size_UniqList)
#3: convert original data to binary matrix
size_mtx <- matrix(rep(0,length(size_UniqList)*length(size_data)), ncol = length(size_UniqList))
head(size_mtx)

colnames(size_mtx ) = size_UniqList
for (i in 1:dim(size_mtx)[1]) {
  for (j in 1:dim(size_mtx)[2]){
    current_obs = size_data[i]
    current_catagory = size_UniqList[j]
    if (current_catagory %in% current_obs){
      size_mtx[i,j] <-1
    }
  }
}
head(print(size_mtx))
colnames(size_mtx) <- paste("size_", colnames(size_mtx), sep = "")
print(head(size_mtx))



### Column Type
head(usedCar.inUse)
##Fill N/A to empty cell
head(usedCar.inUse$type, n = 20)
##Binary variables
uniq_type = sort(unique(usedCar.inUse$type, na.rm = FALSE))
uniq_type
#2: 找出unique value, set as column
type_data <- usedCar.inUse$type
head(type_data)
type_UniqList = c()
for (j in 1:length(type_data)){
  if(!(type_data[j] %in% type_UniqList)){
    type_UniqList = c(type_UniqList, type_data[j])
  }
}
print(type_UniqList)
#3: convert original data to binary matrix
type_mtx <- matrix(rep(0,length(type_UniqList)*length(type_data)), ncol = length(type_UniqList))
head(type_mtx)

colnames(type_mtx ) = type_UniqList
for (i in 1:dim(type_mtx)[1]) {
  for (j in 1:dim(type_mtx)[2]){
    current_obs = type_data[i]
    current_catagory = type_UniqList[j]
    if (current_catagory %in% current_obs){
      type_mtx[i,j] <-1
    }
  }
}
head(print(type_mtx))
colnames(type_mtx) <- paste("type_", colnames(type_mtx), sep = "")
print(head(type_mtx))




### Column paint_color****
# frequency table on how many times of each color occurs
# pick 5 common colors , rest becomes others
sort(barplot(prop.table(table(usedCar.inUse$paint_color))),decreasing = TRUE)
#Conclusion: 6 common colors white black silver blue grey red  occupies 90% of data

# rest type of colors as OTHER
#if it is not in 6 common colors, append to other


new_color_col <-  c()
for (i in 1 : length(usedCar.inUse$paint_color)){
  if (usedCar.inUse$paint_color[i] %in% c("black","blue","red","white","silver","grey")) {
    new_color_col <- c(new_color_col, usedCar.inUse$paint_color[i])
  } else { 
    new_color_col <- c(new_color_col, "other")
  }
}
print(new_color_col) #return array containing all observations of color

usedCar.inUse$paint_color <- new_color_col


uniq_paint_color = sort(unique(usedCar.inUse$paint_color, na.rm = FALSE))
uniq_paint_color
#2: 找出unique value, set as column
color_data <- usedCar.inUse$paint_color
head(color_data)
color_UniqList = c()
for (j in 1:length(color_data)){
  if(!(color_data[j] %in% color_UniqList)){
    color_UniqList = c(color_UniqList, color_data[j])
  }
}
print(color_UniqList)
#3: convert original data to binary matrix
color_mtx <- matrix(rep(0,length(color_UniqList)*length(color_data)), ncol = length(color_UniqList))
head(color_mtx)

colnames(color_mtx) = color_UniqList
for (i in 1:dim(color_mtx)[1]) {
  for (j in 1:dim(color_mtx)[2]){
    current_obs = color_data[i]
    current_catagory = color_UniqList[j]
    if (current_catagory %in% current_obs){
      color_mtx[i,j] <-1
    }
  }
}
head(print(color_mtx))

colnames(color_mtx) <- paste("Color_", colnames(color_mtx), sep = "")
print(head(color_mtx))




## Final cleansed data set

usedCar_inUse_bind <- cbind(usedCar.inUse, manufacturer.mtx, condition_mtx, cylinders_mtx, fuel_mtx, title_mtx, transmission_mtx, drive_mtx, size_mtx, type_mtx, color_mtx)

usedCar_inUse_bind <- usedCar_inUse_bind[ , c(-3, -4, -5, -6,-8,-9,-10,-11,-12,-13)]

#----------------------   import cleansed data set with binary variables
write.csv(usedCar_inUse_bind, file = "/Users/Milkywave/Desktop/Group_project/usedCar_inUse_bind.csv")

#-------------------------------------------------------------------------------------------------------------------------------------






###---------------------------------------  Data Analysis
##       Multivariate Regression 
usedCarUse.bind <- read.csv("/Users/Milkywave/Desktop/Projects/First_project_usedCare/usedCar_inUse_bind.csv", header = T)
dim(usedCarUse.bind)
head(usedCarUse.bind)
usedCarUse.bind <- usedCarUse.bind [ , -1]
head(usedCarUse.bind)
nrow(usedCarUse.bind)
## Data Quality
#Intercept ~ Conclusion: Avoid from multi-collinearity reality
#Year ~  Price infatuation  
usedCarUse.bind$year <- usedCarUse.bind$year - 1990
#Odometer ~ set as 500 to 300000
usedCarUse.bind <- usedCarUse.bind[usedCarUse.bind$odometer > 500 & usedCarUse.bind$odometer < 300000, ]
usedCarUse.bind.new <- usedCarUse.bind[usedCarUse.bind$odometer < 500, ]

## Partition
usedCarUse.bind.df <- as.data.frame(usedCarUse.bind)
nrow(usedCarUse.bind.df) #142835 rows
set.seed(12345)
training.index <- sample(1:nrow(usedCarUse.bind.df), 0.5*nrow(usedCarUse.bind.df))
length(training.index)


training.dataset.y <- usedCarUse.bind.df[training.index, 1]
training.dataset.x <- usedCarUse.bind.df[training.index, -1]
nrow(training.dataset.x)

rest.data <- usedCarUse.bind.df[-training.index, ]

#sample()：逻辑上讲，是在拿数据抽样，return数据，但这里是在用数据的index抽#样 所以as.numeric(rownames()): as.numeric("string") 
validation.index <- sample(as.numeric(rownames(rest.data)), 0.6*nrow(rest.data))
length(validation.index)

validation.dataset.y <- usedCarUse.bind.df[validation.index, 1]
validation.dataset.x <- usedCarUse.bind.df[validation.index, -1]


test.dataset.y <- usedCarUse.bind.df[-c(validation.index, training.index), 1]
test.dataset.x <- usedCarUse.bind.df[-c(validation.index, training.index), -1]
nrow(test.dataset.x)




### -------------------------------------   Graphic analysis
library(ggplot2)
scatter.smooth(x=usedCarUse.bind$odometer, y=usedCarUse.bind$price, main = "Price ~ Odometer")
ggplot(usedCarUse.bind, aes(x = odometer, y = price)) + geom_point(alpha = 0.3, color = "beige") + geom_smooth() + xlim(0,250000) + ylim(c(0,50000))
usedCarUse.bind <- usedCarUse.bind[usedCarUse.bind$odometer > 500, ]
#conclusion: it increases the data quality.
ggplot(usedCarUse.bind, aes(x = odometer, y = price)) + geom_point(alpha = 0.3, color = "beige") + geom_smooth(method = "lm") + xlim(0,250000) + ylim(c(0,50000)) #plot useful



scatter.smooth(x=usedCarUse.bind$year, y=usedCarUse.bind$price, main = "Price ~ Year")
ggplot(usedCarUse.bind, aes(x = year, y = price)) + geom_point(alpha = 0.3, color = "beige") + geom_smooth(method = "lm") + xlim(c(1990, 2020)) + ylim(c(0,50000)) #plot useful


boxplot(usedCarUse.bind $odometer, main="Odometer", sub=paste("Outlier rows: ", boxplot.stats(usedCarUse.bind $odometer)$out))
boxplot(usedCarUse.bind $year, main="Year", sub=paste("Outlier rows: ", boxplot.stats(usedCarUse.bind $year)$out))
boxplot(usedCarUse.bind $price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(usedCarUse.bind $price)$out))


cor(usedCarUse.bind$odometer, usedCarUse.bind$price) #-0.2744
cor(usedCarUse.bind$year, usedCarUse.bind$price) #0.5534

##--------------------------------------------------------------Base linear model


# Base lm
base.lm <- lm(training.dataset.y ~ . -1, data = training.dataset.x) 
summary(base.lm)


#Base linear model on RMSE
train_rmse <- sqrt(mean(base.lm$residuals^2))
train_rmse #6447.439
r2 = summary(base.lm)$r.squared
r2 #0.8701
adj_r2 = summary(base.lm)$adj.r.squared
adj_r2 #0.8699

#Test Measure
pred_data <- predict(base.lm, test.dataset.x)
test_residual <- test.dataset.y - pred_data
test_residual
test_rmse <- (mean((test_residual)^2))^0.5
test_rmse #6357.742


##Lasso & Ridge
install.packages("glmnet")
library(glmnet)
?glmnet
#alpha = 0 ridge
#alpha = 1 lasso
train_x <- as.matrix(training.dataset.x) #transform data.frame to matrix
train_y <- training.dataset.y

lambdas <- 10^seq(2,-3,-0.1)

ridge_reg = glmnet(train_x, train_y, alpha = 0, family = "gaussian", lambda =  lambdas)
summary(ridge_reg)
#Interpret: no values

test_x = as.matrix(test.dataset.x)
test_y = test.dataset.y
cv_ridge <- cv.glmnet(train_x, train_y, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda
#just see how good the training model is. Then move to test
pred_train<- predict(ridge_reg,newx = train_x, s = optimal_lambda)
train_residual <- pred_train - train_y
train_rmse <- (mean((train_residual)^2))^0.5
train_rmse #6447.447

#valid_data set  to produce RMSE.test
pred_test<- predict(ridge_reg,newx = test_x, s = optimal_lambda)
test_residual <- pred_test - test_y
test_rmse <- (mean((test_residual)^2))^0.5
test_rmse


lasso_reg <- cv.glmnet(train_x,train_y, alpha = 1, family = "gaussian", lambda = lambdas)
lambda_best <- lasso_reg$lambda.min
lambda_best
lasso_model <- glmnet(train_x, train_y, alpha =1, family = "gaussian", lambda = lambda_best)
pred_train <- predict(lasso_model, newx = train_x, s = lambda_best)
train_residual <- pred_train - train_y
train_rmse <- (mean((train_residual)^2))^0.5
train_rmse 

pred_test<- predict(lasso_model,newx = test_x, s = lambda_best)
test_residual <- pred_test - test_y
test_rmse <- (mean((test_residual)^2))^0.5
test_rmse

#Conclusion: it doesn't make any differences
install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(training.dataset.y ~., data = training.dataset.x, mtry = 10, importance = TRUE, ntree = 500)
#                        Predict Y                                        random feature selections (root of total features)                                                                            Important of predictors to be accessed by default TRUE                                      How many trees ()
print(rf_model)
plot(rf_model) 
22601060^0.5  

pred <- predict(rf_model, test.dataset.x)
residual <- pred - test.dataset.y
test_rmse <- (mean((residual)^2))^0.5
test_rmse                  




##Logistic Regression  --- BASELINE
usedCarUse.bind <- read.csv("/Users/Milkywave/Desktop/Projects/First_project_usedCare/usedCar_inUse_bind.csv", header = T)
usedCarUse.bind <- usedCarUse.bind [ , -1]

colnames(usedCarUse.bind)
usedCarUse.bind.df <- as.data.frame(usedCarUse.bind)
nrow(usedCarUse.bind.df)
set.seed(12345)

training.index <- sample(1:nrow(usedCarUse.bind.df), 0.6*nrow(usedCarUse.bind.df))
length(training.index)

training.dataset <- usedCarUse.bind.df[training.index, ]
head(training.dataset)


test_data = usedCarUse.bind.df[-training.index,]
dim(test_data)
test.index <- as.numeric(rownames(test_data))
length(test.index)
test.dataset <- usedCarUse.bind.df[test.index, ]

dim(training.dataset)
dim(test.dataset)

colnames(training.dataset)

training.dataset.X_lr <-training.dataset[,-c(45:51)]
test.dataset.X_lr <- test.dataset[,-c(45:51)]

training.dataset.Y1 <- training.dataset$condi_excellent
test.dataset.Y1 <- test.dataset$condi_excellent

training.dataset.Y2 <- training.dataset$condi_good
test.dataset.Y2 <- test.dataset$condi_good

training.dataset.Y3 <- training.dataset$condi_salvage
test.dataset.Y3 <- test.dataset$condi_salvage

model1 <- glm(training.dataset.Y1~. , data = training.dataset.X_lr, family = binomial(link = "logit"))
model2 <- glm(training.dataset.Y2~. , data = training.dataset.X_lr, family = binomial(link = "logit"))
model3 <- glm(training.dataset.Y3~. , data = training.dataset.X_lr, family = binomial(link = "logit"))

pred_1_prob <- predict(model1, test.dataset.X_lr, type = "response")
pred_2_prob <- predict(model2, test.dataset.X_lr, type = "response")
pred_3_prob <- predict(model3, test.dataset.X_lr, type = "response")

pred_1 <- ifelse(pred_1_prob > 0.5, 1,0)
pred_2 <- ifelse(pred_2_prob > 0.5, 1,0)
pred_3 <- ifelse(pred_3_prob > 0.5, 1,0)


accur2 <- sum(pred_2 == test.dataset.Y2)/length(pred_2)
accur3 <- sum(pred_3 == test.dataset.Y3)/length(pred_3)



## -------------------------------  SOFTMAX
dim(usedCarUse.bind)
head(usedCarUse.bind)
usedCarUse.bind <- usedCarUse.bind [ , -1]
head(usedCarUse.bind)
nrow(usedCarUse.bind)
# ------------------- Partition
usedCarUse.bind.df <- as.data.frame(usedCarUse.bind)
filter <- usedCarUse.bind.df$condi_NA == 0
usedCarUse.bind.df <- as.data.frame(usedCarUse.bind[filter, ])
head(usedCarUse.bind.df)

usedCarUse.bind.df <- usedCarUse.bind.df[ , -49] 
dim(usedCarUse.bind.df)

nrow(usedCarUse.bind.df) #1135299 rows  45-50col
set.seed(12345)
training.index <- sample(1:nrow(usedCarUse.bind.df), 0.5*nrow(usedCarUse.bind.df))
length(training.index)


training.dataset.y <- usedCarUse.bind.df[training.index, 45:50]
training.dataset.x <- usedCarUse.bind.df[training.index, -c(45:50)]
nrow(training.dataset.x)

rest.data <- usedCarUse.bind.df[-training.index, ]

#sample()：逻辑上讲，是在拿数据抽样，return数据，但这里是在用数据的index抽#样 所以as.numeric(rownames()): as.numeric("string") 
validation.index <- sample(as.numeric(rownames(rest.data)), 0.6*nrow(rest.data))
length(validation.index)

validation.dataset.y <- usedCarUse.bind.df[validation.index, 45:50]
validation.dataset.x <- usedCarUse.bind.df[validation.index, -c(45:50)]


test.dataset.y <- usedCarUse.bind.df[-c(validation.index, training.index), 45:50]
test.dataset.x <- usedCarUse.bind.df[-c(validation.index, training.index), -c(45:50)]
nrow(test.dataset.x)


# 把y变回一列
condition <- c()
for (i in 1:dim(training.dataset.y)[1]){
  temp <- training.dataset.y[i,]
  for (j in 1:dim(temp)[2]){
    if (temp[ ,j] == 1) {
      condition <- c(condition,j) 
    }
  }
}
head(condition, 50)
head(training.dataset.y)
# 1: excellent, 2: good, 3:fair, 4: like.new, 5: new, 6: Salvage



condition.test <- c()
for (i in 1:dim(test.dataset.y)[1]){
  temp <- test.dataset.y[i,]
  for (j in 1:dim(temp)[2]){
    if (temp[ ,j] == 1) {
      condition.test <- c(condition.test,j) 
    }
  }
}
head(condition.test)

# Softmax Model 
install.packages("nnet")
library(nnet)
#softmax: multinomial logistic regression
model <- multinom(condition~ ., data = training.dataset.x)
#summary(model)
pred <- predict(model, test.dataset.x) #return: condition

pred_score <- predict(model, test.dataset.x, "probs")
#                                          output: "probs"


accuracy <-sum(pred == as.numeric(condition.test))/length(pred)
accuracy

table(pred, condition.test)
#output interpretation: 斜对角是预测对的，并且真实是对的。而这张表里面3,4,5,6基本上预测对的占比极其小，几乎是0.所以这个模型建的没啥用。
#修改建议：应该搜集更多数据是属于3,4,5,6
#分类里，每类的sample size均衡，更适合logistic regression 和 softmax
#修改建议：丢掉1,2，只针对3,4,5,6建一次模型。












#FOR WINTER BREAK PRACTICE ON LOOP: odometer based on YEAR get average, 补全na

### ------------------------   Multivariate Regression
##Variables Directory
# Y: Price
# X ~ Numerical variables:  Year, Odometer
# X ~ Categorical variables: Condition, Manufactuer, 
##Graphic Analysis


