# Group Project

library(dplyr)
# Path to .csv file
ds = read.csv("./ZIP/7064/NDAP_REPORT_7064.csv", encoding = "UTF-8")
ds_SDP = read.csv("./HBS_Table_No._06___Net_State_Domestic_Product_at_Factor_Cost_-_State-Wise_(at_Constant_Prices).csv")

# Dropping unnecessary or inconsistent Columns
ds <- select(ds, -ROWID)
ds <- select(ds, -State.LGD.Code)
ds <- select(ds, -District.LGD.Code)
ds <- select(ds, -Surface.Water.Station)
ds <- select(ds, -Surface.Water.Station.Latitude)
ds <- select(ds, -Surface.Water.Station.Longitude)
ds <- select(ds, -SourceMonth)
ds$District = gsub("-", " ", as.character(ds$District))

# Dropping Entries with NA SourceYear or District
ds <- ds[!is.na(ds$SourceYear),]
ds <- ds[!is.na(ds$District),]

dyid <- paste(ds$SourceYear,ds$District,sep="_")
ds = cbind(dyid, ds)
ds[, c(6:50)] <- sapply(ds[, c(6:50)], as.numeric)


# Mean of observations (Done)
ds2 <- aggregate(ds[,6:50],by = list(ds$dyid),FUN=mean, na.rm=TRUE)
colnames(ds2)[1] <- "dyid"
ds = ds[,1:5]
ds = ds[!duplicated(ds),]
ds = merge(x = ds, y = ds2, by = "dyid")
rm(ds2)
#View(ds)


# Modifying Dataset with the Net State Domestic Product (SDP)
# Base Year Correction ??
dd = data.frame(SourceYear = c(NA), State = c(NA), SDP = c(NA))

for (i in 1:23){
  for(j in colnames(ds_SDP)){
      if(j == "YEAR"){
        next
      }
      dd[nrow(dd) + 1,] <- list(ds_SDP$YEAR[i], gsub('.', ' ', j, fixed = TRUE), ds_SDP[, j][i])
  }
}

dd <- dd[!is.na(dd$State),]
# View(dd)
rm(ds_SDP);rm(j);rm(i);

# Merging the two datasets
syid <- paste(substring(dd$SourceYear, 1, 4),dd$State,sep="_")
dd = cbind(syid, dd)

syid <- paste(ds$SourceYear, ds$State,sep = "_")
ds = cbind(syid, ds)

# Inner Join or Left Join ??
merged_dataset = merge(x = dd, y = ds, by = "syid", all.y = TRUE)

# Remove syid and Reorder merged_dataset
merged_dataset <- select(merged_dataset, -syid)
dd <- select(dd, -syid)
ds <- select(ds, -syid)
merged_dataset <- select(merged_dataset, -SourceYear.x)
merged_dataset <- select(merged_dataset, -State.x)
colnames(merged_dataset)[4] <- "State"
colnames(merged_dataset)[10] <- "SourceYear"
merged_dataset = merged_dataset[,c(2,3,4,1,5:ncol(merged_dataset))]

rm(dd);rm(ds);rm(dyid);rm(syid);
# View(merged_dataset)

mohanty <- read.csv("./Mohanty et. al. 2016.csv", encoding = "UTF-8")
mohanty <- mohanty[!is.na(mohanty$Gini.Index),]

# To Remove State wise Gini Values
mohanty <- mohanty[!is.na(mohanty$Sr.No.),]
mohanty$District = gsub("-", " ", as.character(mohanty$District))
mohanty$District <- toupper(mohanty$District)

# Inner Join or Left Join ??
merged_dataset = merge(x = mohanty, y = merged_dataset, by = "District", all.y = TRUE)

# Reordering and removing unwanted columns
merged_dataset <- select(merged_dataset, -Sr.No.)
merged_dataset = merged_dataset[,c(3,4,5,1,6,2,7:ncol(merged_dataset))]
rm(mohanty)

# Saving the datasheet in csv format
write.table(merged_dataset, file = "group_10.csv", sep = ",", row.names = F)
View(merged_dataset)

# 5) Detailed Summary of Variables

# Dichlorophenoxyacetic Acid
# No variation in dataset thereby dropping the parameter
#temp_ds = merged_dataset[!is.na(merged_dataset$Dichlorophenoxyacetic.acid),]
#merged_dataset <- select(merged_dataset, -Dichlorophenoxyacetic.acid)

# Silver
# Very liitle variation in dataset thereby dropping the parameter
#temp_ds = merged_dataset[!is.na(merged_dataset$Silver),]
#merged_dataset <- select(merged_dataset, -Silver)

# Aluminium

# Aldrin

# Alkalinity Phenolphthalein

# Arsenic

# Boron

# Biochemical Oxygen Demand

# Cyanide
# No variation in dataset thereby dropping the parameter
#temp_ds = merged_dataset[!is.na(merged_dataset$cyanides),]
#merged_dataset <- select(merged_dataset, -cyanides)

# Carbonate

# Calcium

# Cadmium

#temp_ds = merged_dataset[!is.na(merged_dataset$Cadimum),]
#View(temp_ds)

# Incomplete
Analysis <- function(cname){
  temp <- merged_dataset[!is.nan(merged_dataset$cname)]
  print(mean(merged_dataset$cnmae))
  
  rm(temp)
}

for(i in 8:52){
  # For every column of merged_dataset
  Analysis(colnames(merged_dataset)[i])
}

mds <- merged_dataset[!is.na(merged_dataset$Biochemical.Oxygen.Demand),]
mds <- mds[!is.na(mds$SDP),]

#View(mds)
SDP <- as.numeric(mds$SDP)
EQI <- as.numeric(mds$Biochemical.Oxygen.Demand)

# 6) Summary of the regression model
reg_model = lm(EQI ~ SDP)
summary(reg_model)

#View(merged_dataset)

# 7) Formatting of Plots
par(mfrow=c(3,1))
# EQI vs SDP (Plot 1)
plot(EQI ~ SDP, col = "red", main='EQI vs SDP', xlab='SDP', ylab='EQI')
abline(lm(EQI ~ SDP),col='purple')

# Residual vs SDP
plot(reg_model$residuals ~ SDP , col = 'blue', main='Residual vs SDP', xlab='SDP', ylab='Residual')
abline(lm(reg_model$residuals ~ SDP),col='purple')

# True vs Predicted
plot(reg_model$fitted.values ~ EQI, col = 'green',main='True vs Predicted', xlab='True Value', ylab='Predicted Value')
abline(lm(reg_model$fitted.values ~ EQI),col='purple')

# 8) Histogram of residuals
windows()
hist(reg_model$residuals,main="Histogram of Residuals", xlab = 'Residuals')
paste("Sum of Residuals:", round(sum(reg_model$residuals), 4))

#View(mds)
# 9) Final estimating Model
SDP1 <- SDP
SDP2 <- SDP * SDP
SDP3 <- SDP * SDP * SDP
GINI <- as.numeric(mds$Gini.Index)
reg_model_1 = lm(EQI ~ SDP1 + SDP2 + SDP3 + GINI)
summary(reg_model_1)

rm(mds)