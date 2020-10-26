## Contributions
## Zijun Feng: Identify incorrect records / outliers from the linear relationship between Bodyfat and 1/Density.
## Mengqi Li: Identify incorrect records / outliers from the equality between Adiposity and calculated BMI.
## Xiangyu Wang & Yiqun Xiao: Identify incorrect records / outliers from other variables.


rm(list=ls())

BodyFat = read.csv("../data/BodyFat.csv")
colnames(BodyFat)
summary(BodyFat)

# Bodyfat and 1/Density
par(mfrow=c(1,1))
png("../image/Density_BodyFat.png", width=800, height=800, res=150)
plot(BodyFat[,2],1/BodyFat[,3], xlab = "Bodyfat", ylab = "1/Density", main="Bodyfat and 1/Density", cex=1.2, cex.lab=1.2, pch=19)
abline(lm(1/BodyFat[,3]~BodyFat[,2]),col = "red")
graphics.off()
# identify(BodyFat[,2],1/BodyFat[,3], col = "red") # use identify() to mark points on plot
# As the plot shows, the bodyfat and 1/density of sample 48, 76, 96 don't follow the linear relationship so we delete these points.

# Remove the index column and density column
data = BodyFat[,-c(1,3)]
summary(data)

# BODYFAT
boxplot(data$BODYFAT)
hist(data$BODYFAT)
which(data$BODYFAT<4)
# The bodyfat of sample 172,182 is smaller than 4 which is unreasonable, so we delete them.
which(data$BODYFAT>45)
# The bodyfat of sample 216 is larger than 45 which is unreasonable, so we delete them.


# AGE
boxplot(data$AGE) 
hist(data$AGE)
#It's reasonable.


# WEIGHT
boxplot(data$WEIGHT)
hist(data$WEIGHT)
which(data$WEIGHT>350)
# Sample 39 seems unreasonable.


# HEIGHT
boxplot(data$HEIGHT)
hist(data$HEIGHT)
which(data$HEIGHT<40)
data[42,4] = sqrt(703*data[42,]$WEIGHT/data[42,]$ADIPOSITY)
# The height for sample 42 is 29.5 inches which is impossible.
# Considering that BMI = 703 * weight(lbs) / height^2(inches) and its BMI and Weight seem reasonable,
# we replace the height for sample 42 by calculating height = sqrt(703 * weight(lbs) / BMI).


# ADIPOSITY
boxplot(data$ADIPOSITY) 
hist(data$ADIPOSITY)
which(data$ADIPOSITY>45)
# Sample 39 

# ADIPOSITY
Calculated_BMI = 703 * data$WEIGHT/data$HEIGHT^2 # Use height and weight of the original dataset to calculate BMI
png("../image/BMI_Adiposity.png", width=800, height=800, res=150)
plot(data$ADIPOSITY,Calculated_BMI, xlab="Adiposity", ylab="Weight/Height^2 (kg/m^2)", main="Consistency of Adiposity (BMI)", cex=1.2, cex.lab=1.2, pch=19)
abline(lm(Calculated_BMI~data$ADIPOSITY), col="red")
graphics.off()
# identify(data$ADIPOSITY, Calculated_BMI, col="red") # use identify() to mark points on plot
# As the plot shows, comparing the calculated BMI and adiposity from original data, 
# there are two potential outliers and we can't tell which data is incorrect(height, weight, adiposity),
# so we delete these two points (sample 163, 221).

# NECK
boxplot(data$NECK)
hist(data$NECK)
which(data$NECK>50)
# Sample 39


# CHEST
boxplot(data$CHEST)
hist(data$CHEST)
which(data$CHEST>130)
# Sample 39


# ABDOMEN
boxplot(data$ABDOMEN)
hist(data$ABDOMEN)
which(data$ABDOMEN>140)
# Sample 39


# HIP
boxplot(data$HIP)
hist(data$HIP)
which(data$HIP>140)
# Sample 39


# THIGH
boxplot(data$THIGH)
hist(data$THIGH)
which(data$THIGH>80)
# Sample 39


# KNEE
boxplot(data$KNEE)
hist(data$KNEE)
which(data$KNEE>47)
# Sample 39


# ANKLE
boxplot(data$ANKLE)
hist(data$ANKLE)


# BICEPS
boxplot(data$BICEPS)
hist(data$BICEPS)
which(data$BICEPS>40)
# Sample 39


# FOREARM
boxplot(data$FOREARM)
hist(data$FOREARM)
# It's reasonable.


# WRIST
boxplot(data$WRIST)
hist(data$WRIST)
# It's reasonable.



# Based on the results above, sample 39 is an outlier so we delete it.
# In conclusion, we delete sample 39,48,76,96,172,182,163,216,221 and replace the height for sample 42 by calculating height = sqrt(703 * weight(lbs) / BMI).
data_clean = data[-c(39,48,76,96,172,182,163,216,221),]
dim(data_clean)
summary(data_clean)
write.csv(data_clean, "../data/BodyFat_clean.csv", row.names=F)

# Remove Rplots.pdf
fn <- "Rplots.pdf"
if (file.exists(fn))
  file.remove(fn)
