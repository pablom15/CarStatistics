library(tidyverse)
library(dplyr)

# Import mecha car data
Mecha_Car <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset
head(Mecha_Car)

# Scatter plot of Vehicle Length vs MPG to get an idea of the data
plt <- ggplot(Mecha_Car,aes(x=vehicle.length,y=mpg))
plt + geom_point()

# Generate multiple linear regression model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=Mecha_Car)

# Generate summary statistics on multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=Mecha_Car))

# Suspension Coil Summary
Coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset
head(Coil)

# Summary Stats for coil pressure data
summary(Coil$PSI)

# Creates a summary table and view the table
Coil_summary <- Coil %>% summarize(Min=min(PSI),
                                   Max=max(PSI),
                                   Mean=mean(PSI),
                                   Median=median(PSI),
                                   Variance=var(PSI),
                                   StdDev=sd(PSI),
                                   n=n())
View(Coil_summary)

ggplot(Coil,aes(x=PSI)) + geom_density() #visualize distribution using density plot


# Test normality
shapiro.test(Coil$PSI)

# Take a sample to perform t-test
coilSample <- Coil %>% sample_n(100)
ggplot(coilSample,aes(x=PSI)) + geom_density()
mean(coilSample$PSI)

# Perform t-test
t.test(coilSample$PSI, mu=1500, alternative = 'two.sided')



