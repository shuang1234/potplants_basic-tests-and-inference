pot_plants <- read.csv("/Users/qiushuang/Desktop/potplants_MT5762.csv")
# Select Ti, Ca, Ga, Ba, Zn to analyse
pot_plants <- as.data.frame(pot_plants[,c("Sample.Name", "Group", 
                                          "Ti", "Ca","Ga", "Ba", "Zn")])
# Code for Q1
attach(pot_plants)
table(Group)

# Compute means for each group
mean <- lapply(pot_plants[3:7], function(x){
  aggregate(x, by = list(Group), FUN = mean)})
mean

# Compute sds for each group
sd <- lapply(pot_plants[3:7], function(x){
  aggregate(x, by = list(Group), FUN = sd)})
sd

# Test the assumption with qqnorm, qqline and shapiro.test
install.packages("car")
library("car")
lapply(pot_plants[3:7], function(x){
  weedANOVA <- aov(x ~ Group, pot_plants)
  qqnorm(weedANOVA$residuals)
  qqline(weedANOVA$residuals)
  shapiro.test(weedANOVA$residuals)})

# Summarise outputs of aov function
sumANOVA <- lapply(pot_plants[3:4], function(x){
  summary(aov(x ~ Group, pot_plants))})
sumANOVA

# Multiple comparisons by TukeyHSD
lapply(pot_plants[3:4], function(x){
  TukeyHSD(aov(x ~ Group, pot_plants))})

lapply(pot_plants[3:4], function(x){
  par(las = 2)
  plot(TukeyHSD(aov(x ~ Group, pot_plants)))})

# Using non-parametric test -- Kruskal-Wallis instead of ANOVA
#   to analyse Zn
kruskal.test(Zn ~ Group, pot_plants)

# Get wmc() fuction, it is a non-parametric fuctioncan that can 
#   compare difference between each pair
# wmc(y ~ A, data, method)
#   y is numeric value
#   A is grouping value
#   data is the dataframe which contain y and A
#   method is the method that can control type-1 error
# Use it instead of TukeyHSD to compare each pair about Zn
source("http://www.statmethods.net/RiA/wmc.txt")
wmc(Zn ~ Group, data = pot_plants, method = "holm")


# Code for Q2
# Calculate the Pearson correlation coefficient between elements
cor(pot_plants[3:7], method = "pearson")

# Test the significance of correlation
cor.test(pot_plants$Ga, pot_plants$Ba)
cor.test(pot_plants$Ga, pot_plants$Zn)
cor.test(pot_plants$Ba, pot_plants$Zn)

# Make plots to visualize the correlation
library(ggplot2)
ggplot(pot_plants) + 
  geom_point(mapping = aes(x = pot_plants$Ba, y = pot_plants$Ga))
ggplot(pot_plants) + 
  geom_point(mapping = aes(x = pot_plants$Ba, y = pot_plants$Zn))
ggplot(pot_plants) + 
  geom_point(mapping = aes(x = pot_plants$Ga, y = pot_plants$Zn))

# Code for Q3
# Make boxplots for Ti and Ca to see the ranges
library(ggplot2)
ggplot(pot_plants) + 
  geom_boxplot(mapping = aes(x = Group, y = pot_plants$Ti))
ggplot(pot_plants) + 
  geom_boxplot(mapping = aes(x = Group, y = pot_plants$Ca))
