#SCRIPTS

library(car)

#Changes working directory to your desktop. Make sure to copy/paste the two data files to your desktop
setwd("~/Desktop")

#Read in the first file. This is fun!
dat=read.csv('data1_exp70_attentionANDmcdi.csv', header=T)

#Let's check out the first few rows of our dataset. 
#This is a good way to check that you (1) loaded in the correct file and (2) the column headers are correct. 
head(dat)

# Independent Samples t-test ----------------------------------------------

#Question: Does candy consumption impact the amount of sustained
#attention an infant produces? 

#Let's subset our data into 'high' and 'low' candy consumption
y1 <- dat[ which(dat$candy_consumption=='high'),]
y2 <- dat[ which(dat$candy_consumption=='low'),]

#Are both vars normally distributed?
shapiro.test(y1$prop_SA_objects)
shapiro.test(y2$prop_SA_objects)
hist(y1$prop_SA_objects,10)
hist(y2$prop_SA_objects,10)

t.test(y1$prop_SA_objects,y2$prop_SA_objects,paired=FALSE)#where y1 and y2 are numeric

#Wow! Give me some candy!

# Dependent Samples t-test ------------------------------------------------
  #ASSUMPTION

#Do infants spend more time looking at the target object or any other object when
#an object utterance is produced? 

#Question: 

#proportion of 'prop_ceye_outofNaming_objects' (out of naming utterance spent looking at any object) v. 'prop_ceye_outofNaming_target'
#(out of naming spent looking at target )

y1 <- dat$prop_ceye_outofNaming_objects
y2 <- dat$prop_ceye_outofNaming_target

#Is the difference between y1 and y2 normally distributed?
shapiro.test(y1-y2)
hist(y1-y2)

t.test(y1,y2,paired=TRUE)#where y1 and y2 are numeric

#Yes!

# Correlation -------------------------------------------------------------
#TWO VARIABLES

#Is sustained attention correlated with MCDI? 

y1 <- dat$prop_SA_objects
y2 <- dat$MCDI_12mo_CompTotal

#Are both variables normally distributed?
shapiro.test(y1)
shapiro.test(y2)
hist(y1)
hist(y2)

#if so,
cor.test(y1,y2,method="pearson")

#if not normally distributed
#cor.test(y1,y2,method="spearman")

  #to get the scatterplot:
plot(y1,y2,
     xlab="Sustained Attention (Proportion)",
     ylab="12 month MCDI", 
     main="Correlation fun!",
     pch=16,
     cex.lab=1.2,cex.axis=1.2,
     bty="n")
  #add regression line to scatterplot if both vars are normally distributed  
model1=lm(y2~y1)
abline(model1)
  
# Linear Regression -------------------------------------------------------

#What are the contributions of other variables to the prediction of MCDI, 
#above and beyond sustained attention? 

#Proportion of sustained attention
x1 <- dat$prop_SA_objects

#Proportion of total object looking
x2 <- dat$prop_ceye_objects

#Proportion of total looking at target when object utterance is produced by parent
x3 <- dat$prop_ceye_outofNaming_target

#MCDI at 12 months
y <- dat$MCDI_12mo_CompTotal

#Let's construct a model! Woohoo!
model1=lm(y~x1+x2+x3)
  
#ASSUMPTIONS (model diagnostics):
outlierTest(model1)#test for outliers
ncvTest(model1)#test for equal variances. If p-value is less than 0.05, then not equal variances
qqPlot(model1)#visually inspect normality of residuals. 
hist(residuals(model1))#visually inspect normality of residuals
shapiro.test(residuals(model1))#test for normality of residuals. If p-value is less than 0.05, then non-normal residuals
plot(residuals(model1))#visually inspect if residuals are random. 
vif(model1)#in case of multiple x variables, is there multicollinarity? If VIF is above 5, indicates there may be multicollinearity

#Remedy to outlier. Let's fix this! Row 9 is causing outlier test to be significant. Let's take out. Yes!
dat.out=dat[-9, ]
x1.out <- dat.out$prop_SA_objects
x2.out <- dat.out$prop_ceye_objects
x3.out <- dat.out$prop_ceye_outofNaming_target
y.out <- dat.out$MCDI_12mo_CompTotal
model1.out=lm(y.out~x1.out+x2.out+x3.out)
summary(model1)
summary(model1.out)
outlierTest(model1.out)#test for outliers

#If all assumptions are met:
summary(model1)
anova(model1)

  
# Linear Mixed Effects Models ------------------------------------------------

# Clear workspace
rm(list = ls())

# Attach required libraries to run the analyses
library(lme4)
library(lmerTest)
library(lsmeans)

#Read in data. Data should be in long format: rows correspond to individual events within a subject.
#Columns correspond to the variables coded for each event, such as subject ID, condition, etc. 
lmer.dat=read.csv('data2_exp32_71_72_attentionANDpbehaviors.csv', header=T)

head(lmer.dat)

#Section 1: load data, organize data 

#Display names of variables in the data. In the sample data provided there are four variables:
#1.subID -- subject ID that generated each infant look. This is the first random factor. 
#2.duration_of_look -- duration of each infant look. This is the Dependent Variable in the analysis.
#3.object_attended -- category ID of the object attended to by the infant in each look. This is the second random factor
#4.overlapping_pbehaviors -- category of each infant look based on combination of overlapping parent behaviors. There are Five mutually exclusive categories (levels)
#5.prop_of_look_inSmiling -- is the proportion of time during a look that infant is smiling (fake data!)
names(lmer.dat)

#Display the minimum duration of infant looks (or all row) to make sure it is at least 0.5 
min(lmer.dat$duration)

#Treat overlapping_pbehaviors, or the independent variable, as a categorical factor with 5 levels instead of a continuous variable.
lmer.dat$overlapping_pbehaviors=as.factor(lmer.dat$overlapping_pbehaviors)


# Section 2: Model fit1 

#Fit Linear Mixed Model (fit1): 
#Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
#Fixed effect is cat_clook, or the category of infant look based on parent behaviors
#Random effects are: intercepts for subjects and intercept for object_attended
fit1 <- lmer(log(duration_of_look) ~ overlapping_pbehaviors + (1|subID) + (1|object_attended), data = lmer.dat)

#Display output of the model including estimates for random effects and fixed effects. 
summary(fit1)

#Test significance of including fixed effects in the model 
anova(fit1)

#Plot estimated intercepts for the random effects specified: subID and object_attended
lattice::dotplot(ranef(fit1, condVar = TRUE))$subID
lattice::dotplot(ranef(fit1, condVar = TRUE))$object_attended

#Compute least squares means for the categories of infant looks obtained from the model
fit1.lsm <- lsmeans(fit1, ~ overlapping_pbehaviors)

#Display all pairwise comparisons between least squares means of the five categories
pairs(fit1.lsm)

#Plot estimated least squares means of the five categories with 95% confidence intervals around them
plot(lsmeans(fit1, ~overlapping_pbehaviors))

#Plot residuals from the model to look for normality and equal variance
qqnorm(residuals(fit1),main="")
plot(fit1)

# Section 3: Model fit2

#Fit Linear Mixed Model (fit2):
#Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
#There is not a fixed effect specified
#Random effects are: intercepts for subjects and for object_attended
fit2 <- lmer(log(duration_of_look) ~ (1|subID) + (1|object_attended), data = lmer.dat)

#Display output from the model
summary(fit2)

#Compare fit1 and fit2 to test for additional value of fit1, which has overlapping_pbehaviors as a fixed effect
anova(fit1,fit2)


# Section 4: Model fit3 

#Fit Linear Mixed Model (fit 3) with random slopes for infants
#Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
#Fixed effect is overlapping_pbehaviors, or the category of infant look based on parent behaviors
#Random effects are: intercepts for subjects and for object_attended, and slopes for subjects
#NOTE: the line below does not run with the sample data provided, which has only 2 subjects. With enough data, this command should run.
fit3 <- lmer(log(duration_of_look) ~ overlapping_pbehaviors + (1 + overlapping_pbehaviors|subID) +(1|object_attended), data = lmer.dat)

#Display output from the model
summary(fit3)

#Plot estimated slopes for different infants
lattice::dotplot(ranef(fit3, condVar = TRUE))$subID

#Compare fit 1 and fit3, to test for additional value of fit3, which adds random slopes for infants
anova(fit1,fit3)

# Section 5: Some assumptions

#packages
library(lme4)
library(lmerTest)
library(plyr)
library(lattice)
library(lsmeans)
library(car)
library(nlme)


#checking assumptions of linear mixed effects models
#normality of residuals
q1 = qqnorm(residuals(fit1), main="M1- Normal Q-Q Plot")
hist(residuals(fit1))
shapiro.test(residuals(fit1))
plot(fit1)

#homegeneity of variance, can be relaxed...
#see... https://stats.stackexchange.com/questions/123648/residual-diagnostics-and-homogeneity-of-variances-in-linear-mixed-model
leveneTest(lmer.dat$duration_of_look,group =lmer.dat$overlapping_pbehaviors)

# normality of the random coefficients
randef = random.effects(fit1) #$`(Intercept)`
rander_new=unlist(randef, recursive = TRUE, use.names = TRUE)
hist(rander_new, prob=TRUE, col="grey",xlab="Histogram of random effects")
#checking normality of the random coefficients
shapiro.test(rander_new)
