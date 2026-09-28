#1
install.packages("olsrr")
install.packages("lmSubsets")
install.packages("car")
library("ggplot2") # data visualization
library("readxl") # import excel files
library('olsrr')
library('lmSubsets')
library(car)

Q1Input <- read_excel("InsInnov.xlsx")
ggplot(data = Q1Input, aes(x = X, y = Y, col = Type))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#c
model2 <- lm(Y~X + Type,data = Q1Input)
summary(model2)

#d
confint(model2, level =.95)

#e
model3 <- lm(Y~X* Type ,data = Q1Input)
summary(model3)


#2.
#a
Q2Input <- read_excel("Basketball.xlsx")
ggplot(data = Q2Input, aes(x = Time, y = Height))+
  geom_point() 
#b
#a quadratic regression model

#c
Q2Input$Time2 <- Q2Input$Time^2
model_quadratic <- lm(Height ~ Time+ Time2, data = Q2Input)
summary(model_quadratic)

#d

#e
coef <- coef(model_quadratic)

roots <- polyroot(c(coef[1], coef[2], coef[3]))
roots

#the time that the basketball will hit the ground at roughly 0.54 seconds
#3
Q3Input <- read_excel("commodities.xlsx")
Y <- Q3Input$'Y=All Commodities'
X1 <- Q3Input$'X1=Food'
X2 <- Q3Input$'X2=Shelter'
X3 <- Q3Input$'X3=Apparel'
X4 <- Q3Input$'X4=Fuel Oil'
Q3Model <- lm(Y ~ X1+ X2+ X3+ X4, data=Q3Input)
summary(Q3Model)



#b
#yes backward elmination requires the removal of independent variables with higher P-values 
#than Alpha stay after fitting the regression model, with an alpha stay of 0.1, and based on the summary of the 
#regression model that we have, we should remove X3, which is X3= Apperal, 
#as its p value is 0.95 which higher than the alpha stay

#c
#after removing apperal we evaluate if there are any other variables hspould be removed based on their Pvalue
#since there are none we have arrived at our final model 


#d
ols_step_backward_p(Q3Model ,prem=.10)
#X3 is removed, X1, X2, X4 remain

#e
ols_step_both_p(Q3Model, penter=.1, prem=.1)
#we start off with no independent variable, similar to forward selection
#X1 is added first, followed by X4 and then X2
#if variables are less than 0.10 (Alpha) stay in the model otherwise they are removed 
#final model contains X1, X2 and X4

#f
#the independent variables in the stepwise regression are X1, X2, AND X4 the same as the backward elimination method 

#g
#X1 is the most important one in the final model as it has the lowest P-value

#h 
intModelAll <- lmSubsets(Y ~ X1 +X2 +X3 + X4, data=Q3Input)
intModelAlloutput <- summary(intModelAll)
intModelAlloutput

bestFull <- lmSelect(intModelAll)
bestFull

full <- refit(bestFull)
summary(full)
#h
#I would pick the model with X1, X2 and X4, this is based on the best subset selection criterion
#i 
#I would pick X1 = Food and X4 = Fuel as they have the lowest P-values, as they have the highest and most significant correlations 

#j 
vif(full)
#since the Vif values are less than 5, there is a weak correlation between variables
#therefore there are no serious multicollinearities present 

#k
model_k <- lm(Y ~ X1 + X3 + X4, data = Q3Input)
hatvalues(model_k)
rstudent(model_k)
cooks.distance(model_k)

#4

#a 
#forward selection 
#for forward selection, we first pick a significance level to assign to alpha enter 
#we start with no independent variables (X variables and their values) and fit the regression with no independent variables associated to it
# we then asses the variables with the lowest p values, less than the value of alpha enter and add that to the model
#we refit the model and add the next lowest value less than alpha enter, until all the values that meet that condition have been added to the model 


#Backward elimination 
#we select a signifiance level to assign to alpha stay 
#we first fit the regression models with all the independent variables in our data set 
#we asses the independent variables with the highest p-values greater than alpha stay
#we remove those variables and refit the regression model,
#we keep repeating the procedure of removing the independent variables with p-values greater than alpha stay 
#till we have no independent variables with p-values greater than alpha stay 

#Stepweise regression

#5
#a
#the partial F test is used to test significant of a subset of independent variables, it is useful when we have a largeamount of predictors, as it allows to us test a few. 
#the test allows us to see if any additional predictor variabes will be better for the models fit compared to if we remove predictors

#b
SSA <- 500
SSB <- 520
n <- 100
k_a <- 5
k_b <- 3
Fpartial <- ((SSB - SSA)/(k_a-k_b)) / (SSB/(n-(k_a+1)))
Fpartial

qf(0.05,2,94, lower.tail =FALSE)