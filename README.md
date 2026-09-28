# commodity-price-change-analysis-
### This project uses R to analyze U.S. Consumer Price Index and identify key predictors of changes in overall commodity prices. Regressions used are  Applied backward elimination, stepwise regression, and best-subset selection to compare models and identify a final model using Food, Shelter, and Fuel Oil CPI changes. Evaluated model performance and reliability using adjusted R², VIF, leverage, studentized residuals, and Cook’s distance.


Q1Input <- read_excel("InsInnov.xlsx")
ggplot(data = Q1Input, aes(x = X, y = Y, col = Type))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
