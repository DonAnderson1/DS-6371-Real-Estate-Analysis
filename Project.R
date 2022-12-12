library(tidyverse)
library(car)
library(leaps)
library(MASS)
library(Metrics)
train <- read.csv('train.csv')
test <- read.csv('test.csv')
dim(train)
dim(test)


train %>%
  count(Neighborhood)


train_set <- train %>% filter(Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))
test_set <- test %>% filter(Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))


p1 <-ggplot(data = train_set, aes(GrLivArea, SalePrice)) +
  geom_point() 

p2 <- ggplot(data = train_set, aes(GrLivArea, SalePrice, color = Neighborhood)) +
  geom_point() 
ggplot(data = train_set, aes(GrLivArea, SalePrice, color = Neighborhood)) +
  geom_point()

p3 <- ggplot(data = train_set, aes(GrLivArea, SalePrice, color = Neighborhood)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

p4 <- ggplot(data = train_set, aes(SalePrice, Neighborhood, fill = Neighborhood)) +
  geom_boxplot() 
p4
ggplot(train_set, aes(SalePrice))+
  geom_histogram(bins = 50, fill = 'lightyellow', color = 'black') +
  ggtitle('Distribution of Sale Prices of Ames Houses') 

gridExtra::grid.arrange(p1, p2, p3, p4 , nrow = 2, ncol = 2)
cor(train_set$SalePrice, train_set$GrLivArea)

cor.test(train_set$SalePrice, train_set$GrLivArea)


price_fit1 <- lm(SalePrice ~ GrLivArea, data = train_set)
summary(price_fit1)

price_fit2 <- lm(SalePrice ~ GrLivArea + Neighborhood, data = train_set)
summary(price_fit2)
anova(price_fit1, price_fit2)


price_fit3 <- lm(SalePrice ~ GrLivArea*Neighborhood, data = train_set)
summary(price_fit3)
anova(price_fit2, price_fit3)

confint(price_fit3)
plot(price_fit3)





ncvTest(price_fit3)

vif(price_fit3)

outlierTest(price_fit3)

plot(price_fit3, 4)




## Q2
train <- train %>%
  select_if(~ !any(is.na(.)))

set.seed(123)
forward_model <- regsubsets(SalePrice ~ ., data = train, nvmax = 20, method = "forward")

plot(summary(forward_model)$bic, xlab="Number of Var.", ylab = "BIC")
coef(forward_model, which.min(summary(forward_model)$bic))

fit_forward <- lm(SalePrice ~ MSSubClass + LotArea + LotShape + Neighborhood + 
                    Condition1 + Condition2 + BldgType + OverallQual +
                    OverallCond + YearBuilt + RoofMatl + Exterior2nd + BsmtFinSF2 +
                    FullBath + FullBath + KitchenQual + PavedDrive + SaleCondition, 
                  data = train)


summary(fit_forward)

plot(fit_forward)

ncvTest(fit_forward)

vif(fit_forward) 
outlierTest(fit_forward)

plot(fit_forward, 4)

set.seed(123)
backward_model <- regsubsets(SalePrice ~ ., data = train, nvmax = 20, method = "backward")
plot(summary(backward_model)$bic, xlab="Number of Var.", ylab = "BIC")
coef(backward_model, which.min(summary(backward_model)$bic))

fit_backward <- lm(SalePrice ~ LotArea + Neighborhood + Condition2 + OverallQual + RoofMatl + 
                    OverallCond + YearBuilt + BsmtFinSF2 +LowQualFinSF+
                  BsmtFullBath + KitchenQual + Functional + SaleCondition, data  = train)
summary(fit_backward)

par(mfrow = c(2, 2))
plot(fit_backward)

ncvTest(fit_backward)

vif(fit_backward) 
outlierTest(fit_backward)

plot(fit_backward, 4)

set.seed(123)
best_model <- regsubsets(SalePrice ~ . , data = train, nvmax = 20, method = "seqrep")

plot(summary(best_model)$bic, xlab="Number of Var.", ylab = "BIC")
coef(best_model, which.min(summary(best_model)$bic))

fit_best <- lm(SalePrice ~ MSSubClass + LotArea + LotShape + Neighborhood + Condition1 +
                 Condition2 + BldgType + OverallQual + OverallCond + YearBuilt +
                 RoofMatl + Exterior2nd + BsmtFinSF2 + FullBath + HalfBath +
                 KitchenQual + PavedDrive + SaleCondition, data = train)
summary(fit_best)

par(mfrow = c(2, 2))
plot(fit_best)

ncvTest(fit_best)

vif(fit_best) 
outlierTest(fit_best)

plot(fit_best, 4)


custom_fit <- lm(SalePrice ~ OverallCond + YearBuilt + YearRemodAdd + BedroomAbvGr + GrLivArea + KitchenAbvGr + TotRmsAbvGrd + GarageCars + PoolArea + LotArea, data = train)

summary(custom_fit)
confint(custom_fit)


par(mfrow = c(2, 2))
plot(custom_fit)

ncvTest(custom_fit)

vif(custom_fit) 
outlierTest(custom_fit)

plot(custom_fit, 4)

custom_Ajdsuted_R2 <- 0.737
best_Ajdusted_R2 <- 0.817
Backward_Adjusted_R2 <- 0.7865
Forward_Adjusted_R2 <- 0.812

data.frame(`Adjusted R2` = c(custom_Ajdsuted_R2, best_Ajdusted_R2, Backward_Adjusted_R2, Forward_Adjusted_R2), 
           row.names = c('Custom', 'Stepwise', 'Backward', 'Forward')) %>%
  knitr::kable()
