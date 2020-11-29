#splitting the dataset 


set.seed(42)

index <- sample(1:nrow(df3), size =  nrow(df3) * 0.8)
train <- df3[index,]
test <- df3[-index,] 

folds <- createFolds(df3$Saleprice, 10)
str(folds)

n <- 10

errors <- numeric(n)




r2_1 = 1 - mean((y-pred1)^2)/mean((y-mean(y))^2) 

#Playing with removing varibles 
#Land contour, land slope, functional, electrical, roof matl, exteriors 1 & 2, foundation, Neighborhood, lot shape, lot config, 
#Bsmt fin type 2 & square footage, central air, alley, paved driveway, fireplaces & fireplace quality, heating & QC, street and lowqual sf, 
#kitchen above grade


model_null <- lm(SalePrice ~ BldgType, data = train)

test_model <- lm(SalePrice ~ BldgType + HouseStyle + RoofStyle + 
                   MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                   BsmtQual + BsmtCond + BsmtFinType1 + 
                   BsmtFinSF1 + BsmtUnfSF  + 
                   X1stFlrSF + X2ndFlrSF + 
                   BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + 
                   BedroomAbvGr + KitchenQual  + 
                   GarageType + GarageFinish + GarageArea + 
                   GarageQual + GarageCond + WoodDeckSF + OpenPorchSF + 
                   EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + 
                   Fence + MiscFeature + MiscVal + was_remodeled + non_lvingArea, data = train)


#AIC too forgiving, not worth using 
step_testing8 <- stepAIC(test_model, direction = 'both')
summary(step_testing8)

step_testing7 <- step(test_model, k = log(2500)) 
summary(step_testing5)  

step_testing6 <- step(model_null, scope = list(lower = model_null, upper = test_model), direction = 'both', k = log(2500)) 
summary(step_testing6)


#good results 
ctrl <- trainControl(method = "cv", number = 10)

lm_modelCV <- train(SalePrice ~ BldgType + BsmtQual + X1stFlrSF + HouseStyle + 
                       ExterQual + GarageCond + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                       non_lvingArea + GarageFinish + GarageArea + BsmtFinSF1 + 
                       ExterCond + WoodDeckSF + FullBath + HalfBath, data=train, method = "lm", trControl = ctrl)
lm_modelCV$results



model_1 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
                        ExterQual + GarageCond + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                        non_lvingArea + GarageFinish + GarageArea + BsmtFinSF1 + 
                        ExterCond + WoodDeckSF + FullBath + HalfBath + Neighborhood + Fireplaces + HeatingQC + was_remodeled + 
                        MasVnrArea + BsmtUnfSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC , data=train)

cv_testmodel <- train(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
                        ExterQual + GarageCond + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                        non_lvingArea + GarageFinish + GarageArea + BsmtFinSF1 + 
                        ExterCond + WoodDeckSF + FullBath + HalfBath + Neighborhood + Fireplaces + HeatingQC + was_remodeled + 
                        MasVnrArea + BsmtUnfSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC, data=train, method = "lm", trControl = ctrl)
cv_testmodel$results 

#trying a verson without pool quality, results from previous modelwere much lower
cv_testmodel1 <- train(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
                        ExterQual + GarageCond + GarageQual + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                        non_lvingArea + GarageFinish + GarageArea + BsmtFinSF1 + 
                        ExterCond + WoodDeckSF + FullBath + HalfBath + Neighborhood + Fireplaces + HeatingQC + was_remodeled + 
                        MasVnrArea + BsmtUnfSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea, data=train, method = "lm", trControl = ctrl)

cv_testmodel1$results

prac_model <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
     ExterQual + GarageCond + GarageQual + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
     non_lvingArea + GarageFinish + GarageArea + BsmtFinSF1 + 
     ExterCond + WoodDeckSF + FullBath + HalfBath + Neighborhood + Fireplaces + HeatingQC + was_remodeled + 
     MasVnrArea + BsmtUnfSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea, data=train)

#====================================================================
  
 

prac_model2 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
                     ExterQual + GarageType + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                     area_ratio + GarageFinish + GarageArea + BsmtFinSF1 + 
                     ExterCond + WoodDeckSF + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC + was_remodeled + MasVnrArea + 
                     BsmtUnfSF , data=train) 

prac_model3 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle  + 
                   ExterQual + GarageType + X2ndFlrSF + KitchenQual +
                   GarageFinish + GarageArea + BsmtFinSF1 + BsmtFullBath + BsmtHalfBath +
                   ExterCond + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC + BsmtUnfSF, data=train) 


prac_model4 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + GarageType + 
                    ExterQual + GarageType + X2ndFlrSF + KitchenQual +
                    GarageFinish + GarageArea + BsmtFinSF1 + BsmtFullBath + BsmtHalfBath +
                    ExterCond + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC + BsmtUnfSF, data=train) 

prac_model5 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + X2ndFlrSF + GarageType + 
                    ExterQual + GarageType + KitchenQual +
                    GarageFinish + GarageArea + BsmtFinSF1 + BsmtFullBath + BsmtHalfBath +
                    ExterCond + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC + BsmtUnfSF, data=train1) 

prac_model6 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + X2ndFlrSF  + 
                    ExterQual + GarageType + KitchenQual +
                    GarageFinish + GarageArea + BsmtFinSF1 + BsmtFullBath + BsmtHalfBath +
                    ExterCond + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC, data=train) 



step_test1 <- step(prac_model6, k = log(2030))

step_test2 <- step(prac_model6, k = 2)


D <- df %>% 
  group_by(HouseStyle, GarageType) %>% 
  summarize(totals = n(), 
            med_price = median(SalePrice))

df %>% 
  group_by(GarageType) %>% 
  summarize(totals = n())




#======================
ctrl <- trainControl(method = "cv", number = 10)

lm_modelCV <- train(SalePrice ~ BsmtQual + X1stFlrSF + HouseStyle + 
                      ExterQual + GarageCond + BsmtFinType1 + X2ndFlrSF + KitchenQual + 
                      area_ratio  + GarageArea + BsmtFinSF1 + 
                      ExterCond + WoodDeckSF + FullBath + HalfBath + neigh_group + Fireplaces + HeatingQC + was_remodeled + MasVnrArea + 
                      BsmtUnfSF , data=train, method = "lm", trControl = ctrl) 

lm_modelCV$results

#Answering Aiko's first question, 
#Based on the data and the model, ranch-style homes(single-level and splits) seem to be more popular than colonial style(2 story houses) homes
#regarding price coeffiecents, overall, the ranch style homes are higher than 




ctrl <- trainControl(method = "cv", number = 10)

#base model from BIC selection criteria 
CV_results <-  train(SalePrice ~ BldgType + neigh_group + X1stFlrSF + 
     HouseStyle + ExterQual + BsmtFinType1 + GarageFinish + X2ndFlrSF + 
     KitchenQual, data = train, method = 'lm', trControl = ctrl)


#trying to finalize model 
#After first run, remove BsmtUnf and added MiscFeature & TotalBsmtSF 
#remove housestyle second pass 
#removed TotalBSF and added Garage Condition and rooms above ground  
#removed garage condition 

#removing the extreme outlier  

train1 <- train[-728, ]
train2 <- train[c(-728, -579, -1338), ]

prac_model3 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + 
                    ExterQual + GarageQual + X2ndFlrSF + KitchenQual + 
                    ExterCond + WoodDeckSF + FullBath + HalfBath + Fireplaces + HeatingQC + was_remodeled + TotRmsAbvGrd +
                    MasVnrArea + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + area_ratio + neigh_group + MiscFeature, data=train1)  

prac_model4 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + 
                    ExterQual + GarageQual + X2ndFlrSF + KitchenQual + 
                    ExterCond + WoodDeckSF + FullBath + HalfBath + HeatingQC + was_remodeled + TotRmsAbvGrd +
                    MasVnrArea + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + area_ratio + neigh_group + MiscFeature, data=train2)  


prac_null <-lm(SalePrice~BsmtQual, data = train2)


step_test8 <- step(prac_null, scope = list(lower = prac_null, upper = prac_model3), direction = 'both', k = log(1997))
step_test9 <- step(prac_null, scope = list(lower = prac_null, upper = prac_model3), direction = 'both', k = 2) 

step_test10 <- step(prac_null, scope = list(lower = prac_null, upper = prac_mod1), direction = 'both', k = log(1997))  
step_test11 <- step(prac_null, scope = list(lower = prac_null, upper = prac_mod1), direction = 'both', k = 2) 


ctrl <- trainControl(method = "cv", number = 10)


CV_results <-  train(SalePrice ~ BsmtQual + X1stFlrSF + X2ndFlrSF + neigh_group + 
                       KitchenQual + Fireplaces + ExterQual + GarageQual + HeatingQC + 
                       area_ratio + HalfBath + ExterCond + MasVnrArea + WoodDeckSF + 
                       FullBath + ScreenPorch + was_remodeled, data = train2, method = 'lm', trControl = ctrl)


prac_mod1 <- lm(SalePrice ~ BsmtQual + X1stFlrSF + X2ndFlrSF + neigh_group + 
                  KitchenQual  + ExterQual + GarageQual + HeatingQC + ExterCond, data = train1)


#making viz 
ggplot(data = df, aes(x = reorder(HouseStyle, SalePrice, FUN = median), y = SalePrice)) + 
  +   geom_boxplot()

ggplot(data = df, aes(x = reorder(HouseStyle, GrLivArea, FUN = median), y = GrLivArea)) + 
  geom_boxplot()

D <- lm(SalePrice ~ BsmtQual + X1stFlrSF + X2ndFlrSF + neigh_group + 
     KitchenQual  + ExterQual + GarageQual + HeatingQC + ExterCond, data = train) 
summary(D)

houseprices %>% 
  mutate(mortgage_est = (SalePrice / 180)) %>% 
  select(mortgage_est) %>% 
  summary()

df1 <- df1 %>% 
  mutate(salary_min = (12*mortgage_est*100)/30) 

ggplot(data = df1, aes(x = reorder(HouseStyle, salary_min, FUN = median), y = salary_min)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 40000)

house