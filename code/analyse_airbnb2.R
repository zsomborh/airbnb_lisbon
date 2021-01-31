rm(list = ls())

library(tidyverse)
library(data.table)
library(rattle)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(moments)
library(cowplot)

#source(paste0(w_dir,'/code/helper.R'))
source('https://raw.githubusercontent.com/zsomborh/airbnb_lisbon/main/code/helper.R')

w_dir = 'C:/Users/T450s/Desktop/programming/git/airbnb_lisbon'
df <- read_csv('https://raw.githubusercontent.com/zsomborh/airbnb_lisbon/main/data/clean/cleaned_airbnb.csv')

# basic descr stat + EDA  -------------------------------------------
skimr::skim(df)
summary(df$price)
Hmisc::describe(df$price)
describe(df$property_type)


# some data prep

df <- df %>% mutate(
  property_type = as.factor(property_type)
)

colnames(df) <- tolower(colnames(df))


#variable grouping 
target_var = 'price'

basic_vars <- c('property_type', 'accommodates', 'bedrooms', 'beds', 'minimum_nights', 'bathrooms')

host_info <- c('host_since', 'host_is_superhost', 'host_listings_count', 'host_identity_verified')

reviews <- c('number_of_reviews', 'review_scores_rating','reviews_per_month', 'flag_review_scores_rating')

amenities <- colnames(df)[!colnames(df) %in% c('id',target_var, basic_vars, host_info,host_info,reviews)]


# EDA

df %>%
    group_by(property_type) %>%
    dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(df$price)


g3a <- ggplot(data=df, aes(x=price)) +
    geom_histogram(aes(x = price), binwidth = 30, color = 'black', fill = 'navyblue', alpha = .8) +
    labs(x = "Price (EUR)",y = "Count")+
    theme_minimal() 
g3a

g3b <- ggplot(data=df, aes(x=ln_price)) +
    geom_histogram(aes(x = ln_price), binwidth = .2, color = 'black', fill = 'navyblue', alpha = .8) +
    labs(x = "Ln Price (EUR)",y = "Count")+
    theme_minimal() 
g3b


g4 <- ggplot(data = df, aes(x = property_type, y = price)) +
    stat_boxplot(aes(group = property_type), geom = "errorbar", width = 0.3,
                 #color = c(color[2],color[1], color[3]), 
                 size = 0.5, na.rm=T)+
    geom_boxplot(aes(group = property_type),
                 #color = c(color[2],color[1], color[3]), fill = c(color[2],color[1], color[3]),
                 size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
    scale_y_continuous(expand = c(0.01,0.01),limits = c(0,200), breaks = seq(0,200,50)) +
    labs(x = "Property_type",y = "Price (EUR)")+
    theme_minimal()


g4

g5 <- ggplot(df, aes(x = factor(accommodates), y = price,
                        fill = factor(property_type), color=factor(property_type))) +
    geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
    scale_color_manual( name = '', values = c('black', 'black')) +
    scale_fill_manual( name = '', values = c('navyblue', 'purple4')) +
    stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
    labs(x = "Accomodates (Persons)",y = "Price (EUR)")+
    theme_minimal() +
    theme(legend.position = c(0.15,0.8))+
    scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 250), breaks = seq(0,250, 50))
g5


# a few plots to check interactions
p1 <- price_diff_by_variables2(df, "property_type", "coffee_maker", "Property type", 'Coffee Maker')
p2 <- price_diff_by_variables2(df, "property_type", "wifi", "Property type", 'Wifi')
p3 <- price_diff_by_variables2(df, "property_type", "dishwasher", "Property type", 'Dishwasher') # looks good
p4 <- price_diff_by_variables2(df, "property_type", "elevator", "Property type", 'Elevator')
p5 <- price_diff_by_variables2(df, "property_type", "lock_on_bedroom_door", "Property type", 'Lock on bedroom')
p6 <- price_diff_by_variables2(df, "property_type", "microwave", "Property type", 'Microwave')
p7 <- price_diff_by_variables2(df, "property_type", "kitchen", "Property type", 'Kitchen') # looks good
p8 <- price_diff_by_variables2(df, "property_type", "lockbox", "Property type", 'LockBox') # looks good 
p9<- price_diff_by_variables2(df, "property_type", 'luggage_dropoff_allowed', "Property type",'Luggage dropoff')
p10<- price_diff_by_variables2(df, "property_type", 'patio_or_balcony', "Property type", 'Balcony avaialable') # looks good
p11<- price_diff_by_variables2(df, "property_type", 'refrigerator', "Property type", 'Fridge')
p12<- price_diff_by_variables2(df, "property_type", 'self_checkin', "Property type", 'Self Check-in') # looks good 

interaction_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7 ,p8, p9, p10, p11,p12, nrow=3, ncol=4)
interaction_plot


# Feature/target engineering -----------------------------------------------------



#Create log transformations on continuous variables where skewness is above .5 or below -5 - we  consider those to be skewed  

cont_vars <- c('minimum_nights', 'bedrooms','beds','bathrooms', 'host_since', 'host_listings_count', 'review_scores_rating', 'reviews_per_month', 'number_of_reviews')

df <- get_lns(df,cont_vars,0.5)

ln_vars <- colnames(df)[!colnames(df) %in% c(basic_vars,host_info,reviews,amenities,'id', 'price')]
ln_vars

# Create interaction variables - a few handpicked from EDA phase and all amenities + property type 
interactions1 <- c('property_type * dishwasher',
                   'property_type * kitchen',
                   'property_type * lockbox',
                   'property_type * patio_or_balcony',
                   'property_type * self_checkin'
)

interactions2 <- c(paste0('property_type * (',paste(amenities, collapse = '+'),')'))

# Transform outcome variable 

df <- df %>% mutate(
    ln_price = log(price)
)

# correct for ln 

df <- df %>% mutate(
    ln_minimum_nights =       ifelse(is.infinite(ln_minimum_nights),0,ln_minimum_nights),
    ln_bedrooms =             ifelse(is.infinite(ln_bedrooms),0,ln_bedrooms),
    ln_beds=                  ifelse(is.infinite(ln_beds),0,ln_beds),
    ln_bathrooms =            ifelse(is.infinite(ln_bathrooms),0,ln_bathrooms),
    ln_host_listings_count =  ifelse(is.infinite(ln_host_listings_count),0,ln_host_listings_count),
    ln_review_scores_rating = ifelse(is.infinite(ln_review_scores_rating),0,ln_review_scores_rating),
    ln_reviews_per_month =    ifelse(is.infinite(ln_reviews_per_month),0,ln_reviews_per_month),
    ln_number_of_reviews =    ifelse(is.infinite(ln_number_of_reviews),0,ln_number_of_reviews),
    ln_price =                ifelse(is.infinite(ln_price),0,ln_price)
)

for (i in c('ln_price',ln_vars)){
    print(paste0(i,'- no. infs is: ',sum(is.infinite(unlist(df[,i]))), ', no. of NAs is: ', sum(is.na(unlist(df[,i]))), ', no.of NaNs is: ', sum(is.nan(unlist(df[,i])))) )
}

sum(is.nan(df$ln_bathrooms))
#create predictors sets

predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, host_info, reviews, amenities)

transformed <- substring(ln_vars,4)

predictors_transformed_small <- c(predictors_2[!predictors_2 %in% transformed],interactions1,ln_vars)
predictors_transformed_big <- c(predictors_2[!predictors_2 %in% transformed],interactions2,ln_vars)


# Modeling with Lasso + OLS ----------------------------------------------------------------

#  create holdout set 
set.seed(7)

train_indices <- as.integer(createDataPartition(df$price, p = 0.75, list = FALSE))
df_train <- df[train_indices, ]
df_holdout <- df[-train_indices, ]


# train control is 5 fold cross validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# Starting off with simple OLSs 

set.seed(7)
system.time({
  ols_model1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})
set.seed(7)
system.time({
    ols_model2 <- train(
        formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model3 <- train(
        formula(paste0("price ~", paste0(predictors_transformed_small, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})



ols_model_coeffs1 <-  ols_model1$finalModel$coefficients
ols_model_coeffs_df1 <- data.frame(
  "variable" = names(ols_model_coeffs1),
  "ols_coefficient" = ols_model_coeffs1
) %>%
  mutate(variable = gsub("`","",variable))


ols_model_coeffs2 <-  ols_model2$finalModel$coefficients
ols_model_coeffs_df2 <- data.frame(
    "variable" = names(ols_model_coeffs2),
    "ols_coefficient" = ols_model_coeffs2
) %>%
    mutate(variable = gsub("`","",variable))


ols_model_coeffs3 <-  ols_model3$finalModel$coefficients
ols_model_coeffs_df3 <- data.frame(
    "variable" = names(ols_model_coeffs3),
    "ols_coefficient" = ols_model_coeffs3
) %>%
    mutate(variable = gsub("`","",variable))



# Using Lasso

set.seed(7)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),  
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.01)),
    trControl = train_control
  )
})

set.seed(7)
system.time({
    lasso_model2 <- train(
        formula(paste0("price ~", paste0(predictors_transformed_big, collapse = " + "))), 
        data = df_train,
        method = "glmnet",
        preProcess = c("center", "scale"),
        tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.01)),
        trControl = train_control
    )
})


lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`) 

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

lasso_coeffs2 <- coef(
    lasso_model2$finalModel,
    lasso_model2$bestTune$lambda) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    rename(lasso_coefficient = `1`) 

lasso_coeffs_non_null2 <- lasso_coeffs2[!lasso_coeffs2$lasso_coefficient == 0,]

# TODO put everything in one 
regression_coeffs <- merge(ols_model_coeffs_df1,ols_model_coeffs_df2,ols_model_coeffs_df3, lasso_coeffs_non_null,lasso_coeffs_non_null2, by = "variable", all=TRUE)

# Check OLS, Lasso performance: 

temp_models <-
    list("OLS1" = ols_model1,
         "OLS2" = ols_model2,
         "OLS3" = ols_model3,
         "LASSO1 (model w/ few interactions)" = lasso_model,
         "LASSO2 (model w/ all interactions)" = lasso_model2)

result_temp <- resamples(temp_models) %>% summary()

result_temp

result_rmse <- imap(temp_models, ~{
    mean(result_temp$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
    rename("CV RMSE" = ".")

result_holdout <- map(temp_models, ~{
    RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
    rename("Holdout RMSE" = ".")

result_temp
result_rmse
result_holdout

# based on cross validated RMSEs, it makes sense to increase model complexity to some extent, but 
# It doesn't do a lot of improvement on OLS to include interactions/ln transformations


# MOdeling with tree based models  --------------------------------------------------------------------

# TODO: pruning, tuning  

set.seed(7)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "rpart",
    tuneLength = 9,
    trControl = train_control
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")

# Forth is rf

tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(7)
system.time({
  rf_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

# Fifth is GBM

gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(7)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = df_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model

# Final models


final_models <-
  list("OLS3" = ols_model3,
       "LASSO (model w/ all interactions)" = lasso_model2,
       "CART" = cart_model,
       "Random forest" = rf_model,
       "GBM" = gbm_model)

results <- resamples(final_models) %>% summary()
results
#Evaluate models

#kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
#  cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.tex"))

#Evaluate on holdout set 

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

result_5

#########################################################################################
#
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################

#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################

# variable importance data frame
rf_model_var_imp <- importance(rf_model$finalModel)/1000
rf_model_var_imp_df <-
  data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_var_imp_df
# variable importance plot - full plot
plot(varImp(rf_model))

# variable importance plot - top 10 only
rf_model_var_imp_plot_b <- ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(colour="navyblue", size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), colour="navyblue", size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_var_imp_plot_b

# grouped variable importance - keep binaries created off factors together
## in our case it is not important as we do not have dummy variable in the TOP10 most important variables

#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

# if we increase the number of accommodates, the price also increases in a fairly linear way
pdp_n_acc <- pdp::partial(rf_model, pred.var = "accommodates", pred.grid = distinct_(df_holdout, "accommodates"), train = df_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(colour="navyblue", size=2) +
  geom_line(colour="navyblue", size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()
pdp_n_acc_plot

# an apartment is predicted to have higher price than a room
pdp_n_roomtype <- pdp::partial(rf_model, pred.var = "property_type", pred.grid = distinct_(df_holdout, "property_type"), train = df_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color="navyblue", size=2) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()
pdp_n_roomtype_plot

# Subsample performance: 
# how well the rf_model performs on different subsets - calculating RMSE and the mean price per each subgroup

# holdout set
df_holdout_w_prediction <- df_holdout %>%
  mutate(predicted_price = predict(rf_model, newdata = df_holdout))


######### create nice summary table of heterogeneity
a <- df_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )




b <- df_holdout_w_prediction %>%
  filter(property_type %in% c("apartment", "room")) %>%
  group_by(property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

b

c <- df_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output - RESULT_3 shows error
# colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
# colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
# colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")

# line1 <- c("Size of accomodation", "", "", "")
# line2 <- c("Property type", "", "", "")
# line3 <- c("All",  "", "", "")

# result_3 <- rbind(line1, a, line2, b, line3, c) %>%
# transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`), RMSE/price` = as.numeric(`RMSE/price`))

# Groupped variable importance
rf_model_var_imp <- importance(rf_model$finalModel)/1000
rf_model_var_imp_df <-
    data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp))


rf_model_var_imp_df <- rf_model_var_imp_df %>% mutate(
    group = ifelse(varname %in% amenities, 'amenities',
                   ifelse(varname %in% basic_vars, basic_vars,
                   ifelse(varname %in% reviews, 'reviews', 'host_name'))))

rf_model_var_imp_df %>%  group_by(group) %>% summarise(group_imp_sum = sum(imp_percentage)) %>%  arrange(desc(group_imp_sum))

