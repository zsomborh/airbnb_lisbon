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


#  create holdout set 
set.seed(2801)

train_indices <- as.integer(createDataPartition(df$price, p = 0.75, list = FALSE))
df_train <- df[train_indices, ]
df_holdout <- df[-train_indices, ]

#feature_engineering 
# TODO - get log transformation for skewed predictors
# TODO - create interaction variables  - property type vs dummies/accomodates/dogs/cats
# TODO - get log transformed outcome variable 

predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, host_info, reviews, amenities)
#predictors_3 <- c(predictors2 -- and interactions and log transformed variables



# Modeling ----------------------------------------------------------------

# train control is 5 fold cross validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# Starting off with simple OLS 

set.seed(7)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))


# Second is Lasso

set.seed(7)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))), #Change predictors to contain interactions... 
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
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

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)

# Third is CART
# CART
set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "rpart",
    tuneLength = 10,
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

set.seed(1234)
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
  list("OLS" = ols_model,
       "LASSO (model w/ interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest" = rf_model,
       "GBM" = gbm_model)

results <- resamples(final_models) %>% summary()

#Evaluate models

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.tex"))

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

