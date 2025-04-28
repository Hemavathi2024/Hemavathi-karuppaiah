library(mlbench)
library(purrr)

data("PimaIndiansDiabetes2")
ds <- as.data.frame(na.omit(PimaIndiansDiabetes2))
## fit a logistic regression model to obtain a parametric equation
logmodel <- glm(diabetes ~ .,
                data = ds,
                family = "binomial")
summary(logmodel)

cfs <- coefficients(logmodel) ## extract the coefficients
prednames <- variable.names(ds)[-9] ## fetch the names of predictors in a vector
prednames

sz <- 100000000 ## to be used in sampling
##sample(ds$pregnant, size = sz, replace = T)

dfdata <- map_dfc(prednames,
                  function(nm){ ## function to create a sample-with-replacement for each pred.
                    eval(parse(text = paste0("sample(ds$",nm,
                                             ", size = sz, replace = T)")))
                  }) ## map the sample-generator on to the vector of predictors
## and combine them into a dataframe

names(dfdata) <- prednames
dfdata

class(cfs[2:length(cfs)])

length(cfs)
length(prednames)
## Next, compute the logit values
pvec <- map((1:8),
            function(pnum){
              cfs[pnum+1] * eval(parse(text = paste0("dfdata$",
                                                     prednames[pnum])))
            }) %>% ## create beta[i] * x[i]
  reduce(`+`) + ## sum(beta[i] * x[i])
  cfs[1] ## add the intercept

## exponentiate the logit to obtain probability values of thee outcome variable
dfdata$outcome <- ifelse(1/(1 + exp(-(pvec))) > 0.5,
                         1, 0)

library(xgboost)
library(dplyr)

set.seed(123)
sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)
results <- data.frame(
  Method = character(),
  Dataset_Size = integer(),
  Accuracy = numeric(),
  Time_Taken = numeric(),
  stringsAsFactors = FALSE
)

for (sz in sizes) {
  df_sample <- dfdata %>% sample_n(sz)
  label <- df_sample$outcome
  data_matrix <- xgb.DMatrix(data = as.matrix(df_sample %>% select(-outcome)), label = label)
  idx_train <- sample(1:sz, size = 0.8 * sz)
  dtrain <- xgb.DMatrix(data = as.matrix(df_sample[idx_train, ] %>% select(-outcome)), label = label[idx_train])
  dtest <- xgb.DMatrix(data = as.matrix(df_sample[-idx_train, ] %>% select(-outcome)), label = label[-idx_train])

  start_time <- Sys.time()
  
  model <- xgboost(
    data = dtrain,
    max.depth = 6,
    eta = 0.3,
    nrounds = 50,
    objective = "binary:logistic",
    verbose = 0
  )
  
  end_time <- Sys.time()
  preds <- predict(model, dtest)
  pred_labels <- ifelse(preds > 0.5, 1, 0)

  acc <- mean(pred_labels == label[-idx_train])
  results <- rbind(results, data.frame(
    Method = "XGBoost (simple CV)",
    Dataset_Size = sz,
    Accuracy = round(acc, 4),
    Time_Taken = round(as.numeric(difftime(end_time, start_time, units = "secs")), 2),
    stringsAsFactors = FALSE
  ))
}

print(results)


library(caret)
library(xgboost)
library(dplyr)

set.seed(123)

# Define dataset sizes
sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

# Initialize a dataframe to store results
results_caret <- data.frame(
  Method = character(),
  Dataset_Size = integer(),
  Accuracy = numeric(),
  Time_Taken = numeric(),
  stringsAsFactors = FALSE
)

# Control for caret training
train_control <- trainControl(method = "cv", number = 5)

# Loop over different dataset sizes
for (sz in sizes) {
  
  # Sample the dataset
  df_sample <- dfdata %>% sample_n(sz)
  
  # Prepare data
  label <- as.factor(df_sample$outcome) # caret needs factors for classification
  predictors <- df_sample %>% select(-outcome)
  
  # Start time
  start_time <- Sys.time()
  
  # Train model using caret + xgboost
  model_caret <- train(
    x = predictors,
    y = label,
    method = "xgbTree",
    trControl = train_control,
    tuneLength = 3, # simple tuning grid
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  acc <- max(model_caret$results$Accuracy)
  results_caret <- rbind(results_caret, data.frame(
    Method = "XGBoost (caret 5-fold CV)",
    Dataset_Size = sz,
    Accuracy = round(acc, 4),
    Time_Taken = round(as.numeric(difftime(end_time, start_time, units = "secs")), 2),
    stringsAsFactors = FALSE
  ))
}

print(results_caret)


