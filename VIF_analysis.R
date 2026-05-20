library(usdm)
library(dplyr)

# 1. Variable Selection and Initial Extraction
response_var <- "pa"
predictors <- names(train_set)[5:ncol(train_set)]

x_train <- train_set[, predictors]
y_train <- train_set[[response_var]]

y_test <- test_set$pa
x_test <- test_set[, predictors]

# 2. Variance Inflation Factor (VIF) Filtering
# Iterative VIF selection with a threshold of 10
set.seed(123)
vif_result <- vifstep(x_train, th = 10, method = 'spearman', keep = c('B1', 'B12'))

# Extract variables that passed the VIF test
selected_vars <- vif_result@results$Variables

# 3. Structural Alignment for Modeling
# Training sets
x_train_vif <- x_train[, selected_vars]
model_train <- cbind(pa = y_train, x_train_vif)

# Testing sets (ensuring identical column composition)
x_test_vif <- x_test[, selected_vars]
model_test <- cbind(pa = y_test, x_test_vif)

# 4. Summary Output
print(vif_result)
cat("Number of variables retained after VIF filtering:", length(selected_vars), "\n")