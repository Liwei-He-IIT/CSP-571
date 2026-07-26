# Secondary Mushroom Dataset
# Step 2: Stratified split, model fitting, and test-set evaluation
#
# This script is designed to run from the project root after the packages listed
# below are installed. It sources code/01_eda.R so that the same validated and
# cleaned data is used every time.

options(stringsAsFactors = FALSE)

# -------------------------------------------------------------------------
# 1. Load the reproducibly cleaned dataset without reprinting EDA plots
# -------------------------------------------------------------------------

RUN_QC <- FALSE
RUN_PLOTS <- FALSE
source("code/01_eda.R")

if (!exists("mushrooms_clean")) {
  stop("mushrooms_clean was not created by code/01_eda.R.")
}

if (anyNA(mushrooms_clean)) {
  stop("The modeling dataset still contains missing values.")
}

if (any(duplicated(mushrooms_clean))) {
  stop("The modeling dataset still contains exact duplicate rows.")
}

# -------------------------------------------------------------------------
# 2. Check modeling packages
# -------------------------------------------------------------------------

required_model_packages <- c(
  "rpart",
  "rpart.plot",
  "randomForest",
  "pROC"
)

missing_model_packages <- required_model_packages[
  !vapply(
    required_model_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_model_packages) > 0) {
  stop(
    paste0(
      "Install the missing modeling packages with: install.packages(c(",
      paste(sprintf('"%s"', missing_model_packages), collapse = ", "),
      "))"
    )
  )
}

# -------------------------------------------------------------------------
# 3. Create one reproducible stratified 80/20 train/test split
# -------------------------------------------------------------------------

set.seed(571)

indices_by_class <- split(
  seq_len(nrow(mushrooms_clean)),
  mushrooms_clean$class
)

train_indices <- unlist(
  lapply(
    indices_by_class,
    function(indices) {
      sample(
        indices,
        size = floor(0.80 * length(indices)),
        replace = FALSE
      )
    }
  ),
  use.names = FALSE
)

train_indices <- sort(unique(train_indices))
test_indices <- setdiff(seq_len(nrow(mushrooms_clean)), train_indices)

train_data <- mushrooms_clean[train_indices, , drop = FALSE]
test_data <- mushrooms_clean[test_indices, , drop = FALSE]

row.names(train_data) <- NULL
row.names(test_data) <- NULL

split_summary <- data.frame(
  dataset = c("Full cleaned data", "Training set", "Test set"),
  rows = c(
    nrow(mushrooms_clean),
    nrow(train_data),
    nrow(test_data)
  ),
  edible_percent = round(
    100 * c(
      mean(mushrooms_clean$class == "edible"),
      mean(train_data$class == "edible"),
      mean(test_data$class == "edible")
    ),
    2
  ),
  poisonous_percent = round(
    100 * c(
      mean(mushrooms_clean$class == "poisonous"),
      mean(train_data$class == "poisonous"),
      mean(test_data$class == "poisonous")
    ),
    2
  )
)

cat("\n--- STRATIFIED SPLIT SUMMARY ---\n")
print(split_summary, row.names = FALSE)

if (length(intersect(train_indices, test_indices)) > 0) {
  stop("Training and test indices overlap.")
}

# -------------------------------------------------------------------------
# 4. Shared evaluation helpers
# -------------------------------------------------------------------------

class_levels <- c("edible", "poisonous")
positive_class <- "poisonous"

evaluate_classifier <- function(
    actual,
    predicted,
    poisonous_probability,
    model_name) {
  actual <- factor(actual, levels = class_levels)
  predicted <- factor(predicted, levels = class_levels)

  confusion <- table(
    Actual = actual,
    Predicted = predicted
  )

  true_negative <- confusion["edible", "edible"]
  false_positive <- confusion["edible", "poisonous"]
  false_negative <- confusion["poisonous", "edible"]
  true_positive <- confusion["poisonous", "poisonous"]

  accuracy <- (
    true_positive + true_negative
  ) / sum(confusion)

  sensitivity <- true_positive / (
    true_positive + false_negative
  )

  specificity <- true_negative / (
    true_negative + false_positive
  )

  precision <- true_positive / (
    true_positive + false_positive
  )

  f1 <- 2 * precision * sensitivity / (
    precision + sensitivity
  )

  roc_object <- pROC::roc(
    response = actual,
    predictor = poisonous_probability,
    levels = class_levels,
    direction = "<",
    quiet = TRUE
  )

  metrics <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(accuracy),
    Poisonous_Recall = as.numeric(sensitivity),
    Specificity = as.numeric(specificity),
    Precision = as.numeric(precision),
    F1 = as.numeric(f1),
    AUC = as.numeric(pROC::auc(roc_object)),
    Dangerous_False_Negatives = as.integer(false_negative),
    row.names = NULL
  )

  list(
    confusion_matrix = confusion,
    metrics = metrics,
    roc = roc_object
  )
}

probability_to_class <- function(probability, threshold = 0.50) {
  factor(
    ifelse(
      probability >= threshold,
      "poisonous",
      "edible"
    ),
    levels = class_levels
  )
}

# -------------------------------------------------------------------------
# 5. Model 1: Logistic regression
# -------------------------------------------------------------------------

# Standardize numerical predictors using training-set statistics only.
logistic_train <- train_data
logistic_test <- test_data

training_means <- vapply(
  logistic_train[numeric_variables],
  mean,
  numeric(1)
)

training_standard_deviations <- vapply(
  logistic_train[numeric_variables],
  stats::sd,
  numeric(1)
)

if (any(training_standard_deviations == 0)) {
  stop("At least one numerical predictor has zero variance in training.")
}

for (variable in numeric_variables) {
  logistic_train[[variable]] <- (
    logistic_train[[variable]] - training_means[[variable]]
  ) / training_standard_deviations[[variable]]

  logistic_test[[variable]] <- (
    logistic_test[[variable]] - training_means[[variable]]
  ) / training_standard_deviations[[variable]]
}

cat("\nFitting logistic regression...\n")

logistic_model <- stats::glm(
  class ~ .,
  data = logistic_train,
  family = stats::binomial(),
  control = stats::glm.control(maxit = 100)
)

logistic_probability <- stats::predict(
  logistic_model,
  newdata = logistic_test,
  type = "response"
)

if (anyNA(logistic_probability)) {
  stop("Logistic regression produced NA probabilities.")
}

logistic_prediction <- probability_to_class(
  logistic_probability
)

logistic_results <- evaluate_classifier(
  actual = logistic_test$class,
  predicted = logistic_prediction,
  poisonous_probability = logistic_probability,
  model_name = "Logistic Regression"
)

cat("\n--- LOGISTIC REGRESSION CONFUSION MATRIX ---\n")
print(logistic_results$confusion_matrix)

# -------------------------------------------------------------------------
# 6. Model 2: Decision tree
# -------------------------------------------------------------------------

set.seed(571)
cat("\nFitting decision tree...\n")

tree_model <- rpart::rpart(
  class ~ .,
  data = train_data,
  method = "class",
  control = rpart::rpart.control(
    cp = 0.001,
    minsplit = 30,
    xval = 10
  )
)

tree_probability <- predict(
  tree_model,
  newdata = test_data,
  type = "prob"
)[, positive_class]

tree_prediction <- probability_to_class(
  tree_probability
)

tree_results <- evaluate_classifier(
  actual = test_data$class,
  predicted = tree_prediction,
  poisonous_probability = tree_probability,
  model_name = "Decision Tree"
)

cat("\n--- DECISION TREE CONFUSION MATRIX ---\n")
print(tree_results$confusion_matrix)

tree_cp_table <- as.data.frame(tree_model$cptable)

# -------------------------------------------------------------------------
# 7. Model 3: Random forest
# -------------------------------------------------------------------------

set.seed(571)
cat("\nFitting random forest (300 trees)...\n")

random_forest_model <- randomForest::randomForest(
  class ~ .,
  data = train_data,
  ntree = 300,
  importance = TRUE,
  keep.forest = TRUE,
  do.trace = 50
)

random_forest_probability <- predict(
  random_forest_model,
  newdata = test_data,
  type = "prob"
)[, positive_class]

random_forest_prediction <- probability_to_class(
  random_forest_probability
)

random_forest_results <- evaluate_classifier(
  actual = test_data$class,
  predicted = random_forest_prediction,
  poisonous_probability = random_forest_probability,
  model_name = "Random Forest"
)

cat("\n--- RANDOM FOREST CONFUSION MATRIX ---\n")
print(random_forest_results$confusion_matrix)

random_forest_oob_error <- tail(
  random_forest_model$err.rate[, "OOB"],
  1
)

random_forest_importance <- as.data.frame(
  randomForest::importance(
    random_forest_model,
    type = 1
  )
)

random_forest_importance$Variable <- row.names(
  random_forest_importance
)
row.names(random_forest_importance) <- NULL

importance_metric_name <- setdiff(
  names(random_forest_importance),
  "Variable"
)[1]

random_forest_importance <- random_forest_importance[
  order(
    random_forest_importance[[importance_metric_name]],
    decreasing = TRUE
  ),
  c("Variable", importance_metric_name),
  drop = FALSE
]

# -------------------------------------------------------------------------
# 8. Compare the three models
# -------------------------------------------------------------------------

model_comparison <- rbind(
  logistic_results$metrics,
  tree_results$metrics,
  random_forest_results$metrics
)

numeric_metric_columns <- setdiff(
  names(model_comparison),
  c("Model", "Dangerous_False_Negatives")
)

model_comparison[numeric_metric_columns] <- lapply(
  model_comparison[numeric_metric_columns],
  function(x) round(x, 4)
)

cat("\n--- MODEL COMPARISON ON THE HELD-OUT TEST SET ---\n")
print(model_comparison, row.names = FALSE)

cat("\n--- RANDOM FOREST OOB ERROR ---\n")
print(round(as.numeric(random_forest_oob_error), 6))

cat("\n--- TOP 10 RANDOM FOREST VARIABLES ---\n")
print(
  head(random_forest_importance, 10),
  row.names = FALSE
)

# -------------------------------------------------------------------------
# 9. Optional model visualizations
# -------------------------------------------------------------------------

if (!exists("RUN_MODEL_PLOTS")) {
  RUN_MODEL_PLOTS <- TRUE
}

if (RUN_MODEL_PLOTS) {
  # The evaluated tree may contain too many terminal nodes for a legible static
  # figure. Create a small display-only tree with at most six splits. This does
  # not replace tree_model and does not change the reported test metrics.
  tree_display_candidates <- tree_model$cptable[
    tree_model$cptable[, "nsplit"] <= 6,
    ,
    drop = FALSE
  ]

  tree_display_cp <- tree_display_candidates[
    nrow(tree_display_candidates),
    "CP"
  ]

  tree_display_model <- rpart::prune(
    tree_model,
    cp = tree_display_cp
  )

  rpart.plot::rpart.plot(
    tree_display_model,
    type = 2,
    extra = 104,
    fallen.leaves = TRUE,
    tweak = 0.9,
    main = "Simplified Decision Tree (Display Only)"
  )

  randomForest::varImpPlot(
    random_forest_model,
    type = 1,
    n.var = 15,
    main = "Random Forest Variable Importance"
  )
}

cat("\nModeling completed successfully.\n")
cat(
  "Review model_comparison, each confusion matrix, OOB error, and ",
  "random_forest_importance before writing the conclusion.\n",
  sep = ""
)
