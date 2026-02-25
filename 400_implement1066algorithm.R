rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
user <- "Emma"
if (Sys.info()["sysname"] == "Windows") {
    code_filepath <- "C:/Users/emmanich/code/ADAMS-HCAP-ALGO/"
} else {
    code_filepath <- "/Users/emmanich/code/ADAMS-HCAP-ALGO/"
}
source(here::here(paste0(code_filepath, "001_libraries.R")))
source(here::here(paste0(code_filepath, "002_directories.R")))

tidied <- readr::read_rds(here::here(rds_filepath, "010_tidy-data.rds"))
fscores <- readr::read_rds(paste0(rds_filepath, "fscores.rds"))

# SET UP TRAINING DATA ------------------------------------------------------------------

data_for1066 <- data.table(tidied)
data_for1066 <- as.data.table(merge(data_for1066, fscores, by = "ADAMSSID"))
setnames(data_for1066, c("vdori1", "vdvis1"), c("ORI", "VIS"))

## impute missing components of the 1066 algorithm  
data_for1066 <- simputation::impute_pmm(data_for1066, EXF ~ MEM + LFL + ORI)
data_for1066 <- simputation::impute_pmm(data_for1066, VIS ~ MEM + EXF + LFL + ORI)
data_for1066 <- simputation::impute_pmm(data_for1066, aRECALLcs ~ MEM + EXF + LFL + ORI + VIS)
data_for1066 <- simputation::impute_pmm(data_for1066, COGSCORE ~ MEM + EXF + LFL + ORI + VIS + aRECALLcs)

## subset to only variables needed for the 1066 algorithm 
data_for1066 <- data_for1066[, .(ADAMSSID, diagnosis_adjusted, COGSCORE, RELSCORE, aRECALLcs)]

# Create binary outcome for classification: Dementia vs. No Dementia
data_for1066[, dementia_binary := factor(ifelse(diagnosis_adjusted == "Dementia", "Dementia", "No Dementia"), levels = c("No Dementia", "Dementia"))]

# Add row identifier for merging back
data_for1066[, row_id := .I]

# Set up 10-fold cross-validation
cv_folds <- vfold_cv(data_for1066, v = 10)

# DO MODELING AND CHECK OUT PERFORMANCE --------------------------------------------------

# set seed 
set.seed(5846)

# Define the logistic regression model for classification
log_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Define the recipe
log_recipe <- recipe(dementia_binary ~ COGSCORE + RELSCORE + aRECALLcs, data = data_for1066)

# Create workflow
log_workflow <- workflow() %>%
  add_model(log_model) %>%
  add_recipe(log_recipe)

# Fit the model with cross-validation
log_cv_results <- fit_resamples(
  log_workflow,
  resamples = cv_folds,
  metrics = metric_set(roc_auc),  # Only ROC AUC as it's threshold-independent
  control = control_resamples(save_pred = TRUE, save_workflow = TRUE)
)

# Collect predictions
preds <- collect_predictions(log_cv_results)

# Reclassify with threshold 0.25
preds <- preds %>%
  mutate(.pred_class_0.25 = factor(if_else(.pred_Dementia > 0.25, "Dementia", "No Dementia"), levels = c("No Dementia", "Dementia"))) %>%
  as.data.table()

# Compute metrics with threshold 0.25
custom_metrics <- preds %>%
  summarise(
    accuracy = mean(.pred_class_0.25 == dementia_binary),
    sens = yardstick::sens_vec(dementia_binary, .pred_class_0.25),
    spec = yardstick::spec_vec(dementia_binary, .pred_class_0.25),
    roc_auc = yardstick::roc_auc_vec(dementia_binary, .pred_Dementia)
  )

print(custom_metrics)

# MERGE PREDICTIONS BACK INTO DATA ------------------------------------------------------------------

# Merge predictions back into data.table with ADAMSSID
results <- merge(data_for1066[, .(row_id, ADAMSSID)], preds[, .(.row, pred1066_dementia_prob = .pred_Dementia, pred1066_dementia = .pred_class_0.25)], by.x = "row_id", by.y = ".row", all.x = TRUE)
results[, row_id := NULL]

# Save results
readr::write_rds(results, here::here(paste0(rds_filepath, "results_1066.rds")))
