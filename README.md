# mlTools
Tools to evaluate regression and classification models -  Work in Progress  
  
The package currently contains the following functions for binary classification.  

* bin_calc_auc_pr - Calculates area under the Precision-Recall curve
* bin_conf_matrix - Returns the confusion matrix as a dataframe for a given threshold
* bin_thres_eval - Returns the TP, TN, FP, FN numbers for a given threshold
* bin_model_eval - For a given set of original and predicted values, this function returns the AUC-ROC, AUC-PR, ROC curve, PR curve, and a dataframe of TP/TN/FP/FN for all thresholds between 0 to 1
* bin_cv_glm - K-fold cross validated GLM model. Returns a list of AUC-ROC and AUC-PR, evaluated at each fold
* bin_cv_rf - K-fold cross validated random forest model. Returns a list of AUC-ROC and AUC-PR, evaluated at each fold