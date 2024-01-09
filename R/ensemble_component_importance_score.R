#' Evaluate ensemble component model's importance based on a measure of their contribution to ensemble prediction accuracy
#'
#' @param forecast_data A data.frame with the predictions that is or can be coerced to a model_out_tbl format.
#' @param true_value Ground truth data, i.e., "target data", for the variables that are used to define modeling targets.
#' @param output_type String specifying the type of representation of the predictive distribution;  `c("quantile", "pmf", "cdf", "mean", "median", "sample")`.
#' @param agg_fun String specifying method for ensembling quantiles; `c("mean", "median")`.
#' @param weighted Boolean indicating whether model weighting should be done. If `FALSE`, all models are given equal weight. If `TRUE`, model weights are estimated.
#' @param method String specifying algorithm for model importance calculation; `c("lomo", "lasomo")`. `"lomo"` stands for leave-one-model-out and `"lasomo"` stands for leave all subsets of models out.
#' @param subset_wt String specifying method for assigning weight to subsets when using `lasomo` algorithm.
#' @param scoring_rule String specifying metric to use to calculate importance; `c("MAE", "MSE", "WIS", "CRPS", "Logscore")`.
#' @param na.action String specifying treatment for missing data; `c("worst", "average", "drop")`.
#'
#' @return A data.frame with columns `task_id`, `model`, `importance_score`.
#' @export
#'
#' @examples
ensemble_component_importance_score <- function(
                forecast_data,
                true_value,
                output_type = c("quantile", "pmf", "cdf", "mean", "median", "sample"),
                agg_fun = c("mean", "median"),
                weighted = FALSE,
                method = c("lomo", "lasomo"),
                subset_wt = c("equal", "perm_based"),
                scoring_rule = c("MAE", "MSE", "WIS", "CRPS", "Logscore"),
                na.action = c("worst", "average", "drop")){
        return(score_result)
}

