#' Check forecast data structure
#'
#' @param forecast_data A data.frame with the predictions that is or can be coerced to a model_out_tbl format.
#'
#' @return
#' @export
#'
#' @examples
check_data_format <- function(forecast_data){
        # Convert model output to a `model_out_tbl` class object
        tbl <- hubUtils::as_model_out_tbl(forecast_data)
        # Validate a `model_out_tbl` object.
        valid_tbl <- hubUtils::validate_model_out_tbl(tbl)
        # Extract task_IDs
        task_id <- setdiff(colnames(forecast_data),
                            c("model_id", "output_type", "output_type_id", "value"))

        # List of each task data set
        grouped_df <- valid_tbl %>%
                dplyr::group_by(across(all_of(task_id))) %>%
                dplyr::group_split()


        return(grouped_df)
}
