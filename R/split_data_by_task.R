#' Check forecast data structure and split data by a single task
#'
#' @param forecast_data A data.frame with the predictions that is or can be coerced to a model_out_tbl format.
#'
#' @return List of data sets. Each data set is for a single task.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @import hubUtils
#' @export
#'
#' @examples
#' library(dplyr)
#' library(hubUtils)
#' hub_path <- system.file("testhubs/flusight", package = "hubUtils")
#' hub_con <- connect_hub(hub_path)
#' hub_con %>%
#'         collect() %>%
#'         split_data_by_task()
split_data_by_task <- function(forecast_data){

        valid_tbl <- forecast_data %>%
                # Convert model output to a `model_out_tbl` class object
                hubUtils::as_model_out_tbl() %>%
                # Validate a `model_out_tbl` object
                hubUtils::validate_model_out_tbl()

        # Extract task_IDs
        task_id <- setdiff(colnames(forecast_data),
                            c("model_id", "output_type", "output_type_id", "value"))

        # List of data sets by task and output_type
        grouped_df <- valid_tbl %>%
                dplyr::group_by(across(all_of(c(task_id, "output_type")))) %>%
                dplyr::group_split()

        return(grouped_df)
}
