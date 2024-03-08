#' Split forecast data by a single task
#'
#' @param valid_tbl A data.frame in a model_out_tbl format.
#'
#' @return List of data sets. Each data set is for a single task.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @import hubUtils
#' @export
#'
#' @examples \dontrun{
#' library(dplyr)
#' library(hubUtils)
#' hub_path <- system.file("testhubs/flusight", package = "hubUtils")
#' hub_con <- connect_hub(hub_path)
#' hub_con |>
#'   filter(output_type == "quantile") |>
#'   collect() |>
#'   valid_input_data() |>
#'   split_data_by_task()
#'   }
split_data_by_task <- function(valid_tbl) {
  # Extract task_ID columns
  task_id_cols <- setdiff(
    colnames(valid_tbl),
    c("model_id", "output_type", "output_type_id", "value")
  )

  # List of data sets by task and output_type
  grouped_df <- valid_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(task_id_cols)))) |>
    dplyr::group_split()

  return(grouped_df)
}
