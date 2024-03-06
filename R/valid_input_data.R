#' Check the input forecast data structure
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a model_out_tbl format.
#'
#' @return a model_out_tbl format that has a single output type
#' @export
#'
#' @examples
#' library(dplyr)
#' library(hubUtils)
#' load("data/example_quantile_model_output.rda")
#' example_quantile_model_output |>
#'   valid_input_data()
valid_input_data <- function(forecast_data) {
  valid_tbl <- forecast_data |>
    # Convert model output to a `model_out_tbl` class object
    hubUtils::as_model_out_tbl() |>
    # Validate a `model_out_tbl` object
    hubUtils::validate_model_out_tbl()

  # Check if NA exists in the output type
  if (sum(is.na(valid_tbl$output_type)) != 0) {
    stop("The output type has a missing value.")
  }

  # Check if the data contain a single output type
  num_output_type <- valid_tbl$output_type |>
    unique() |>
    length()
  if (num_output_type != 1) {
    stop("The input data must contain a single output type.")
  }

  return(valid_tbl)
}
