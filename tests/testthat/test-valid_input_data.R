library(dplyr)

test_that("valid_input_data() requires a single output type in a dataset", {
  forecast_data1 <- readRDS("testdata/flu_data.rds")
  forecast_data2 <- forecast_data1 |> dplyr::filter(output_type == "quantile")
  row <- forecast_data2[23, ]
  row$output_type <- NA
  forecast_data3 <- rbind(forecast_data2[1:22, ], row)

  expect_error(
    valid_input_data(forecast_data1),
    "The input data must contain a single output type."
  )

  expect_error(
    valid_input_data(forecast_data3),
    "The output type has a missing value."
  )

  expect_equal(
    valid_input_data(forecast_data2) |>
      dplyr::select(output_type) |>
      unique() |>
      as.character(),
    forecast_data2$output_type[1]
  )
})
