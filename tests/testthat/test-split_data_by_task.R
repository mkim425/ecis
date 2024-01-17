library(dplyr)
library(hubUtils)

test_that("split_data_by_task() groups data correctly", {
        hub_con <- connect_hub(system.file("testhubs/flusight", package = "hubUtils"))
        forecast_data <- hub_con %>% dplyr::collect()
        result <- split_data_by_task(forecast_data)

        # Check if the result is a list
        expect_type(result, "list")

})
