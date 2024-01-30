test_that("input_data_validation() requires a single output type in a dataset", {
        hub_con <- connect_hub(system.file("testhubs/flusight", package = "hubUtils"))
        forecast_data1 <- hub_con %>% dplyr::collect()
        forecast_data2 <- forecast_data1 %>% dplyr::filter(output_type == "quantile")
        row <- forecast_data2[23,]
        row$output_type <- NA
        forecast_data3 <- rbind(forecast_data2[1:22,], row)

        expect_error(input_data_validation(forecast_data1),
                     "The input data must contain a single output type.")

        expect_error(input_data_validation(forecast_data3),
                     "The output type has a missing value.")

        expect_equal(input_data_validation(forecast_data2) %>%
                             select(output_type) %>%
                             unique() %>%
                             as.character(),
                     forecast_data2$output_type[1])

})
