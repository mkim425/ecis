library(dplyr)
library(hubUtils)

test_that("split_data_by_task() groups data correctly", {
        hub_con <- connect_hub(system.file("testhubs/flusight", package = "hubUtils"))
        forecast_data <- hub_con %>% dplyr::collect()
        result <- split_data_by_task(forecast_data)

        # Check if the result is a list
        expect_type(result, "list")

        # Check if each data set has unique task_id
        expect_true(all(
                sapply(
                        result, function(x) {
                                task_id <- setdiff(colnames(x),
                                                   c("model_id", "output_type", "output_type_id", "value"))
                                a <- x %>%
                                        select(all_of(task_id)) %>%
                                        summarise_all(n_distinct) %>%
                                        unlist %>% unname()
                                setequal(a, rep(1, length(task_id)))}
                        )
                ))

})
