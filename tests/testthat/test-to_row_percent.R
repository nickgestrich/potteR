test_that("to_row_percent rows add up to 100", {
  df <- tibble::tibble(x = 3:5, y = 4:6, z = 6:8)
  expect_equal(
    df |>
      to_row_percent() |>
      rowSums(),
    c(100, 100, 100)
  )
})
