test_that("convert to well works", {

  expect_equal(
    to.well(
      15,
      wise = "row",
      plate = 96),
    "B3")


  expect_equal(
    to.well(c(15, 35, 100, 204),
            wise = "row",
            plate = 384),
    c("A15", "B11", "E4", "I12"))


})
