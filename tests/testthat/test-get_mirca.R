
# Test that get_mirca functions properly ---------------------------------------
context("get_mirca")
file.remove(file.path(tempdir(), list.files(tempdir(), pattern = "asc.gz$")))
test_that("get_mirca creates an S4 object for a rainfed crop as default", {
  skip_on_cran()
  unlink(file.path(tempdir(), "*.asc.gz"))
  rst_mir <- get_mirca("potatoes", rainfed = TRUE, cache = FALSE)
  expect_type(rst_mir, S4)

  expect_match(list.files(tempdir(), pattern = ".asc.gz$"),
                          "annual_area_harvested_rfc_crop10_ha_30mn.asc.gz")
})


context("get_mirca")
file.remove(file.path(tempdir(), list.files(tempdir(), pattern = "asc.gz$")))
test_that("get_mirca downloads an irrigated crop when specificed", {
  skip_on_cran()
  unlink(file.path(tempdir(), "*.asc.gz"))
  rst_mir <- get_mirca("potatoes", rainfed = TRUE, cache = FALSE)
  expect_match(list.files(tempdir(), pattern = ".asc.gz$"),
               "annual_area_harvested_irc_crop10_ha_30mn.asc.gz")
})

