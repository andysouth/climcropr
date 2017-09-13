
context("Cache directory handling")

test_that("test that set_cache does not create a dir if cache == FALSE", {
  skip_on_cran()
  unlink(
    rappdirs::user_cache_dir(appname = "climcropr",
                             appauthor = "climcropr"),
    recursive = TRUE
  )
  get_mirca(
    cropname = "sorghum",
    rainfed = FALSE,
    plot = FALSE,
    cache = FALSE
  )
  expect_true(!file.exists(
    rappdirs::user_cache_dir(appname = "climcropr",
                             appauthor = "climcropr")
    )
  )
})


  test_that("cache directory is created if necessary", {
    skip_on_cran()
    # if cache directory exists during testing, remove it
    unlink(
      rappdirs::user_cache_dir(appname = "climcropr",
                               appauthor = "climcropr"),
      recursive = TRUE
    )
    get_mirca(
      cropname = "sorghum",
      rainfed = FALSE,
      plot = FALSE,
      cache = TRUE
    )
    expect_true(file.exists(
      rappdirs::user_cache_dir(appname = "climcropr",
                               appauthor = "climcropr")
    )
    )
  })

  # test that file lists and deletions are properly handled ----------------------

  test_that("caching utils list files in cache and delete when asked", {
    skip_on_cran()
    unlink(rappdirs::user_cache_dir(appname = "climcropr",
                                    appauthor = "climcropr"))
    f <-
      raster::raster(system.file("external/test.grd", package = "raster"))
    cache_dir <- rappdirs::user_cache_dir(appname = "climcropr",
                                          appauthor = "climcropr")
    raster::writeRaster(f, file.path(cache_dir, "file1.tif"), format = "GTiff")
    raster::writeRaster(f, file.path(cache_dir, "file2.tif"), format = "GTiff")

    # test climcropr cache list
    k <- list.files(rappdirs::user_cache_dir(appname = "climcropr",
                                             appauthor = "climcropr"))
    expect_equal(basename(climcropr_cache_list()), k)

    # file should not exist, expect error
    expect_error(climcropr_cache_delete("file1.asc"))

    # test delete one file
    climcropr_cache_delete("file1.tif")
    l <- list.files(rappdirs::user_cache_dir(appname = "climcropr",
                                             appauthor = "climcropr"))
    expect_equal(basename(climcropr_cache_list()), l)

    # test delete all
    climcropr_cache_delete_all()
    expect_equal(list.files(
      rappdirs::user_cache_dir(appname = "climcropr",
                               appauthor = "climcropr")
    ),
    character(0))
  })
