
climcropr_cache <- NULL
.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set(path = "climcropr", type =  "user_cache_dir")
  climcropr_cache <<- x
}
