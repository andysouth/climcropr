
#' @title Manage Cached climcropr Files
#'
#' @description Manage cached \pkg{climcropr} files with \pkg{hoardr}
#'
#' @export
#' @name climcropr_cache
#'
#' @details The dafault cache directory can be discovered by using
#' \code{`file.path(rappdirs::user_cache_dir(), "R/climcropr")`}. However,
#' users may define their own path using \code{cache_path_set()}.
#'
#' \code{cache_delete} only accepts one file name, while \code{cache_delete_all}
#' doesn't accept any names, but deletes all files. For deleting many specific
#' files, use \code{cache_delete} in an \code{\link[base]{lapply}()} type call.
#'
#' @section Useful user functions:
#' \itemize{
#'  \item \code{climcropr_cache$cache_path_get()} - get cache path
#'  \item \code{climcropr_cache$cache_path_set()} - set cache path
#'  \item \code{climcropr_cache$list()} - returns a character vector of full
#'  path file names
#'  \item \code{climcropr_cache$files()} - returns file objects with metadata
#'  \item \code{climcropr_cache$details()} - returns files with details
#'  \item \code{climcropr_cache$delete()} - delete specific files
#'  \item \code{climcropr_cache$delete_all()} - delete all files, returns
#'  nothing
#' }
#'
#' @examples \dontrun{
#' climcropr_cache
#'
#' # list files in cache
#' climcropr_cache$list()
#'
#' # delete certain database files
#' # climcropr_cache$delete("file path")
#' # climcropr_cache$list()
#'
#' # delete all files in cache
#' # climcropr_cache$delete_all()
#' # climcropr_cache$list()
#'
#' # set a different cache path from the default
#' }
NULL
