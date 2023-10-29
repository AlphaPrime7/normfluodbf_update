#' Title: A function to read multiple dbfs into the R environment
#' @description
#' A function that facilitates reading multiple dbfs at once (Some lazy loading)
#'
#' @author Tingwei Adeck
#'
#' @param dbf_list A dbf list (usually list.files() if dbf files are found in the pwd)
#'
#' @return Returns DBF data frames to the R environment
#' @export
#'
#' @seealso [check_dbfs_pwd()], [read_dats()]
#'
#' @examples
#' \donttest{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' read_dbfs(list.files(getwd()))
#' }

read_dbfs <- function(dbf_list){

  dbf_files <- lapply(dbf_list,foreign::read.dbf)

  for(i in 1:length(dbf_files)){

    n <- "dbf"

    assign(paste0(n, i), as.data.frame(dbf_files[i]), envir = parent.frame())

  }

}


