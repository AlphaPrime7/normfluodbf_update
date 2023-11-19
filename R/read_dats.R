#' Title: A function to read multiple DAT files into the R environment
#' @description
#' A function that facilitates reading multiple DAT files at once (Some lazy loading)
#'
#' @author Tingwei Adeck
#'
#' @param dat_list A DAT list (usually list.files() if DAT files are found in the pwd)
#'
#' @return Returns DAT data frames to the R environment
#' @export
#'
#' @seealso [read_dbfs()]
#'
#' @examples
#' \donttest{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' read_dats(list.files(getwd()))
#' }

read_dats <- function(dat_list){

  dat_files <- lapply(dat_list, utils::read.table)

  for(i in 1:length(dat_files)){

    n <- "dat"

    assign(paste0(n, i), as.data.frame(dat_files[i]), envir = parent.frame())

  }

}

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




