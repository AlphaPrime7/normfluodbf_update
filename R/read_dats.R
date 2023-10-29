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




