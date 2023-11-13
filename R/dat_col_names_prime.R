#' Title: A function to obtain attribute names for experimental samples
#' @description
#' The function takes a clean data frame, data on the experiment and returns the column names that match the FLUOstar plate reader.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame obtained from the large scale delineation of samples.
#' @param rows_used A character vector representing the plate rows used; eg ru <- c('A','B','C'). can be used in sequence or out of sequence.
#' @param cols_used A numeric vector representing the plate columns used; eg cu <- c(1,2,3,4). keep as null if all the columns were used or columns are used in sequence.
#' @param user_specific_labels A character vector with specific sample labels based on the plate setup
#'
#'
#' @return Returns column names that will be added to the normalized data frame that contains all samples
#' @export
#' @note This function is a subordinate function and follows a sequence of actions. In this package, this function cannot be used as a standalone.
#' Also, some work is needed here on the part of the user because i have no access to their setup file.
#' A function that takes the setup excel file from the user should be part of the next update to prevent the user from doing much work.
#' The program is always going to need rows_used. The user can choose to specify columns used but typically if things are in sequence then everything should be fine.
#' The extreme case is an extreme unorthodox plate (hard to know when this will happen) and then the user must either specify rows used directly or the user is given a prompt by R to input rows used.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' n = c('A','B','C')
#' sample_col_names <- dat_col_names_prime(resampled_scaled, n)

dat_col_names_prime <- function(df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL){
  col_names <- c()
  if(!is.null(cols_used)){
    normal_sequence = c(min(cols_used):max(cols_used))
  } else {
    normal_sequence = NULL
  }

  if(!is.null(user_specific_labels)){
    return(user_specific_labels)

  } else if(is.null(cols_used)){
    for(i in 1:ncol(df)){
      col_names <- c(col_names, paste0(rows_used,i)) #normal (left to right start at position 1 of plate (A1))
    }
    return(col_names[1:ncol(df)])

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    return(col_names[1:ncol(df)])

  } else if(is.null(user_specific_labels) && ncol(df) < length(cols_used)*length(rows_used) || length(cols_used) < length(normal_sequence) ){
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(user_specific_labels)){
      print(col_names[1:(length(cols_used)*length(rows_used))])
      print('From the printed above list enter the columns used; must match the sample positions on the plate;')
      choose_cols_used=scan(what=character(), n=ncol(df))
      print(choose_cols_used)
      return(as.vector(choose_cols_used))
    } else{
      return(user_specific_labels)
    }
  }
}
