normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL){

  library(data.table)

  df <- read.table(dat)
  #df <- clean_odd_cc(df)
  df <- clean_odddat(df)
  check_max_fluor_na(df)
  check_max_fluor_raw(df)

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }


}
n <- c('A','B','C')
normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n)
normalized_fluo_dat2 <- normfluodat(dat, tnp = 3, cycles = 40, n,read_direction = 'horizontal')

normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL){

  library(data.table)

  df <- read.table(dat)
  #df <- clean_odd_cc(df)
  df <- clean_odddat(df)
  check_max_fluor_na(df)
  check_max_fluor_raw(df)

#Function revamp
if(is.null(dat)){
  warning("please enter a string for the .dat file you want to normalize")
  warning("User must provide tnp(# of rows), cycles, and vector for rows used")

} else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'raw'){
  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }

} else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }
} else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }
} else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }

} else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }

} else if (!is.null(file) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)

    return(df)

  }

} else if( !is.null(file) && !is.null(tnp) && !is.null(cycles) ){

  if(is.null(read_direction) || read_direction == 'vertical'){

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)
    #colnames(df) <- c(1:(ncol(df)-1))

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)
    #colnames(df) <- c(1:(ncol(df)-1))

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #name the columns
    ru = rows_used
    cu = cols_used
    usl = user_specific_labels
    rd = read_direction
    sample_col_names <- dat_col_names_prime(df, ru, cu, usl, rd)
    colnames(df) <- sample_col_names

    #add unique_id
    df <-unique_identifier(df)
    #colnames(df) <- c(1:(ncol(df)-1))

    return(df)

    }

  }

}

normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n)
normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n, read_direction = 'horizontal')
normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40)


#NORMFLUODAT_BASE
#' Title: Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#'
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#'
#' @author Tingwei Adeck
#'
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame with
#' samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1) OR “horizontal”
#' if the user intends to have a final data frame with samples arranged as clusters per sample type (A1, A2, A3, B1, B2, B3).
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale; Initialized as NULL.
#'
#' @import utils
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#'
#' @export
#'
#' @note This function is a single-step function leveraging several subordinate functions.
#' It is assumed that the user has the 3 baseline parameters to get this function working.
#' Users must double-check attribute names to ensure they end up with accurate results.
#'
#' @seealso [normfluordat()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluodat_base(dat=fpath, tnp = 3, cycles = 40)

normfluodat_base <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL){

  df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)


  #Function revamp
  if(is.null(dat) && is.null(tnp) && is.null(cycles)){
    warning("please enter the DAT file path or string, tnp(# of rows) & cycles")

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'raw'){
    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)
      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)
      }

    } else{

      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }
    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else{

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else{

      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }
    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else{
      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }
    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)
      }

    } else{
      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }
    }

  } else if (!is.null(dat) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name the columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)

        return(df)

      }

    } else{
      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        return(df)

      }
    }

  } else if( !is.null(dat) && !is.null(tnp) && !is.null(cycles) ){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      } else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(ncol(df) == 1){
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      } else {
        df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      }

    } else{
      if(ncol(df)==1){
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles) #needs to change in package
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles) #needs to change in package
        fluor_threshold_check(df) #needs to be changed in pkg
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #add unique_id
        df <-unique_identifier(df)
        #colnames(df) <- c(1:(ncol(df)-1))

        return(df)

      }

    }

  }

}
