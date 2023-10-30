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

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))

    return(df)

  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))

    return(df)

  } else{

    df <- resample_dat_scale(df, tnp = tnp, cycles = cycles) #needs to change in package
    check_max_fluor(df) #needs to be changed in pkg
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))

    return(df)

    }

  }

}

normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n)
normalized_fluo_dat2 <- normfluodat(dat, tnp = 3, cycles = 40, n,read_direction = 'horizontal')
normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40)


