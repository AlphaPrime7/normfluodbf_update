# Normfluodbf Update

dat = Sys.getenv('first_dat')
dat = Sys.getenv('third_dat')
dat = Sys.getenv('scnd_dat')
dat = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/dat/dat_6.dat"
dat = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/dat/dat_4.dat"
dat = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/dat/dat_5.dat"
dat = system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
lipo_dat <- read.table(dat)

#SUBORDINATE FUNCTIONS

# 1. Standalone sub function for dat files with an idied sequence but this is an anomaly so the function for nonseqdats is the only one used here

clean_evendat <- function(df){
  for (i in (4 * (1:(nrow(df)/4)))){
    k <- seq(4)
    skip_values = 8 * seq(40)
    if(i %in% skip_values) next
    df[c(k+i,i),] <- NA
  }
  df <- na.omit(df)
  return(df)
}

# 2. Clean odd dats (PRIME)

#dependency
comma_cleaner <- function(comma_df){

  cols_to_becleaned <- c(colnames(comma_df))
  comma_df[ , cols_to_becleaned] <- lapply(comma_df[ , cols_to_becleaned],  # Convert data
                                           function(x){ as.numeric(as.character(gsub(",", "", x))) })
  return(as.data.frame(comma_df))
}
comma_cleaner(test)

#from the package as a damaged good
clean_odddat <- function(df){
  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j]){
        df[i,j] <- NA
      }
    }
  }
  na_df <- df

  comma_df <- na_df
  nocomma_df <- comma_cleaner(comma_df)
  nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]

  return(nocomma_df)
}

clean_odddat <- function(df, tnp, cycles){
  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j]){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- df

  comma_df <- nona_rows_df
  nocomma_df <- comma_cleaner(comma_df)
  nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]
  rownames(nocomma_df) <- c(1:(tnp*cycles))

  return(nocomma_df)
}

clean_odddat <- function(df){

  special_chars <- c('-,','-' )
  empty_df <- data.frame()
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if( special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j] ){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- df


  if(ncol(df) == 1){
    comma_df <- nona_rows_df
    comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]
    comma_df = as.numeric(as.character(gsub(",", "", comma_df)))

    return(as.data.frame(comma_df))
  } else {
    comma_df <- nona_rows_df
    nocomma_df <- comma_cleaner(comma_df)
    nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]
    nocomma_df <- as.data.frame(nocomma_df)

    return(nocomma_df)
  }

}

clean_odddat <- function(df,tnp,cycles){

  special_chars <- c('-,','-' )
  empty_df <- data.frame()
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if( special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j] ){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- df


  if(ncol(df) == 1){
    comma_df <- nona_rows_df
    comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]
    comma_df = as.numeric(as.character(gsub(",", "", comma_df)))
    comma_df = as.numeric(comma_df)

    return(as.data.frame(comma_df))
  } else {
    comma_df <- nona_rows_df
    nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]
    nocomma_df <- comma_cleaner(comma_df)
    #nocomma_df <- as.numeric(nocomma_df)
    nocomma_df <- as.data.frame(nocomma_df)

    return(nocomma_df)
  }

}

#test
test <- clean_odddat(lipo_dat)
test <- clean_odddat(lipo_dat)

#2.5
actual_cols_used <- function(df){
  colnames(df) <- c(1:ncol(df))
  acu <- names(which(colSums(!is.na(df)) > 0))
  acu <- as.numeric(as.vector(acu))

  return(acu)

}
#test
acutest <- actual_cols_used(test)


for(i in 1:nrow(test)){
  for (j in 1:ncol(test)) {
    if(j == 1){
      test[3,j] <- NA
    }

  }
}

# 3.

clean_odd_cc <- function(df){

  df <- comma_cleaner(df)
  df <- df[rowSums(is.na(df)) != ncol(df), ]

  return(as.data.frame(df))

}

test <- clean_odd_cc(lipo_dat)

# 3.5

resample_dat_alt <- function(df, tnp, cycles, samples_per_tnp=NULL){

  k <- c(1:tnp)
  type_size <- c(1:tnp)

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    insert_row = df[k,]

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)

    increment_k = tnp
    k <- k + increment_k

    #colnames(resulting_df) <- NULL

  }
  return(resulting_df)
}
alt_test <- resample_dat_alt(test,3,40)

resample_dat <- function(df, tnp, cycles){

  type_size <- c(1:tnp)
  k <- c(1:tnp)

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    #colnames(resulting_df) = NULL
    insert_row = df[k,]
    #colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df, showWarnings = FALSE)

    increment = tnp
    k <- k + increment


  }
  return(resulting_df)
}

sc_resample <- resample_dat(test,3,40)


# 4.

resample_dat_scale <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df <- resample_dat(df,tnp,cycles)
      return(sc_df)

    } else {

      type_size <- c(1:tnp) #k is now nseq(same kinda thing)


      sample_df <- data.frame()
      final_df <- matrix(ncol = cycles)

      for(i in 1:ncol(df)){
        nseq <- c(1:tnp)

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,i]
          sample_df[j,type_size] <- rbind(insert_row, sample_df, na.omit=FALSE)
          increment_n = tnp
          nseq <- nseq + increment_n

        }
        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(!is.na(final_df))>=1] #retain nas
        #rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
        colnames(final_df) <- NULL

      }
      #return(sample_df)
      return(final_df)

    }

  })
}

test_scale <- resample_dat_scale(test, tnp = 1, cycles = 40)
test_scale <- resample_dat_scale(test, tnp = 3, cycles = 40)
test_scale <- as.data.frame(lapply(test_scale[1:ncol(test_scale)], min_max_norm_percent))
alt_test_wow <- resample_dat_scale(col_1,3,40)

resample_dat_scale <- function(df, tnp, cycles){
  library(data.table)
  library(dplyr)

  col_list <- c()
  for(i in 1:ncol(df)){
    col_list <- c(col_list, as.data.frame(df[,i]) )
  }

  j_vect <- c()
  for(j in col_list){
    j <- as.data.frame(j)
    j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
    j_dfs <- as.data.frame(j_resampled)
    j_vect <- c(j_vect, j_dfs)
    #j_vect <- c(j_vect, j_dfs)
  }

  big_data = do.call(rbind, j_vect)
  big_data = as.data.frame(big_data)
  big_data_t = transpose(l=big_data)

  #big_data_t <- big_data_t %>% select_if(~ !any(is.na(.)))
  big_data_t <- big_data_t[ , colSums(is.na(big_data_t))==0]
  big_data_t <- as.data.frame(big_data_t)

  return(big_data_t)
}

#NA retainer for update
resample_dat_scale_naretainer <- function(df, tnp, cycles){
  library(data.table)
  library(dplyr)

  col_list <- c()
  for(i in 1:ncol(df)){
    col_list <- c(col_list, as.data.frame(df[,i]) )
  }

  j_vect <- c()
  for(j in col_list){
    j <- as.data.frame(j)
    j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
    j_dfs <- as.data.frame(j_resampled)
    j_vect <- c(j_vect, j_dfs)
    #j_vect <- c(j_vect, j_dfs)
  }

  big_data = do.call(rbind, j_vect)
  big_data = as.data.frame(big_data)
  big_data_t = transpose(l=big_data)

  big_data_t <- big_data_t[,which(!apply(big_data_t,2,FUN = function(x){all(x == 0)}))]
  #big_data_t <-  big_data_t[, !sapply(big_data_t, function(x) all(x == 0))]
  #big_data_t <- big_data_t[ , colSums(!is.na(big_data_t))>=1]
  big_data_t <- as.data.frame(big_data_t)

  return(big_data_t)
}

test_scale_stand <- resample_dat_scale_naretainer(test, tnp = 3, cycles = 40)


#ALT-bf_na
resample_dat_scale_alt_cpuint <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat_alt(df,tnp,cycles)
      return(df1)

    } else {

      row_list <- c()
      for(i in 1:nrow(df)){
        row_list <- c(row_list, as.data.frame(df[i,]) )
      }

      j_vect <- c()
      for(j in row_list){
        j <- as.data.frame(j)
        j_resampled <- resample_dat_alt(j, tnp = tnp, cycles = cycles)
        j_dfs <- as.data.frame(j_resampled)
        j_vect <- c(j_vect, j_dfs)
      }

      big_data = do.call(rbind, j_vect)
      big_data = as.data.frame(big_data)
      big_data_t = transpose(l=big_data)

      big_data_t <- as.data.frame(big_data_t)
      colnames(big_data_t) <- NULL

      num_samples = ncol(big_data_t)/cycles
      numrows = ncol(big_data_t)/num_samples
      k <- c(1:num_samples)
      type_size <- c(1:num_samples)

      resulting_df <- data.frame()
      for (i in 1:numrows){

        insert_col = big_data_t[,k]
        colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        colnames(resulting_df) <- c(1:num_samples)

      }

      resulting_df <- resulting_df %>% dplyr::select_if(~ !any(is.na(.)))
      #resulting_df <- resulting_df[ , colSums(!is.na(resulting_df))>=1]
      return(resulting_df)


    }

  })

}

test_scale <- resample_dat_scale_alt_cpuint(test, tnp = 3, cycles = 40)
test_scale_comp <- resample_dat_scale_alt_cpuint(test, tnp = 1, cycles = 40)

#NONE-OUT OF PKG
resample_dat_scale_altv2 <- function(df, tnp, cycles){

  suppressWarnings({
    library(data.table)
    library(dplyr)

    row_list <- c()
    for(i in 1:nrow(df)){
      row_list <- c(row_list, as.data.frame(df[i,]) )
    }
    row_list = as.data.frame(row_list)
    big_data_t <- row_list
    colnames(big_data_t) <- NULL

    num_samples = ncol(big_data_t)/(cycles)
    numrows = ncol(big_data_t)/(num_samples)

    k <- c(1:num_samples)
    type_size <- c(1:num_samples)

    resulting_df <- data.frame()
    for (i in 1:numrows){

      if(ncol(df)==1){
        insert_col = big_data_t[,k]
        #colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        #colnames(resulting_df) <- c(1:num_samples)
      } else {
        insert_col = big_data_t[,k]
        colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        colnames(resulting_df) <- c(1:num_samples)

      }
    }

    resulting_df <- resulting_df[ , colSums(!is.na(resulting_df))>=1]
    #resulting_df <- resulting_df %>% select_if(~ !any(is.na(.)))
    return(as.data.frame(resulting_df))

  })

}
test_scale <- resample_dat_scale_altv2(test, tnp = 3, cycles = 40)
test_scale_comp <- resample_dat_scale_altv2(test, tnp = 1, cycles = 40)

#bfv
resample_dat_scale_altv1 <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat_alt(df,tnp,cycles)
      return(df1)

    } else{

      row_list <- c()
      for(i in 1:nrow(df)){
        row_list <- c(row_list, as.data.frame(df[i,]) )
      }
      row_list = as.data.frame(row_list)
      big_data_t <- row_list
      colnames(big_data_t) <- NULL

      num_samples = ncol(big_data_t)/(cycles)
      numrows = ncol(big_data_t)/(num_samples)

      k <- c(1:num_samples)
      type_size <- c(1:num_samples)

      resulting_df <- data.frame()
      for (i in 1:numrows){

        insert_col = big_data_t[,k]
        colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        colnames(resulting_df) <- c(1:num_samples)

      }

      resulting_df <- resulting_df %>% dplyr::select_if(~ !any(is.na(.)))
      return(as.data.frame(resulting_df))

    }

  })

}

test_scale <- resample_dat_scale_altv1(test, tnp = 3, cycles = 40)
test_scale_comp <- resample_dat_scale_altv1(test, tnp = 1, cycles = 40)


resample_dat_scale_alt <- function(df, tnp, cycles){

  if(ncol(df) == 1){
    sc_df = resample_dat_alt(df,tnp,cycles)
    return(sc_df)

  } else {
    type_size <- c(1:ncol(df)) #k is now nseq(same kinda thing)

    sample_df <- data.frame()
    final_df <- matrix(ncol = cycles)

    for(i in 1:tnp){
      nseq <- c(i:ncol(df))
      nseq <- nseq

      for (j in 1:(nrow(df)/tnp)){

        insert_row = df[nseq,]
        sample_df[j,type_size] <- rbind(insert_row, sample_df, na.omit=FALSE)
        increment_n = tnp
        nseq <- nseq + increment_n

      }
      final_df <- cbind(final_df, sample_df)
      final_df <- final_df[ , colSums(!is.na(final_df))>=1]
      #final_df <- final_df[ , colSums(is.na(final_df))==0]
      colnames(final_df) <- NULL
      rownames(final_df) <- c(1:cycles)
      final_df <- as.data.frame(final_df)

    }
    #return(sample_df)
    return(final_df)

  }

}
resamp_demo <- resample_dat_scale_alt(test,1,40)

# 5.

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
    cols_used = acutest
    for(i in cols_used){
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

dat_col_names_prime <- function(df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL){

  if(is.null(rows_used)){
    warning('user must enter rows_used which is a character vector with length == tnp')
  }

  col_names <- c()
  col_names_sort <- c()
  if(!is.null(cols_used)){
    normal_sequence = c(min(cols_used):max(cols_used))
  } else {
    normal_sequence = NULL
  }

  if(!is.null(user_specific_labels)){
    return(user_specific_labels)

  } else if(is.null(cols_used)){
    for(i in 1:ncol(df)){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      message('check data frame for NA column or last sample column names with unmatched or isolated samples')
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      for(i in 1:(ncol(df)/length(rows_used)) ){
        col_names_sort <- c(col_names_sort, paste0(rows_used,i))
      }
      col_names_sort <- stringr::str_sort(col_names_sort, decreasing = F, na_last = T, locale = 'en', numeric = T)
      message('check data frame for NA column names or last sample column with unmatched or isolated samples')
      return(col_names_sort[1:ncol(df)])
    }

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      return(col_names_sort[1:ncol(df)])
    }
  } else if(is.null(user_specific_labels) || ncol(df) < length(cols_used)*length(rows_used) && length(cols_used) < length(normal_sequence) ){
    if(ncol(df) > length(cols_used)*length(rows_used)){
      message('number of columns exceeds users estimate; try leaving the cols_used blank')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }

    }else {
      message('number of columns is lower than the users estimate; select the columns used from the list below')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }
    }

  }
}


check_dtc <- dat_col_names_prime(test, c('A','B','C'))
check_dtc <- dat_col_names_prime(test_scale, c('A','B','C'), c(1,2,3,4,5,6,7,8,9,10,11,12))
check_dtc

#NEW ONE
dat_col_names_prime <- function(decoy, df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL){

  acutest <- actual_cols_used(decoy)
  if(is.null(rows_used)){
    warning('user must enter rows_used which is a character vector with length == tnp')
    colnames_nocu <- c(1:ncol(df))
    return(colnames_nocu)
  }

  col_names <- c()
  col_names_sort <- c()
  if(!is.null(cols_used)){
    normal_sequence = c(min(cols_used):max(cols_used))
  } else {
    normal_sequence = NULL
  }

  if(!is.null(user_specific_labels)){
    return(user_specific_labels)

  } else if(is.null(cols_used)){
    cols_used = acutest
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      message('check data frame for NA column or last sample column names with unmatched or isolated samples')
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      #for(i in 1:(ncol(df)/length(rows_used)) ){
      #col_names_sort <- c(col_names_sort, paste0(rows_used,i))
      #}
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      message('check data frame for NA column names or last sample column with unmatched or isolated samples')
      return(col_names_sort[1:ncol(df)])
    }

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      return(col_names_sort[1:ncol(df)])
    }
  } else if(is.null(user_specific_labels) || ncol(df) < length(cols_used)*length(rows_used) || ncol(df) < length(acutest)*length(rows_used) && length(cols_used) < length(normal_sequence) ){
    if(ncol(df) > length(cols_used)*length(rows_used)){
      message('number of columns exceeds users estimate; try leaving the cols_used blank')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }

    }else {
      message('number of columns is lower than the users estimate; select the columns used from the list below')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }
    }

  } else {
    colnames_nocu <- c(1:ncol(df))
    return(colnames_nocu)
  }
}


check_dtc <- dat_col_names_prime(decoy= test, test, c('A','B','C'))
check_dtc <- dat_col_names_prime(decoy= test, test)
check_dtc <- dat_col_names_prime(test_scale, c('A','B','C'), c(1,2,3,4,5,6,7,8,9,10,11,12))
check_dtc

dat_col_names_prime <- function(df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL){

  if(is.null(rows_used)){
    message('User is advised to input a vector of rows used')
    colnames_noru <- c(1:ncol(df))
    return(colnames_noru)
  }

  col_names <- c()
  col_names_sort <- c()
  if(!is.null(cols_used)){
    normal_sequence = c(min(cols_used):max(cols_used))
  } else {
    normal_sequence = NULL
  }

  if(!is.null(user_specific_labels)){
    return(user_specific_labels)

  } else if(is.null(cols_used)){
    for(i in 1:ncol(df)){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      message('check data frame for NA column or last sample column names with unmatched or isolated samples')
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      for(i in 1:(ncol(df)/length(rows_used)) ){
        col_names_sort <- c(col_names_sort, paste0(rows_used,i))
      }
      col_names_sort <- stringr::str_sort(col_names_sort, decreasing = F, na_last = T, locale = 'en', numeric = T)
      message('check data frame for NA column names or last sample column with unmatched or isolated samples')
      return(col_names_sort[1:ncol(df)])
    }

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      return(col_names_sort[1:ncol(df)])
    }
  } else if(is.null(user_specific_labels) || ncol(df) < length(cols_used)*length(rows_used) && length(cols_used) < length(normal_sequence) ){
    if(ncol(df) > length(cols_used)*length(rows_used)){
      message('number of columns exceeds users estimate; try leaving the cols_used blank')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:ncol(df)] )
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }

    }else {
      message('number of columns is lower than the users estimate; select the columns used from the list below')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:(length(cols_used)*length(rows_used))])
        print('From the printed above list enter the columns used; must match the sample positions on the plate;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }
    }

  }
}

dat_col_names_horizontal <- function(df, rows_used=NULL,cols_used=NULL){

  cols_sort <- c()
  cols_sort_fit <- c()

  if(!is.null(cols_used) && !is.null(rows_used)){

    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,cols_used))
    }

    range <- 1:(length(df)/length(rows_used) )

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && !is.null(rows_used)){

    colnames_nocu <- c(1:ncol(df) )
    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,colnames_nocu))
    }

    range <- 1:( length(cols_sort)/ (length(rows_used)*length(rows_used)) )

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && is.null(rows_used)){

    colnames_nocu <- c(1:ncol(df))
    return(colnames_nocu)
  }

}

#MODIFIED FOR THE BS REWARD OF KNOWING HOW TO GET THE COLS
dat_col_names_horizontal <- function(df, rows_used=NULL,cols_used=NULL){

  cols_sort <- c()
  cols_sort_fit <- c()

  if(!is.null(cols_used) && !is.null(rows_used)){

    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,cols_used))
    }

    range <- 1:(length(df)/length(rows_used) )

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && !is.null(rows_used)){

    #colnames_nocu <- c(1:ncol(df) )
    colnames_nocu <- acutest
    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,colnames_nocu))
    }

    range <- 1:( length(cols_sort)/ length(rows_used))

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && is.null(rows_used)){

    colnames_nocu <- c(1:ncol(df))
    return(colnames_nocu)
  }

}
n <- c('A','B','C','D')
n <- c('A','B','C')
q <- c(1,2,3,4,5,6,7,8,9,10,11,12)
test_horlabel <- dat_col_names_horizontal(test_scale,n)
test_horlabel <- dat_col_names_prime(test_scale,n)
test_verlabel <- dat_col_names_prime(test_scale,n)
colnames(test_scale) <- test_verlabel
test_horlabel
# 6.

#check proper resampled
check_max_fluor <- function(clean_df, fun = NA){
  library(emojifont)
  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  clean_df <- rbind(clean_df, NA)
  clean_df <- cbind(clean_df, NA)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if ( clean_df[i,j] >= (2^15) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel')))
        message(paste("YIKES, value > 2^15, Watch in future experimental designs",'column:', j , 'row:', i))
      } else if ( clean_df[i,j] <= (2^11) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel'), emoji('lion')))
        message(paste("YIKES, value < 2^11, Watch in future experimental designs",'column:', j , 'row:', i))
      }
    }
  }
}
check_max_fluor(test_scale)
check_max_fluor(test)

#checks for NAs
check_max_fluor_na <- function(clean_df, fun = NA){
  library(emojifont)
  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  #clean_df <- rbind(clean_df, NA)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] <= 2^15 || clean_df[i,j] >= 2^11)  && is.na(clean_df[i,j]) != nofun ){
        #nothing
      } else{
        warning("Crikee, some values in your original data are NA values")
        warning(c(emoji('pig'), emoji('camel')))
        warning(paste(j,i))
      }
    }
  }
}
check_max_fluor_na(test)

#checks for thresholds in original
check_max_fluor_raw <- function(clean_df, fun = NA){
  library(emojifont)
  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  #clean_df <- rbind(clean_df, NA)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] >= 2^15 || clean_df[i,j] <= 2^11)  && is.na(clean_df[i,j]) != nofun ){
        warning(c(emoji('pig'), emoji('camel')))
        warning("Crikee, some values in your original data violate thresholds")
        warning(paste(j,i))
      } else{
        #nothing
      }
    }
  }

}
check_max_fluor_raw(test)

for(i in 1:ncol(test_scale)){
  for(j in 1:nrow(test_scale)){
    if(i ==2  && j ==3){
      test_scale[j,i] <- 2^16
    }
  }
}

for(i in 1:ncol(test_scale)){
  for(j in 1:nrow(test_scale)){
    if(i ==2  && j ==4){
      test_scale[j,i] <- 2^11
    }
  }
}


# 7.

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_norm_percent <- function(x){
  ((x - min(x)) / (max(x) - min(x))) * 100
}

norm_z <- function(x){
  (x - mean(x)) / sd(x)
}

decimal_scaling <- function(x){
  max_abs <- max(abs(x))
  power <- ceiling(log10(max_abs))
  x/(10^power)
}

roundfluor <- function(x){
  round(x, 3)
}

# 9. Unique_identifier

unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$Cycle_No[i] <- x
  }
  return(df)
}
test_scale <- unique_identifier(test_scale)

# 8. PARENT

normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL){


  df <- read.table(dat) #dat becomes df
  df <- clean_odddat(df)

  cleaned_dat_t <- resample_dat_scale_alt(df,tnp=tnp,cycles=cycles)
  check_max_fluor(cleaned_dat_t)

  #normalize
  cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

  #name the columns
  ru = rows_used
  cu = cols_used
  usl = user_specific_labels
  sample_col_names <- dat_col_names_prime(cleaned_dat_t, ru, cu, usl)
  colnames(cleaned_dat_t) <- sample_col_names

  #add unique_id
  cleaned_dat_t <-unique_identifier(cleaned_dat_t)

  return(cleaned_dat_t)
}
n <- c("A","B","C")
normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n)

# 9.

yvars <- c("A1","B1","C1")
xvar <- c("Cycle_No")
color <- c("Test","Negative Control","Positive Control")
xl <- c(0,40)
yl <- c(0,1)
#shinyapp version 1 ).DATA[[]] VERSION
library(ggplot2)
library(ggthemes)
library(ggdark)
gg_plot_triplets <- function(df, x, y_list, xlim, ylim){
  ggplot(df, aes(x=.data[[x[1]]])) +
    geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
    geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
    geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
    geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
    geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
    geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
    dark_mode()+
    labs(title = 'NavAb Liposome Flux Assay',
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}

gg_plot_triplets(normalized_fluo_dat,x=xvar,y_list=yvars,xlim=xl,ylim=yl)
gg_plot_triplets(test_scale,x=xvar,y_list=yvars,xlim=xl,ylim=yl)

# 'Cycle_no'
# 'Normalized Fluorescence'
# 'NavAb Liposome Flux Assay'
library(ggplot2)
gg_plot_triplets <- function(df, x=NULL, y_list=NULL, xlim, ylim){

  if(is.null(x) && is.null(y_list)){
    print('Enter the X-variable usually the cycle number')
    x_var = scan(what=character(), n=1)

    print('Enter the test sample')
    test = scan(what=character(), n=1)

    print('Enter the negative control')
    nc = scan(what=character(), n=1)

    print('Enter the positive control')
    pc = scan(what=character(), n=1)

    print('Enter the x-label')
    x_lab = scan(what=character(), n=1)
    print('Enter the y-label')
    y_lab = scan(what=character(), n=1)
    print('Enter the plot title')
    titlef = scan(what=character(), n=1)

    ggplot(df, aes(x=.data[[x_var]])) +
      geom_point(aes(y=.data[[test]]), size=3, color="blue") +
      geom_point(aes(y=.data[[nc]]), size=3, color="red") +
      geom_point(aes(y=.data[[pc]]), size=3, color="green") +
      geom_line(aes(y=.data[[test]], color="Test"), size = 0.8) +
      geom_line(aes(y=.data[[nc]], color="Negative Control"),size = 0.8) +
      geom_line(aes(y=.data[[pc]], color="Positivetive Control"), size = 0.8) +
      coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
      labs(title = titlef,
           x = x_lab , y= y_lab, color='Sample Type (TNP)')
  } else{
    print('Enter the x-label')
    x_lab = scan(what=character(), n=1)
    print('Enter the y-label')
    y_lab = scan(what=character(), n=1)
    print('Enter the plot title')
    titlef = scan(what=character(), n=1)

    ggplot(df, aes(x=.data[[x[1]]])) +
      geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
      geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
      geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
      geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
      geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
      geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
      coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
      labs(title = titlef,
           x = x_lab , y= y_lab, color='Sample Type (TNP)')

  }

}

yvars <- c("logA1","logB1","logC1")
yvars <- c("A1","B1","C1")
xvar <- c("Cycle_No")
color <- c("Test","Negative Control","Positive Control")
xl <- c(0,40)
yl <- c(0,1)
yl_log <- c(-2,0)
gg_plot_triplets(trial1_scale,x=xvar,y_list=yvars,xlim=xl,ylim=yl)
gg_plot_triplets(trial1_scale,xlim=xl,ylim=yl)
