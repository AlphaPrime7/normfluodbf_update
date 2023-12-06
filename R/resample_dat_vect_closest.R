resample_vect_scale <- function(df, dat, tnp, cycles){

  #df <- df[,colSums(is.na(df))<nrow(df)]

  df_vector = as.vector(df)
  num_list = actual_cols_used(dat)
  num_list = c(1:length(num_list))

  total_list = c()
  for (i in (1:length(df_vector)) ){
    vec_len = length(df_vector[[i]])
    total_list = c(total_list, i)
  }
  vec_len
  total_list

  resulting_vec <- vector(mode = 'list', length = tnp * length(total_list) )
  kth = 1

  for (j in 1:ncol(df)){

    for(l in 1:(vec_len/tnp)){

      inc_kth_by = tnp
      k = df_vector[[j]] %>% .[c(kth)]
      kth = kth + inc_kth_by

      for (m in 1:length(resulting_vec)){

        resulting_vec[[m]] = c(resulting_vec[[m]], k)

      }

    }
    return(as.data.frame(resulting_vec))
    #return(resulting_vec)
  }
  #return(resulting_vec)

}

alt_test_scale <- resample_vect_scale(dat,test,3,40)
testvec = as.vector(test)
rapply(test[1:ncol(test)], resample_dat_alt_vect, tnp =3, cycles = 40, output = NULL)

resample_vect_scale <- function(df, tnp, cycles, method = c('normal','brute')){

  #df <- df[,colSums(is.na(df))<nrow(df)]
  df_vector = as.vector(df)

  total_list = c()
  for (i in (1:length(df_vector)) ){
    vec_len = length(df_vector[[i]])
    total_list = c(total_list, i)
  }
  vec_len
  total_list

  resulting_df = matrix()
  j_vect <- c()
  dfbs = data.frame() #yes bs= bullshit

  if( is.null(method) || 'normal' %in% method){
    for(j in df_vector){
      j = as.data.frame(j)
      resampled_df = resample_dat_alt_vect(j,tnp=tnp,cycles = cycles)
      colnames(resampled_df) = 'NULL'
      resulting_df = cbind(resulting_df, resampled_df)
    }

    resulting_df = resulting_df[,-1]
    resulting_df = as.data.frame(resulting_df)
    colnames(resulting_df) = c(1:ncol(resulting_df))
    return(resulting_df)

  } else {

    for(k in df_vector){
      k = as.data.frame(k)
      resampled_df = resample_dat_alt_vect(k,tnp=tnp,cycles = cycles)
      j_vect <- c(j_vect, resampled_df)
    }
    dfbs = do.call(rbind, j_vect)
    dfbs = as.data.frame(dfbs)
    dfbs = data.table::transpose(l = dfbs)
    return(dfbs)
  }

}

alt_test_scale <- resample_vect_scale(test,3,40, method = 'brute')
alt_test_scale <- resample_vect_scale(test,3,40, method = 'normal')
alt_test_scale <- resample_vect_scale(test,1,40, method = 'normal')
alt_test_scale <- resample_vect_scale(test,1,40, method = 'brute')



