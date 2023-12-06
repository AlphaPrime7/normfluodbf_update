resample_dat_alt_vect <- function(df, tnp, cycles){

  df_vector = as.vector(df)

  for (i in (1:length(df_vector)) ){
    list_len = i
    vec_len = length(df_vector[[i]])
  }
  vec_len
  list_len

  #all_vals = vec_len * length(df_vector)
  #k = df_vector[[j]] %>% .[c(j:tnp)]
  #vec_len

  kth = 1

  #k
  k = df_vector[[list_len]] %>% .[c(kth)]

  #resulting_vec[1:tnp] <- c(list(), 'NULL')
  resulting_vec <- vector(mode = 'list', length = tnp)

  for(j in 1:(vec_len/tnp)){

    for (l in 1:length(resulting_vec)){

      inc_tnp_by = tnp
      inc_kth_by = 1

      k = df_vector[[list_len]] %>% .[c(kth)]

      resulting_vec[[l]] = c(resulting_vec[[l]], k)

      #append(resulting_vec[j], k)

      kth = kth + inc_kth_by
      #tnp = tnp + inc_tnp_by

    }

  }

  return( as.data.frame(resulting_vec))
}
