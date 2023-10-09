#use renv file for better code
usethis::edit_r_environ() #store wds for files
dat = Sys.getenv('first_dat')
dat2 = Sys.getenv('scnd_dat')
lipo_dat <- read.table(dat)
lipo_dat2 <- read.table(dat2)


na_standard_dat <- function(df){
  for (i in (4 * (1:(nrow(df)/4)))){
    k <- seq(4)
    skip_values = 8 * seq(40)
    if(i %in% skip_values) next
    df[c(k+i,i),] <- NA
  }
  df <- na.omit(df)
  return(df)
}
nona_dat <- na_standard_dat(lipo_dat)


#FUNCTION 2-Clean dat files no matter the setup
clean_odd_dat <- function(df){
  library(dplyr)
  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,] && special_chars[2] %in% df[i,]){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- na.omit(df)

  for (i in 1:nrow(nona_rows_df)){
    for (j in 1:ncol(nona_rows_df)){
      if(special_chars[1] %in% nona_rows_df[,j] || special_chars[2] %in% nona_rows_df[,j]){
        nona_rows_df[i,j] <- NA
      }
    }
  }

  comma_df <- nona_rows_df %>% select_if(~ !any(is.na(.)))

  return(comma_df)
}



lipo1_test <- clean_odd_dat(lipo_dat)
lipo2_test <- clean_odd_dat(lipo_dat2)

#comma cleaner
commaclean_test <- comma_cleaner(lipo2_test)
commaclean_test <- as.data.frame(commaclean_test)

#PROTOTYPE

#original adjusting k to account for the position of the increment statement

resample_datv2 <- function(df, tnp, cycles){

  type_size <- c(1:tnp)
  k <- c((1-tnp):(tnp-tnp)) # 1:3, 4:7, 115:117, 118:120 kth element < 120 based on standard setup

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    increment = tnp #(tnp=3 for 3 samples; tnp =2 for 2 samples)
    k <- k + increment

    colnames(resulting_df) = NULL
    insert_row = df[k,]
    colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)


  }
  suppressWarnings(return(resulting_df))
}

trial1 <- resample_datv2(col1_80, tnp = 2, cycles = 40)
trial2 <- resample_datv2(col1_80, tnp = 2, cycles = 40)

#remix-moving the increment statement
resample_dat <- function(df, tnp, cycles, samples_per_tnp){

  type_size <- c(1:tnp)
  k <- c(1:tnp)

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    colnames(resulting_df) = NULL
    insert_row = df[k,]
    colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)

    increment = tnp
    k <- k + increment


  }
  suppressWarnings(return(resulting_df))
}

trial1r <- resample_dat(col1_80, tnp = 2, cycles = 40, samples_per_tnp = 12)
trial2r <- resample_dat(col1_80, tnp = 2, cycles = 40, samples_per_tnp = 12)


#SCALE UP NOW

#BUILD ON RESAMPLE_DAT-ITS THERE JUST FINAL TOUCHES OR NORMFLUODAT
resample_dat_scale <- function(df, tnp, cycles){
  library(data.table)

  col_list <- c()
  for(i in 1:ncol(df)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(df[,i])) )
  }

  j_vect <- c()
  for(j in col_list){
    j <- as.data.frame(j)
    j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
    j_dfs <- as.data.frame(j_resampled)
    j_vect <- c(j_vect, j_dfs)
  }

  big_data = do.call(rbind, j_vect)
  big_data = as.data.frame(big_data)
  big_data_t = transpose(l=big_data)

  suppressWarnings(return(big_data_t))
}

trial1_scale <- resample_dat_scale(dat_nocycles, tnp = 3, cycles = 40)

#NAME COLUMNS-FINAL MOMENTS
n <- c('A','B','C')

dat_col_names <- function(df, rows_used = NULL, cols_used= NULL){
  col_names <- c()
  if(is.null(cols_used)){
    for(j in n){
      for(i in 1:ncol(trial1_scale)){
        col_names <- c(col_names, paste0(n,i))
      }
    }
    return(col_names[1:ncol(trial1_scale)])
  } else {
    for(j in n){
      for(i in cols_used){
        col_names <- c(col_names, paste0(n,i))
      }
    }
    return(col_names[1:(length(cols_used)*length(n))])
  }
}


trial1_scale <- as.data.frame(lapply(trial1_scale[1:ncol(trial1_scale)], min_max_norm))

sample_col_names <- dat_col_names(trial1_scale, n)
sample_col_names_spec <- dat_col_names(trial1_scale, n, c(2,4))
sample_col_names
sample_col_names_spec

colnames(trial1_scale) <- sample_col_names


#NORMALIZE
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#UNIQUE IDENTIFIER
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$unique_id[i] <- x
  }
  return(df)
}


#PLOT
#shinyapp version 1 ).DATA[[]] VERSION
GG_plot_triplets <- function(df, x, y_list, xlim, ylim){
  ggplot(df, aes(x=.data[[x[1]]])) +
    geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
    geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
    geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
    geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
    geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
    geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
    dark_mode()+  coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
    labs(title = 'NavAb Liposome Flux Assay',
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}

GG_plot_triplets(wrangled_dbf2,x=xvar,y_list=test,xlim=xl,ylim=yl)
