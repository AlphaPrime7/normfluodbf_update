#use renv file for better code
usethis::edit_r_environ() #store wds for files
lipo_dat <- read.table(Sys.getenv('first_dat'))
lipo_dat2 <- read.table(Sys.getenv('scnd_dat'))

#UNIQUE IDENTIFIER
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$unique_id[i] <- x
  }
  return(df)
}

#FUNCTION 1- Pattern recognized
na_standard_dat <- function(df){
  for (i in (4 * (1:(nrow(df)/4)))){
    k <- seq(4)
    skip_values = 8 * seq(40)
    if(i %in% skip_values) next
    df[c(k+i,i),] <- NA
  }
  return(df)
}
na_dat <- na_standard_dat(lipo_dat)
nona_dat <- na.omit(na_dat)

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


#JUST SOME HARDCODE TESTING, GOING INTO THE FUNCTIONS AND PACKAGE SHORTLY

#lipo_dat
col_conv <- c(colnames(nona_dat))
col_conv
nona_updated <- nona_dat
nona_updated[ , col_conv] <- lapply(nona_updated[ , col_conv],  # Convert data
                                       function(x){ as.numeric(as.character(gsub(",", "", x))) })

dat_cycles <- unique_identifier(nona_updated)

#SUBSAMPLES NOT WORKING BECAUSE DATA IS JANKY
sub_samples <- dat_cycles[c(1:40),]
plot(sub_samples$unique_id, sub_samples$V1)
plot(sub_samples$unique_id, sub_samples$V2)

#lipo_dat2
col_lipo2 <- c(colnames(lipo_dat2))
lipo2_updated <- lipo_dat2
lipo2_updated[ , col_conv] <- lapply(lipo2_updated[ , col_conv],  # Convert data
                                          function(x){ as.numeric(as.character(gsub(",", "", x))) })
lipo2_updated[, c(1:ncol(lipo2_updated))] <- sapply(lipo2_updated[, c(1:ncol(lipo2_updated))], as.numeric)
is.numeric(lipo2_updated)
lipo2_clean <- na.omit(lipo2_updated)


## REARRANGE THE DATA INTO A USABLE FORMAT, NO COMPLICATED MOVES
## file = dat_cycles IS THE RIGHT FILE

dat_cycles <- as.data.frame(dat_cycles)
col_1 <- dat_cycles[,1]
col_1 <- as.data.frame(col_1)


#CLOSE TO THE UPDATE-# of cycles,# of samples
col_1c <- data.frame()
for(i in col_1){
  col_1c <- rbind(col_1c,i)
}

resample_dat_close <- function(df, tnp, cycles){

  sample_size <- c(1:cycles)
  type_size <- c(1:tnp) #tnp is the sample types (3 for all, 2 for tp-tn-np if fixing something)
  k <- c((1-tnp):(tnp-tnp)) # 1:3, 4:7, 115:117, 118:120 kth element < 120 based on standard setup

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/3)){

    increment = tnp #(tnp=3 for 3 samples; tnp =2 for 2 samples)
    k <- k + increment

    colnames(resulting_df) = NULL
    insert_row = df[k,]
    colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)


  }
  suppressWarnings(return(resulting_df))
}

trial1 <- resample_dat_close(col_1, tnp = 3, cycles = 40)


#tnp=testnegpos, cols = vector c(123)-match tnp, n = number of samples per group(tnp), cycles=chosen samples (40),
resample_redo <- function(df, cols, tnp, n, cycles){

  resulting_df <- data.frame()

  for (i in 1:(ncol(col_1c)/3)){
    for (j in tnp * seq(cycles/tnp)){

      colnames(resulting_df) = NULL
      insert_row = col_1c[,cols]
      print(insert_row)
      colnames(insert_row) = NULL
      resulting_df[i,cols] <- rbind(resulting_df, insert_row)
      print(resulting_df)

      increment = 3
      i = i + increment
      j = j + increment

      }
  }
  return(resulting_df)
}

resample_redo(col_1c, c(1:3), 3, 12, 40)

#BETTER TO GO FROM COLUMN TO ROWS INSTEAD
#3 in the loop is the # of sample types (pos,neg vs test)
#i can deduce your number of cycles from the setup and type of samples (advice that odd number samples are good for algos in this case)
# 39 cycles is easier to code than 40 cycles at least so far, still working on it
13
46
79
1012
1315
1618
1921
2224
2527
2830
3133
3436
3740
.
115117
118120
