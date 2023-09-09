#use renv file for better code
usethis::edit_r_environ() #store wds for files
lipo_dat <- read.table()
lipo_dat2 <- read.table()

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
