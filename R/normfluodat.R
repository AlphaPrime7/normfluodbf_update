# Normfluodbf Update

#keep track that i had df be4
#also dat is the directory to the dat file
dat = Sys.getenv('first_dat')

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

# 2. Clean odd dats

clean_odddat <- function(df){
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

# 3. Comma cleaner

comma_cleaner <- function(comma_df){
  cols_to_becleaned <- c(colnames(comma_df))
  comma_df[ , cols_to_becleaned] <- lapply(comma_df[ , cols_to_becleaned],  # Convert data
                                           function(x){ as.numeric(as.character(gsub(",", "", x))) })

}

# 4. Resample Prototype-Users have control at the per column level (You are welcome)

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

# 5. Normalize

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# 6. Name dats

dat_col_names <- function(df, rows_used = NULL, cols_used= NULL){
  col_names <- c()
  if(is.null(cols_used)){
    for(j in n){
      for(i in 1:ncol(df)){
        col_names <- c(col_names, paste0(n,i))
      }
    }
    return(col_names[1:ncol(df)])
  } else {
    for(j in n){
      for(i in cols_used){
        col_names <- c(col_names, paste0(n,i))
      }
    }
    return(col_names[1:(length(cols_used)*length(n))])
  }
}

# 7. Unique_identifier

unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$Cycle_No[i] <- x
  }
  return(df)
}

# PARENT FUNCTION
# if you used all the plate columns then leave as null else specify
# same with rows
n = c('A','B','C')
normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL){

  library(data.table)

  df <- read.table(dat) #dat becomes df
  df <- clean_odddat(df)
  df <- comma_cleaner(df)
  df <- as.data.frame(df)

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

  cleaned_dat = do.call(rbind, j_vect)
  cleaned_dat = as.data.frame(cleaned_dat)
  cleaned_dat_t = transpose(l=cleaned_dat)

  #normalize
  cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

  #name the columns
  ru = rows_used
  cu = cols_used
  sample_col_names <- dat_col_names(cleaned_dat_t, ru, cu)
  colnames(cleaned_dat_t) <- sample_col_names

  #add unique_id
  cleaned_dat_t <-unique_identifier(cleaned_dat_t)

  suppressWarnings(return(cleaned_dat_t))
}

normalized_fluo_dat <- normfluodat(dat, tnp = 3, cycles = 40, n)

# VISUALIZER
#PLOT
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
    dark_mode()+  coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
    labs(title = 'NavAb Liposome Flux Assay',
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}

gg_plot_triplets(normalized_fluo_dat,x=xvar,y_list=yvars,xlim=xl,ylim=yl)
