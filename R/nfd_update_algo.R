#Figured out the algo for the dat data cleanup
#1. extremely janky data and will need some old code but some new stuff to really get the data cleaned
#up and accurate.
#2. The Algo summary:
# turns out for every 3 tuples in a column, it needs to be converted into 1 row, to represent 3 samples
A1 B1 C1

so as such

v1
1
2
3
4
5
6

becomes (depending on your experimental setup)

A1 -- B1 -- C1
1     2     3
4     5     6

#UPDATES AND FAILS
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

#what i thought will be a good option for cleaning the dat file properly
col_1c <- data.frame()
for(i in col_1){
  col_1c <- rbind(col_1c,i)
}

#v2 prototype
resample_datv2 <- function(df, tnp, cycles){

  type_size <- c(1:tnp)
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

trial1 <- resample_datv2(col_1, tnp = 3, cycles = 40)
trial2 <- resample_datv2(col_2, tnp = 3, cycles = 40)

#sample full dat file (SCALE UP)
resample_dat_scale <- function(df, tnp, cycles, samples_per_tnp){

  col_list <- c()
  for(i in 1:ncol(dat_nocycles)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(dat_nocycles[,i])) )
  }

  sample_size <- c(1:(tnp*samples_per_tnp))
  type_size <- c(1:tnp) #ignore for now
  k <- c(1:tnp) # 1:3, 4:7, 115:117, 118:120 kth element < 120 based on standard setup

  resulting_df_scale <- data.frame()
  for (i in 1:(nrow(df)/3)){
    for(j in col_list){

      colnames(resulting_df_scale) = NULL
      df_sub_coerce = as.data.frame(j)
      insert_row = df_sub_coerce[k,]
      colnames(insert_row) = NULL

      resulting_df_scale[i,sample_size] <- rbind(insert_row, resulting_df_scale)

      increment = tnp
      k <- k + increment
      type_size <- type_size + increment

    }
  }
  suppressWarnings(return(resulting_df_scale))
}

trial1_scale <- resample_dat_scale(dat_nocycles, tnp = 3, cycles = 40, samples_per_tnp = 12)


sample_size <- c(1:(tnp*samples_per_tnp))
type_size <- c(1:tnp)
cycle_range <- c(1:cycles)

increment = tnp
type_size <- type_size + increment


#original
resample_dat_scale <- function(df, tnp, cycles, samples_per_tnp){

  col_list <- c()
  for(i in 1:ncol(dat_nocycles)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(dat_nocycles[,i])) )
  }

  sample_size <- c(1:(tnp*samples_per_tnp))
  type_size <- c(1:tnp) #ignore for now
  k <- c(1:tnp) # 1:3, 4:7, 115:117, 118:120 kth element < 120 based on standard setup

  resulting_df_scale <- data.frame()
  for (i in 1:(nrow(df)/3)){
    for(j in col_list){

      colnames(resulting_df_scale) = NULL
      df_sub_coerce = as.data.frame(j)
      insert_row = df_sub_coerce[k,]
      colnames(insert_row) = NULL

      resulting_df_scale[i,type_size] <- rbind(resulting_df_scale, insert_row)

      increment = tnp
      k <- k + increment
      type_size <- type_size + increment

    }
  }
  suppressWarnings(return(resulting_df_scale))
}

trial1_scale <- resample_dat_scale(dat_nocycles, tnp = 3, cycles = 40, samples_per_tnp = 12)

#remix ensuring that the outcome is as expected
resample_dat_scaler <- function(df, tnp, cycles, samples_per_tnp){

  sample_size <- c(1:(tnp*samples_per_tnp))
  type_size <- c((1-tnp):(tnp-tnp)) #tnp is the sample types (3 for all, 2 for tp-tn-np if fixing something)
  k <- c((1-tnp):(tnp-tnp)) # 1:3, 4:7, 115:117, 118:120 kth element < 120 based on standard setup

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/3)){
    for(j in 1:ncol(df)){

      increment = tnp
      k <- k + increment
      type_size <- type_size + increment

      colnames(resulting_df) = NULL
      insert_row = df[k,]
      colnames(insert_row) = NULL

      resulting_df[i,sample_size] <- rbind(insert_row, resulting_df)

    }
  }
  suppressWarnings(return(resulting_df))
}

trial1_scale <- resample_dat_scaler(dat_nocycles, tnp = 3, cycles = 40, samples_per_tnp = 12)

rm(col_list)
# MUST WORK
resample_dat_scalev2 <- function(df, tnp, cycles){

  col_list <- c()
  for(i in 1:ncol(dat_nocycles)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(dat_nocycles[,i])) )
  }

  resulting_df_scale <- data.frame()

  samples_delineated <- list()
  for(j in 1:length(col_list)){
    col_list[j] <- as.data.frame(col_list[j])
    j_resampled <- resample_dat(col_list[j], tnp = tnp, cycles = cycles)
    col_list$j <- j
    samples_delineated[[j]] <- j_resampled
    print(samples_delineated)
  }

  dat_cycles_fixed = do.call(rbind, samples_delineated)

  suppressWarnings(return(dat_cycles_delineated))
}

trial1_scale2 <- resample_dat_scalev2(dat_nocycles, tnp = 3, cycles = 40)


