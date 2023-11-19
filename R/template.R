#name the columns
ru = rows_used
cu = cols_used
usl = user_specific_labels
rd = read_direction
sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
colnames(df) <- sample_col_names

#remove NA cols to ensure the best approximation of sample names
df <- df[ , colSums(is.na(df))==0]

#apply procedure
df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

if(is.null(na_omit) || na_omit == 'yes') {


} else if(!is.null(na_omit) || na_omit == 'no'){

}
