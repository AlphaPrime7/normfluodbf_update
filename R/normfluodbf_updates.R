

#NAME COLUMNS-FINAL MOMENTS
n <- c('A','B','D')
#c <- c(1,3,4,5,6)
c <- c(10,11,12)
c <- c(1,2,3)
c <- c(9,11,12,14)
c <- c(1,2,3,4,5,6,7,8,9,10,11,12)
c <- c(1,2,3,4,5,6)

sample_col_names <- dat_col_names_prime(trial1_scale, c('A','B','C'),cols_used = c, read_direction = 'horizontal')
sample_col_names <- dat_col_names_prime(trial1_scale, c('A','B','C'), read_direction = 'horizontal')
sample_col_names <- dat_col_names_prime(test9, c('A','B','C','D','E'),cols_used = c, read_direction = 'horizontal')
sample_col_names <- dat_col_names_prime(trial1_scale, c('A','B','C'),cols_used = c)
sample_col_names <- dat_col_names_prime(test9, c('A','B','C'),cols_used = c)
sample_col_names <- dat_col_names_prime(trial1_scale, c('A','B','C'))
sample_col_names <- dat_col_names_prime(trial1_scale)
stringr::str_sort(sample_col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
ord <- c("A1","A2","A3","A4")
f1 <- factor(sample_col_names,levels=ord)
sort(f1)
col_names <- sample_col_names
sample_col_names <- sort(sample_col_names)
sample_col_names <- order(sample_col_names)
sample_col_names

colnames(trial1_scale) <- sample_col_names
trial1_scale$logA1 <- log10(trial1_scale$A1)
trial1_scale$logB1 <- log10(trial1_scale$B1)
trial1_scale$logC1 <- log10(trial1_scale$C1)
test6 <- trial1_scale[,c(1:6)]
test9 <- trial1_scale[,c(1:9)]
sample_col_names_spec <- dat_col_names(test6, n, c(10,12))
sample_col_names_spec <- dat_col_names_prime(test6, n)
sample_col_names_spec <- dat_col_names_norm(test6,n,1,2,'no')
sample_col_names_spec
colnames(test6) <- sample_col_names_spec
colnames(test9) <- sample_col_names

colnames(test6) <- sample_col_names_spec












