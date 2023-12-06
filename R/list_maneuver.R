#create empty list of length 8
empty_list <- vector(mode='list', length=8)

#get length of list
len <- length(empty_list)

#define values to append to list
new <- c(3, 5, 12, 14, 17, 18, 18, 20)

#fill values in list
i = 1
while(i <= length(new)) {
  empty_list[[i]] <- new[i]
  i <- i + 1
}

#display updated list
empty_list
