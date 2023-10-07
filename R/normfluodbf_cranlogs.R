#Just some more fun tracking-trust me when you make a package, it is surreal to see users increase
#and the fun part is tracking the package
library(cranlogs)
library(ggplot2)

#Last week's downloads
cran_downloads(packages="normfluodbf", when="last-month") #last-week, last-month


#overall downloads
#How many overall downloads
#helps me remember the day the package was officially published
nfd_totals <- cran_downloads(packages="normfluodbf", from = "2023-08-25", to = Sys.Date()-1)
sum(nfd_totals[,2]) #=1014
nfd_totals

#Plot
nfd <- ggplot(nfd_totals, aes(nfd_totals$date, nfd_totals$count)) +
  geom_line(colour = "red",size=1)
nfd + xlab("Time") + ylab("Nr. of downloads") +
  labs(title = paste0("Normfluodbf daily downloads ", Sys.Date()-1))

#Cumulative
cumulative <- cumsum(nfd_totals[,2])
nfd_cumulative_append <- cbind(nfd_totals,cumulative)


#Plot
nfd2 <- ggplot(nfd_cumulative_append, aes(nfd_cumulative_append$date, nfd_cumulative_append$cumulative)) +
  geom_line(colour = "blue",size=1)
nfd2 + xlab("Time") + ylab("Nr. of downloads") +
  labs(title = paste0("Normfluodbf cumulative downloads until ", Sys.Date()-1))
