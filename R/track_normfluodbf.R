package_trackeR <- function(packages){
  library(ggplot2)
  library(dlstats)
  library(tibble)
  #Create the downloads for the package
  dl <- dlstats::cran_stats(c(packages))
  #Create the plot
  plot <- ggplot(dl,
                 aes(end, downloads, group=package)) + 
    geom_line(aes(color=package),linetype="dashed") +
    geom_point(aes(shape=package, color=package)) + 
    theme_minimal()
  plot <- plot + xlab("Download date") + 
    ylab("Number of downloads")
  #Create a list for multiple returns
  returns_list <- list("download_df"=as_tibble(dl),
                       "downloads_to_date"=sum(dl$downloads),
                       "downloads_plot"=plot)
  
  return(returns_list)
}

#Call the new function
package_trackeR(c("normfluodbf"))
