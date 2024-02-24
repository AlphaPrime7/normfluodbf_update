#PLATE FUNCTIONS

##an empty plate for liposome flux assays. define this? what happens in the plate full picture?
## contains plate_data=cleaned data so everything done by normfluodat is what goes into the plate
## plate has default parameters defined by the machine(threshold, ncol, nrows, capacity(w,h,wspacing,wdiam) etc)-the latter more about wells
## better initialized as a list for containing that cleaned data from normfluodat

#1-wells used-easiest function to write as it simply blends with normfluodbf
get_wells_used = function(norm_dat){
  library(magrittr)

  pat = "^\\w{1-2}\\d+"

  wells_used = names(norm_dat)

  wells_used <- grep(pat,wells_used , value = TRUE)

  wells_used = tibble::as_tibble(wells_used) %>%
    magrittr::set_colnames(c('wells_used')) %>%
    dplyr::mutate(
      used = TRUE)

  wells_used

}
#run-works
library(normfluodbf)
help(normfluodat)
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
norm_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'))
norm_datt = data.table::transpose(norm_dat, keep.names = 'rn')
get_wells_used(norm_dat)

#2-imaginary empty plate with all its possible properties-some are properties in property
#-step ggplot in the well
#-the only differentiating factor in my plates is the size (a few parameter changes)
#- in my case when i say plate types i mean plate sizes for the most part(diam,vol etc diffs)
#- in my case the parent plate type will be all things about a plate apart from the size params
# and plate meta (changes by plate type so will be NULL)
#-the plate type is irrespective of the plate but will define one parameter in my plate which is
# the plate_tech_meta
empty_plate <- function() {
  list(
    plate_data = NULL, #init null (data will be placed in wells eventually)
    plate_tech_meta = NULL, #vary by plate(contains plate technical specs different fluostar physics thresholds)
    plate_sci_meta = NULL, #parent type specific (init not null-define thresholds)
    well_status = NULL, #dirty or clean (used vs not used)(init null)
    steps      = NULL #init null plot steps happen eventually
  )
}
#run
blank_plate = empty_plate() #plate_tech_meta is specific to a plate type

#The list created in the empty plate function can then be populated with data
#the list is not init as plate_types is (more on this later)

params <- function(plate, category, name) {
  stopifnot(plate %>% inherits("ddpcr_plate"))

  res <- plate[['params']]
  if (!missing(category)) {
    res <- res[[category]]
    if (!missing(name)) {
      res <- res[[name]]
    }
  }

  res
}

#play with a 123 vector and changing its class
#essence is to ensure the numvec comes out with a type that is specific
#to my desires and in normfluodbf will be my pkg
#why is this important? good logic when trying to ensure that an object
#meets certain criteria generally before any more work can be done with or
#on said object
x = c(1,2,3)
set_vec_type <- function(numvec, type=NULL) {

  if(is.null(type) && class(numvec)[1] != "numeric"){
    return(numvec)
  }

  new_class <- type
  if (class(numvec)[1] != "numeric") {
    new_class <- c(class(numvec), new_class)
    class(numvec) <- new_class
    return(numvec)
  } else {
    new_class = c('new_type','isaidso')
    class(numvec) = new_class
    return(numvec)
  }

}
#run-works
set_vec_type(x,type=NULL)

#IMPORTANT
#' @inheritParams define_params
define_params.plate_384_wells <- function(plate) {
  params <- NextMethod("define_params")

  new_params <- list(
    'GENERAL' = list(
      'COLS' = 2, #place holders
      'ROWS' = 3
    ),

    TECH_SPECS <- list(),
    TECH_SPECS['well_width'] <- 2.00,
    TECH_SPECS['well_height'] <- 1.00,
    TECH_SPECS['well_capacity'] <- 0.75,
    TECH_SPECS['other'] <- NULL
  )
  params %<>% utils::modifyList(new_params)

  params
}

plot.plate_384_wells <- function(
    x,
    wells, samples, ...)
{
  dots <- list(...)

  # call the plot function for general mutant/wildtype ddpcr plates
  # but use more user-friendly param names
  NextMethod("plot", x)
}



