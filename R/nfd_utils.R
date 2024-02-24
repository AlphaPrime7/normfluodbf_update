tpath = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/sample_data"
tpath = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update"
fpath = "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/sample_data/test.csv"
save.image("C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/nfd_up.RData")
load("C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/nfd_up.RData")

library(pkgsearch)
pkg_search("normfluodbf")
data(package = "normfluodbf")

library(foreign)
lookfun = read.dbf("liposomes_218.dbf")
norm_dat <- normfluodat(sample_data_file("dat_2"), tnp = 3, cycles = 40)
data.table::transpose(norm_dat, keep.names = 'rn')

#normfluodbf new update -OOP
empty_plate <- function() {
  list(
    plate_meta = NULL, #vary by plate(contains plate technical specs different fluostar physics thresholds)
    #zero_plate in here as well
    #parent type specific (init not null-define thresholds)
    #properties = NULL, #physical dimensions-know where to find this
    plate_data = NULL, #plate data
    well_status = NULL, #dirty or clean (used vs not used)(init null)
    steps = NULL, #init null plot steps happen eventually
    #step1=normfluodat
    #step2=take each well data x and y variable-try to keep them like that more overhead
    #step3=plot the well data
    status = NULL,
    dirty = NULL,
    version = NULL)
}

parent_plate_type <- function(plate) {
  UseMethod("parent_plate_type")
}

plate_types <- list()
plate_types[['normfluodbf_plate']] <- "normfluodbf_plate"
plate_types[['test_plate']] <- "test_plate"

parent_plate_type.normfluodbf_plate <- function(plate) {
  NULL
}
parent_plate_type.default <- function(plate) {
  if(class(plate)[1] != "list"){
    class(plate) <- class(plate)
    plate
  } else {
    class(plate) = c("normfluodbf_plate", class(plate))
  }

}


#SET TYPE-THE SUBORDINATE TO SETUP_PLATE (WORKS WITH SET DEFAULT PARAMS LIKELY NOT USEFUL IN NORMFLUODBF)

set_plate_type <- function(plate, type) {
  #the plate type should be not be specified for the plate setup
  #otherwise simply return the specified plate type
  if (!missing(type) && !is.null(type) && class(plate)[1] != "list") {
    return(plate)
  }

  if (missing(type) || is.null(type) ) {
    type <- NULL
  }

  # Add the given type to the classlist (initially the class will be "list"
  # because that's the mode of a plate - we want to exclude that one)
  new_class <- type
  if (class(plate)[1] != "list") {
    new_class <- c(class(plate), new_class)
  }
  #structure(plate, class=new_class)
  class(plate) <- new_class
  plate

  set_plate_type(plate,
                 structure(plate, class = type) %>% parent_plate_type)
}
library(dplyr)
set_plate_type(empty_plate(), type = NULL)


#SETUP PLATE-SUB TO NEW_PLATE
setup_plate <- function(plate, type) {
  plate <- set_plate_type(plate, type)
  plate
}
#NEW PLATE

status <- function(plate) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['status']]
}

is_plate_empty <- function(plate) {
  is.null(status(plate))
}

plate_types$normfluodbf_plate
#seems a little more intuitive now
fpath <- system.file("extdata", "dat_2.dat", package = "normfluodbf", mustWork = TRUE)
create_plate <- function(well_row = LETTERS[1:8], well_col = 1:12) {
  library(tibble)

  plate_meta = expand.grid(well_row, as.factor(well_col), stringsAsFactors = TRUE)
  plate_meta = as_tibble(plate_meta)

  plate_meta = plate_meta %>%
    magrittr::set_colnames(c("well_row", "well_col")) %>%
    dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                  "selected_wells" = NA, "used_wells" = FALSE) %>%
    dplyr::select("well", "well_row", "well_col", "used_wells") %>%
    as_tibble()

  return(plate_meta)
}
#this is the thing, to use plate functions effectively, I trigger must supply rows_used
#really need to be in Dean's neural network

step_begin <- function(text) {
  .globals$set("step_tstart", proc.time())
  msg(text, "... ", appendLF = FALSE)
}

type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}

steps <- function(plate) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['steps']]
}

step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  if (res %>% length != 1) {
    normfluodbf_msg_msg(sprintf("could not find step `%s`", step))
  }
  res
}

plate = empty_plate()
plate = set_plate_type(plate)
step_begin(sprintf("Initializing plate of type `%s`", type(plate)))

status(plate) <- step(plate, 'INITIALIZE')
plate[['version']] <- as.character(utils::packageVersion("normfluodbf"))
plate[['dirty']] <- FALSE
step_end()

plate

init_plate <- function(plate) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  step_begin(sprintf("Initializing plate of type `%s`", type(plate)))

  plate %<>%
    set_default_clusters %>%
    set_default_steps %>%
    init_data %>%
    init_meta

  status(plate) <- step(plate, 'INITIALIZE')
  plate[['version']] <- as.character(utils::packageVersion("normfluodbf"))
  plate[['dirty']] <- FALSE
  step_end()

  plate
}

plate_data <- function(plate) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['plate_data']]
}

`plate_data<-` <- function(plate, value) {
  plate[['plate_data']] <- value
  plate
}

new_plate <- function(dat,
                      tnp,
                      cycles,
                      rows_used, type=NULL) {

  stopifnot(!is.null(rows_used))

  library(normfluodbf)
  plate = empty_plate()
  plate <- setup_plate(plate,type=type)
  status(plate)

  if(is_plate_empty(plate)){
    lip_data = normfluodbf::normfluodat(dat,tnp,cycles,rows_used)
    #plate[["zero_plate"]] <- create_plate(well_row = LETTERS[1:8], well_col = 1:12)
    #plate[["plate_data"]] <- lip_data
    plate_data(plate) = lip_data
    status(plate) = "in_use"
    return(plate)
  } else {
    return(plate)
  }

}
new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL)
save.image("C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/nfd_up.RData")
load("C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/nfd_up.RData")

#MORE NEXT-ONE STEP AT A TIME

params = function(plate, category, name){
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

define_params <- function(plate) {
  UseMethod("define_params")
}


set_default_params <- function(plate) {
  if (!is_empty_plate(plate)) {
    new_params <- define_params(plate)
    x_var(plate) <- new_params[['GENERAL']]['X_VAR']
    y_var(plate) <- new_params[['GENERAL']]['Y_VAR']
  }

  params(plate) <- define_params(plate)
  plate
}

setup_plate <- function(plate, type) {
  plate <- set_plate_type(plate, type)
  plate <- set_default_params(plate)
  plate
}

type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}

define_params.ddpcr_plate <- function(plate) {
  # Each parameter has a somewhat descriptive name of what it is used for, and
  # all parameters used by a single step in the pipeline are in a list together
  PARAMS_GENERAL <- list()
  PARAMS_GENERAL['X_VAR'] <- "HEX"
  PARAMS_GENERAL['Y_VAR'] <- "FAM"
  PARAMS_GENERAL['DROPLET_VOLUME'] <- 0.85e-3
  PARAMS_GENERAL['RANDOM_SEED'] <- 8
  PARAMS_REMOVE_OUTLIERS <- list()
  PARAMS_REMOVE_OUTLIERS['TOP_PERCENT'] <- 1
  PARAMS_REMOVE_OUTLIERS['CUTOFF_IQR'] <- 5
  PARAMS_REMOVE_FAILURES <- list()
  PARAMS_REMOVE_FAILURES['TOTAL_DROPS_T'] <- 5000
  PARAMS_REMOVE_FAILURES['EMPTY_LAMBDA_LOW_T'] <- 0.3
  PARAMS_REMOVE_FAILURES['EMPTY_LAMBDA_HIGH_T'] <- 0.99
  PARAMS_REMOVE_EMPTY <- list()
  PARAMS_REMOVE_EMPTY['CUTOFF_SD'] <- 7
  DEFAULT_PARAMS <- list(
    'GENERAL'           = PARAMS_GENERAL,
    'REMOVE_FAILURES'   = PARAMS_REMOVE_FAILURES,
    'REMOVE_OUTLIERS'   = PARAMS_REMOVE_OUTLIERS,
    'REMOVE_EMPTY'      = PARAMS_REMOVE_EMPTY
  )
  DEFAULT_PARAMS
}


