#SAVE IMAGE
save.image("C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/On_GitHub/normfluodbf_update/warmed_up.RData")
#96 WELL PLATE RUN- DF OR LIST

#PLATE META RAW
library(dplyr)
library(magrittr)
blank_plate_meta <- function(well_row = NULL, well_col = NULL) {

  if(is.null(well_row)){
    well_row = LETTERS[1:8]
  } else {
    well_row = well_row
  }

  if(is.null(well_col)){
    well_col = 1:12
  } else {
    well_col = well_col
  }

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}
blank_plate_meta()

#PLATE DATA RAW
plate_ready_data = function(dat, tnp, cycles, rows_used,...){

  phd = suppressMessages({normfluodbf::normfluodat(dat,
                                                   tnp,
                                                   cycles,
                                                   rows_used,
                                                   norm_scale = 'raw',
                                                   ...)})
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')
    pfd = pfd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                    "sample" = NA,
                    "used" = TRUE) %>%
      dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values")
    return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    pfd = pfd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor( gsub("[^0-9.-]", "", well)),
                    "sample" = NA,
                    "used" = TRUE) %>%
      dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number","fluor_values")
    #pfd = type.convert(pfd, as.is = TRUE)
    return(pfd)
  }

}
#run
demo = plate_ready_data(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'))
head(demo)
sapply(demo, class)

mod_meta = function(nfdmeta, nfddata, cols_to_remove = NULL,  bycol = "well"){

    jd <- dplyr::left_join(nfdmeta,
                           nfddata,
                           suffix = c("",""), #can also use original vs new suffix
                           by = bycol
    ) #multiple = "all" for full joins

    if(!is.null(cols_to_remove)){
      jd <- jd %>% select(!all_of(c(cols_to_remove)))
      jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
      jd %<>% dplyr::arrange(desc(used), well_row, well_col)
      jd

    } else {
      jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
      jd
    }


}
#run
nfdmeta = mod_meta(plate_meta(nfdplate), plate_data(nfdplate),
                                    bycol = c("well","well_row","well_col"),
                                    cols_to_remove = 'sample')

#EMPTY PLATE
empty_plate = function(){
  list(
    params = NULL,
    plate_meta = NULL, #simple data frame of plate meta above
    plate_data = NULL, #simple data frame of plate data from normfluodat (decide if to transpose or not)
    status = NULL, #marks the step of making and/or analyzing data
    steps = NULL,
    version = NULL
  )
}

#META & DATA
plate_meta = function(plate){
  plate[['plate_meta']]
}

`plate_meta<-` <- function(plate, value) {
  plate[['plate_meta']] <- value
  plate #need to return a plate at all times after adding the desired values
}

plate_data = function(plate){
  plate[['plate_data']]
}

`plate_data<-` <- function(plate, value) {
  plate[['plate_data']] <- value
  plate
}

#STATUS
define_status = function(plate){
  UseMethod('define_status')
}

define_status.list <- function(plate) {
  list(
    'INITIALIZE_EMPTY' = 0,
    'DATA_INITIALIZED' = 1,
    'FLAG_OUTLIERS' = 2, #remember those threshold values
    'PLATE_READY' = 3
  )
}

status = function(plate){
  plate[['status']]
}

`status<-` <- function(plate, value) {
  plate[['status']] <- value
  plate
}

set_default_status <- function(plate) {
  status(plate) <- define_status(plate)
  plate
}

#PARAMS-S3 Class
define_params = function(plate){
  UseMethod("define_params")
}

define_params.list <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  TECH_SPECS <- list()
  TECH_SPECS['well_width'] <- 2.00
  TECH_SPECS['well_height'] <- 1.00
  TECH_SPECS['well_capacity'] <- 0.75
  TECH_SPECS['other'] <- NULL

  THRESHOLDS <- list() #what makes normfluodbf special?noise-signal zones
  THRESHOLDS['MAX_FLUOR'] <- 2^16 #65536
  THRESHOLDS['MIN_FLUOR'] <- 2^7  #128-guess it can be lower
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'TECH_SPECS' = TECH_SPECS,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

define_params.list()[['GENERAL']][['X_VAR_ONE']]

set_default_params <- function(plate) {
  plate[['params']] <- define_params(plate)
  plate
}

params = function(plate,category,name){
    plate[['params']][[category]][[name]]
}

`params<-` = function(plate, category,name, value){
    plate['params'][[category]][[name]] <- value
    plate
}

#STEPS
define_steps = function(plate){
  UseMethod('define_steps')
}

define_steps.list <- function(plate) {
  list(
    'INITIALIZE_EMPTY' = 'init_empty_plate',
    'DATA_INITIALIZED' = 'init_data',
    'FLAG_NOISE' = 'flag_noise',
    'PLATE_READY' = 'plate_ready'
  )
}

steps <- function(plate) {
  plate[['steps']]
}

`steps<-` <- function(plate, value) {
  plate[['steps']] <- value
  plate
}

step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  res
}

step_name <- function(plate, step) {
  step %<>% as.integer
  plate %>% steps %>% names %>% .[step]
}

set_default_steps <- function(plate) {
  steps(plate) <- define_steps(plate)
  plate
}

# X & Y VARS
x_var_one <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_ONE')
}

x_var_two <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_TWO')
}

y_var <- function(plate) {
  params(plate, 'GENERAL', 'Y_VAR')
}

`x_var_one<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_ONE') <- value
  plate
}

`x_var_two<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_TWO') <- value
  plate
}

`y_var<-` <- function(plate) {
  params(plate, 'GENERAL', 'Y_VAR')
}

#INIT DATA
init_data = function(plate,dat,
                     tnp,
                     cycles,
                     rows_used,
                     ...,
                     type = NULL){

  library(normfluodbf)
  stopifnot(!is.null(rows_used))

  tryCatch({
    lipo_data = plate_ready_data(dat,tnp,cycles,rows_used)
  },
  error = function(err) {
    message("There was a problem retreiving data from normfluodat")
  }
  )

  plate_data(plate) = lipo_data
  plate

}
#run
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
init_data(empty_plate(),dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL)

#PLATE -INIT PLATE
init_plate = function(plate,dat,
                      tnp,
                      cycles,
                      rows_used,type = NULL){

  #no separate meta init function
  plate_meta(plate) = blank_plate_meta()

  plate = plate %>%
    set_default_steps %>%
    set_default_status %>%
    set_default_params %>%
    init_data(dat,tnp,cycles,rows_used)

  #setting plate/list stuff
  #set_default_steps(plate)

  steps(plate) = names(define_steps(plate)[2])
  status(plate) = define_status(plate)[['DATA_INITIALIZED']]
  plate[['version']] <- as.character(utils::packageVersion("normfluodbf"))

  plate

}
#run
init_plate(empty_plate(),dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL )

#NEW PLATE
new_plate = function(dat,
                     tnp,
                     cycles,
                     rows_used,
                     type = NULL,
                     begin = TRUE){
  plate = empty_plate()
  set_default_steps(plate)
  set_default_status(plate)
  set_default_params(plate)

  if(is.null(rows_used) || begin == FALSE || length(status(plate) > 1 )){
    plate = set_default_params(plate)
    steps(plate) = names(define_steps(plate)[1])
    status(plate) = define_status(plate)[['INITIALIZE_EMPTY']]
    plate[['version']] <- as.character(utils::packageVersion("normfluodbf"))
    plate[['dirty']] <- FALSE
    plate
  } else{
    #initialize a plate with its data
    #1. add plate meta data (3 types of plates by default-for now just 96 wells plates)
    plate[['dirty']] <- TRUE
    init_plate(plate, dat, tnp,cycles, rows_used, type = NULL)
  }

}
#run
new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = NULL, type = NULL)
new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL, begin = FALSE)
new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL, begin = TRUE)
nfdplate = new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL, begin = TRUE)

#NFD DATA EXTRACT
nfddata = plate_data(nfdplate)

#SINGLE WELL EXTRACT
get_single_well <- function(plate, well_id) {

  well_id %<>% toupper

  result <-
    plate_data(plate) %>%
    dplyr::filter(well == well_id) %>%
    dplyr::select(quote(well))

  result
}
#run
get_single_well(nfdplate,'A1')

#SUBSET UTILS
rangel = ("A1, B2:C4, C7")
rangel = ("A01, B02:C04, C07")

#RANGE
WELL_ID_REGEX <- "^[A-H]([0-9])?[0-9]$"
range_to_endpoints = function(well_range){
  wr <- gsub("[[:space:]]", "", well_range)
  wr <- strsplit(wr, ",") %>% unlist %>% .[. != ""]
  endpoints <- strsplit(wr, ":") %>% unlist
  endpoints <- c(endpoints[1], endpoints[length(endpoints)])

  if (!grepl(WELL_ID_REGEX, endpoints) %>% all) {
    stop("Invalid wells given to for endpoints")
  }
  if (endpoints %>% length != 2) {
    stop("Invalid range given to endpoints")
  }

  endpoints #this function is ok because it covers dbf and dat files in normfluodbf
}
#run
range_to_endpoints(rangel)

#WELLS BTWN
get_wells_btwn <- function(well1, well2) {

  well1 = toupper(well1)
  well2 = toupper(well2)

  rows <-
    substring(c(well1,well2), 1, 1) %>%
    magrittr::is_in(LETTERS, .) %>% which
  rows_seq = seq(min(rows), max(rows)) %>%
    LETTERS[.]
  rows = rows_seq

  cols <-
    substring(c(well1, well2), 2, 3) %>%
    as.integer
  cols_seq = seq(min(cols), max(cols))
  cols = sprintf("%1d", cols_seq)
  cols = as.integer(cols)

  wells <- lapply(rows, function(x) paste(x, cols, sep = "")) %>% unlist
  wells
}
#run
get_wells_btwn('A1','C3')

get_wells_btwn <- function(well1, well2) {

  well1 = toupper(well1)
  well2 = toupper(well2)

  dpm <-
    expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
    magrittr::set_colnames(c("row", "col")) %>%
    dplyr::mutate(
      well = sprintf("%s%1d", row, col)
    ) %>%
    dplyr::select(c("well", "row", "col"))

  w1 = which(dpm$well %in% c(well1))
  w2 = which(dpm$well %in% c(well2))

  #mi = min(w1,w2)
  #ma = max(w1,w2)


  #dpm = dpm[c(mi:ma),]
  dpm = dpm[c(w1:w2),]
  w_btwn = dpm$well
  w_btwn = as.vector(w_btwn)
  w_btwn = as.list(w_btwn)

  return(w_btwn)

}
#run
get_wells_btwn('A1','C3')

#RANGE TO LIST
range_list_to_vec <- function(rangel) {
  rangel <- gsub("[[:space:]]", "", rangel)
  ranges <- strsplit(rangel, ",") %>% unlist %>% .[. != ""]
  if (length(ranges) == 0) {
    return(NULL)
  }

  wells <-
    lapply(ranges, function(range) {
      endpoints <- range_to_endpoints(range)
      get_wells_btwn(endpoints[1], endpoints[length(endpoints)])
    }) %>%
    unlist %>%
    unique %>%
    sort
  wells
}
#run
range_list_to_vec(rangel)

is_range <- function(x) {
  length(x) == 1 && grepl("[,:]", x)
}

subset = function(plate,wells,...){
  UseMethod('subset')
}

subset.data.frame = function(plate,wells,...){

  #nfd already has the wells covered so only sub-setting based on user input
  #is most beneficial

  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_meta = plate %>%
    dplyr::filter(well %in% wells)

  #update meta if needed-LATER for now just a data frame
  wells_subset_meta
}
#run
rangel = ("A1:C1")
subset(nfdmeta, rangel)

#PLOTS
nfdmeta[['well_row']] %<>% as.factor
nfdmeta[['well_col']] %<>% as.factor
col_drops = "black"
col_drops_undefined = col_drops
col_drops_failed = col_drops
col_drops_empty = col_drops
col_drops_outlier = "orange"
bg_plot = "transparent"
bg_outlier = "#111111"
bg_unused = "#FFFFFF"
bg_used = "#DDDDDD"
alpha_bg_failed = 0.7

p <-
  ggplot2::ggplot() +
  ggplot2::xlab("CYCLE_NUMBER") +
  ggplot2::ylab("FLUOR") +
  ggplot2::ggtitle("FINALLY") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(angle = 90, vjust = 0.5),
    title            = ggplot2::element_text(size = 14),
    axis.title       = ggplot2::element_text(size = 12),
    axis.text        = ggplot2::element_text(size = 12),
    strip.text       = ggplot2::element_text(size = 12),
    plot.background  = ggplot2::element_rect(fill = bg_plot, color = bg_plot),
    aspect.ratio     = NULL
  )
p

s <-
  ggplot2::ggplot() +
  ggplot2::xlab("CYCLE_NUMBER") +
  ggplot2::ylab("FLUOR") +
  ggplot2::ggtitle("FINALLY") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(angle = 90, vjust = 0.5),
    title            = ggplot2::element_text(size = 14),
    axis.title       = ggplot2::element_text(size = 12),
    axis.text        = ggplot2::element_text(size = 12),
    strip.text       = ggplot2::element_text(size = 12),
    plot.background  = ggplot2::element_rect(fill = bg_plot, color = bg_plot),
    aspect.ratio     = NULL
  )
s

#create the grid with all the wells

rowlevels <-
  dplyr::pull(nfdmeta, well_row) %>%
  as.factor() %>%
  levels()
rowlevels
rev(rowlevels)

collevels <-
  dplyr::pull(nfdmeta, well_col) %>%
  as.numeric() %>%
  as.factor() %>%
  levels() %>% as.numeric()
collevels
class(collevels)

#IMPORTANT
reverse_row <- function(vect) {
  vect = levels(as.factor(vect))
  vect = rev(vect)
  vect
}
reverse(nfdmeta$well_row)

tmeta = nfdmeta %>% dplyr::distinct(well,well_row,well_col,used, .keep_all=FALSE)
tmeta[order(tmeta$well_row), ]
tmeta = tmeta[order(tmeta$well_row), ]
tmeta[['well_row']] %<>% as.factor
tmeta[['well_col']] %<>% as.factor

umeta = dplyr::filter(tmeta, !used)

#SUPERIMPOSE NO-PLOT all samples in GRIDS
#facet grid for superimposition-data with $ needed here since generic plot is missing the dataset
p <- p +
  ggplot2::facet_grid(as.factor(tmeta$well_row) ~ tmeta$well_col)
p

p <- p +
  ggplot2::facet_grid(vars(umeta$well_row) ~ vars(umeta$well_col),
                      space = "free_y",
                      switch = "y",
                      labeller = labeller(),
                      drop = FALSE)
p

p <- p +
  ggplot2::facet_grid(reverse(well_row) ~ well_col,
                      space = "free_y",
                      switch = "both",
                      #labeller = labeller(reverse(nfdmeta$well_row)),
                      drop = FALSE) +
  theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="red", fill="orange"))
p

#before
p <- p +
  ggplot2::facet_grid(nfdmeta$well_row ~ nfdmeta$well_col,
                      space = "free_y",
                      #switch = "both",
                      #labeller = labeller(well_row = reverse),
                      drop = FALSE) +
  theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="red", fill="orange"))
p

p <- p +
  ggplot2::facet_grid(submeta$well_row ~ submeta$well_col,
                      space = "free_y",
                      #switch = "both",
                      #labeller = labeller(well_row = reverse),
                      drop = FALSE) +
  theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="red", fill="orange"))
p

p <- p +
  ggplot2::facet_grid(factor(nfdmeta$well_row, levels = reverse(nfdmeta$well_row)) ~ nfdmeta$well_col,
                      space = "free_y",
                      #switch = "both",
                      #labeller = labeller(well_row = reverse),
                      drop = FALSE) +
  theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="red", fill="orange"))
p

p <- p +
  ggplot2::facet_grid(forcats::fct_rev(nfdmeta$well_row) ~ nfdmeta$well_col,
                      space = "free_y",
                      switch = "y",
                      #labeller = labeller(well_row = reverse),
                      drop = FALSE) +
  theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="red", fill="orange"))
p

##plotting well values-PLOT WELL VALUES
sample_cols = nfdmeta %>% .[['well']] %>% as.vector() %>% unique()
alpha_cols = c(1:length(sample_cols) / 100)
alpha_cols

p <- p +
  ggplot2::geom_point(
    data = nfdmeta,
    ggplot2::aes_string(x = "Cycle_Number", y = "fluor_values", label = 'well'),
    size = 0.00000001,
    shape = 21, #empty oval
    show.legend = FALSE, fill = bg_used) +
  ggplot2::scale_color_manual(values = sample_cols) +
  ggplot2::scale_alpha_manual(values = alpha_cols)
p

#OR
p <- p +
  ggplot2::geom_point(
    data = nfdmeta,
    ggplot2::aes_string(x = "Cycle_Number", y = "fluor_values", label = 'well'),
    size = 0.00000001,
    shape = 21, #empty oval
    show.legend = FALSE, fill = bg_used) +
  #facet_wrap(~forcats::fct_rev(nfdmeta$well_row)) + #fails
  ggplot2::scale_color_manual(values = sample_cols) +
  ggplot2::scale_alpha_manual(values = alpha_cols)
p
ggsave("fluostar_format.png", width=9,height=6)

#OR
p <- p +
  ggplot2::geom_point(
    data = nfdmeta,
    ggplot2::aes_string("Cycle_Number", "fluor_values"),
    alpha = 0.3,
    size = 1
  )

p

p <- p +
  ggplot2::geom_line(
    data = nfdmeta,
    ggplot2::aes_string(x = nfdmeta$Cycle_Number, y = nfdmeta$fluor_values),
    size = 0.001,
    show.legend = FALSE)
p

p <- p +
  ggplot2::geom_line(
    data = submeta,
    ggplot2::aes_string(x = submeta$Cycle_Number, y = submeta$fluor_values),
    size = 0.001,
    show.legend = FALSE)
p

#reset with same data
p <- p +
  ggplot2::geom_rect(
    data = nfdmeta,
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
    fill = bg_unused)
p

ggsave("fluostar_format.png", width=9,height=6)

#VISUAL CONTROL
p <- p +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank())
p

p <- p +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())
p

rangel = ("A1:B5")
submeta = subset(nfdmeta, rangel)

p2 = s + ggplot2::geom_point(
  data = submeta,
  ggplot2::aes_string(x = submeta$Cycle_Number, y = submeta$fluor_values),
  size = 1,
  show.legend = FALSE)
p2
library(gridExtra)
pcat =gridExtra::arrangeGrob(p, p2, heights=c(0.4, 0.6), widths=c(0.6, 0.4))
plot(pcat) #method1
grid.arrange(p, p2, ncol=2) #method2

#UNK
p <- p +
  ggplot2::geom_text(data = nfdmeta, ggplot2::aes(0, 0, label = ""))
p

#set attributes
rows <- nfdmeta[['well_row']] %>% unique %>% length
cols <- nfdmeta[['well_col']] %>% unique %>% length

attr(p, 'nfd_rows') <- rows
attr(p, 'nfd_cols') <- cols

p

#GGplot adv
ggb <- ggplot2::ggplot_build(p)
ggb
ggb$layout$panel_scales_x[[1]]$range$range[1] + diff(ggb$layout$panel_scales_x[[1]]$range$range) * 0.95

#Plot to select wells

rowlevels <-
  dplyr::pull(nfdmeta, well_row) %>%
  as.factor() %>%
  levels()
rowlevels
rev(rowlevels)

#PLATE SELECTOR
nfdmeta[['well_row']] <- as.factor(nfdmeta[['well_row']])
nfdmeta[['well_row']] <- factor(nfdmeta[['well_row']], levels = rev(levels(nfdmeta[['well_row']])))
p <-
  ggplot2::ggplot(nfdmeta, ggplot2::aes(as.factor(well_col),as.factor(well_row) )) +
  ggplot2::geom_tile(ggplot2::aes(fill = used), color = "#222222", show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "#333333", "FALSE" = "white")) +
  ggplot2::theme(
    panel.grid       = ggplot2::element_blank(),
    line             = ggplot2::element_blank(),
    axis.text        = ggplot2::element_text(size = 20, color = "black"),
    panel.background = ggplot2::element_blank()
  ) +
  ggplot2::xlab(NULL) +
  ggplot2::ylab(NULL) +
  ggplot2::coord_fixed()
p
#OR
p <-
  ggplot2::ggplot(nfdmeta, ggplot2::aes(as.factor(well_col),as.factor(well_row) )) +
  ggplot2::geom_tile(ggplot2::aes(fill = used), color = "#222222", show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "#333333", "FALSE" = "white")) +
  ggplot2::scale_y_discrete(expand = c(0, 0),
                            limits = rev(rowlevels) ) +
  ggplot2::theme(
    panel.grid       = ggplot2::element_blank(),
    line             = ggplot2::element_blank(),
    axis.text        = ggplot2::element_text(size = 20, color = "black"),
    panel.background = ggplot2::element_blank()
  ) +
  ggplot2::xlab(NULL) +
  ggplot2::ylab(NULL) +
  ggplot2::coord_fixed()
p

#cleaner look
p <- p +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())
p

#RANGE PATTERNS
WELL_ID_REGEX <- "^[A-H]([0-9])?[0-9]$"
pattern <- "[A-H]([0-9])?[0-9]:[A-H]([0-9])?[0-9]"
strings <- ranges
print (grep(pattern, strings))

("C05,F05")
gsub('(\\w+).*:(\\w+).*', '\\1',"A2:B1")
gsub('(\\w+).*:(\\w+).*', '\\2',"A2:B1")

gsub('(\\w+).*,(\\w+).*','\\1',"C05,F05")
gsub('(\\w+).*,(\\w+).*', '\\2',"C05,F05")



