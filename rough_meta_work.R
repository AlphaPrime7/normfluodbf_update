#META DATA MANIPULATION FOR DDPCR
ddmeta = plate_meta(ddplate)
dddata
#used if plate meta is null
DEFAULT_PLATE_META <-
  expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
  magrittr::set_colnames(c("well_row", "well_col")) %>%
  dplyr::mutate(
    well = sprintf("%s%1d", well_row, well_col),
    sample = NA,
    target_ch1 = NA,
    target_ch2 = NA,
    used = FALSE
  ) %>%
  dplyr::select(c("well", "sample", "well_row", "well_col", "used"))
meta_cols_keep <- c("well", "sample", "target_ch1", "target_ch2")

#if not null %<>% used to reassign values back to the original variable
ddmeta %<>% dplyr::mutate("target" = assay,
                               "channel" = substr(typeassay, 1,3))

ddmeta %<>%
  dplyr::group_by("well", "sample") %>%
  dplyr::summarise(
    "target_ch1" = unique(target[channel == 'Ch1']),
    "target_ch2" = unique(target[channel == 'Ch2'])
  ) %>%
  dplyr::ungroup()
colnames(ddmeta) = c('well', 'sample','target_ch1','target_ch2')

ddmeta %<>%
  dplyr::select(dplyr::one_of(meta_cols_keep))

wells_used = result[['well']] %>% unique
result <- dplyr::left_join(DEFAULT_PLATE_META, dddata, by = "well")
result[['used']] <- result[['well']] %in% wells_used
result[['sample']][!result[['used']]] <- NA

result1 <-
  dddata %>%
  dplyr::group_by("well") %>%
  dplyr::summarise("drops" = dplyr::n())
colnames(result1) = c('well','drops')

result2 = dplyr::left_join(result, result1, by = "well")
result2 = result2 %>% dplyr::arrange(desc(used), row, col)
colnames(result2)[6] = c("HEX")
colnames(result2)[7] = c("FAM")


get_single_well <- function(result, well_id) {

 result =  result %>%
    dplyr::filter(well == well_id) %>%
    dplyr::select(quote(well),HEX,FAM)

  result
}

quiet <- function(expr, all = TRUE) {
  if (Sys.info()['sysname'] == "Windows") {
    file <- "NUL"
  } else {
    file <- "/dev/null"
  }

  if (all) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      utils::capture.output(expr, file = file)
    )))
  } else {
    utils::capture.output(expr, file = file)
  }
}


#Mixtools Intro-Using it on default data
library(mixtools)
data("Waterdata")
cutpts <- 10.5*(-6:6)
watermult <- makemultdata(Waterdata, cuts = cutpts)
set.seed(15)
theta4 <- matrix(stats::runif(56), ncol = 14)
theta3 <- theta4[1:3,]
mult3 <- multmixEM(watermult, lambda = rep(1, 3)/3, theta = theta3)
mult4 <- multmixEM (watermult, lambda = rep (1, 4) / 4, theta = theta4)

#Mixtools Intro-Using it on data from Dean
#other bs
result3 = get_single_well(result2,"A01")
set.seed(8)
mixmdl_y <- mixtools::normalmixEM(result3[["HEX"]], k = 2)
mixmdl_y
mixmdl_y$mu
mixmdl_y$sigma
smaller_comp_y <- mixmdl_y$mu %>% which.min
smaller_comp_y

cutoff_y <-
  ( mixmdl_y$mu[smaller_comp_y] +
      2 * mixmdl_y$sigma[smaller_comp_y]
  ) %>%
  ceiling %>%
  as.integer
cuttoffs = list("x" = cutoff_y, "y" = cutoff_y)
cuttoffs
as.data.frame(cuttoffs)

wells_success <- function(meta) {

    meta %>%
    dplyr::filter(as.logical(success)) %>%
    .[['well']]
}
result3$success = "TRUE"
wells_success(result3)

lol_to_df <- function(lol, name = "well") {
  lol %<>% t() %>% as.data.frame(stringsAsFactors = TRUE)
  lol[[name]] <- row.names(lol)
  lol <- move_front(lol, name)
  lol[] <- lapply(lol, unlist)
  row.names(lol) <- NULL
  lol
}

well_id = result3[['well']]
wasap = result3 %>% dplyr::group_by(well) %>% ungroup()

plate <- subset(result3, success = TRUE, select = c('well',"HEX","FAM") )
rm(x)
all_params <- c(as.list(environment()), list(...))
colnames(meta)[6] = c("HEX"); colnames(meta)[7] = c("FAM")
if (plate %>% wells_used %>% length == 0)
meta_used <- meta %>% dplyr::filter(used)
meta_used %<>%
    dplyr::filter(col %in% (meta_used[['col']] %>% unique),
                   row %in% (meta_used[['row']] %>% unique)) %>%
    dplyr::arrange(row, col)

#MOMENT OF TRUTH


###NOT APPLICABLE FOR NFD FOR NOW
p <- p +
  ggplot2::geom_rect(
    data = dplyr::filter(nfdmeta, !used),
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = bg_failed)
p















