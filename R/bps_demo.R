#BPS NUTS

#PLATE META
Virtual_plate_maker = function(well_count = c(96L, 384L, 1536L), well_type_1536 = NULL){

  if(96L %in% well_count){
    PLATE_META <-
      #another way to make data frames in R
      #default is A-H rows vs 1:12 cols
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", as.factor(well_row), as.factor(well_col)),
                    "sample_type" = NA, "selected_wells" = NA,
                    "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else if (384L %in% well_count){
    #A-P rows vs 1:24 cols
    PLATE_META <-
      expand.grid(LETTERS[1:16], 1:24, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                    "sample_type" = NA, "selected_wells" = NA,
                    "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else if (1536L %in% well_count){
    if (is.null(well_type_1536) || well_type_1536 == 1){
      #for the Corning? and Roche Lightcycler (tm) 1536-well plates
      row_names = paste0(rep(LETTERS[1:8], each = 4), letters[1:4])
    } else {
      #for the Labcyte Echo 1536-well plates
      row_names = c(LETTERS[1:26], paste0("A", LETTERS[1:6]))
    }

    PLATE_META <-
      #a little weird but A(a)-H(d ) (Aa,Ab,Ac,Ad...)

      expand.grid(row_names, 1:48, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                    "sample_type" = NA, "selected_wells" = NA,
                    "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else {
    PLATE_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                    "sample_type" = NA, "selected_wells" = NA, #i assume selected is used when working with a gui version of the well display.more on this.
                    "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  }

  PLATE_META

}
#run
Virtual_plate_maker(well_count = 96L, well_type_1536 = NULL)

#BLANK PLATE
create_blank_plate <- function(well_row = NULL, well_col = NULL) {

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
                 sep = "", remove = FALSE)
}

#SAMPLE TYPE KEY-CONCEPT ONLY
sample_type_key <- function(..., plate_type = c(96L,384L,1536L)) {
  sample_type_code = as.factor(c(1,2,3)) %>% as_tibble()
  if(96L %in% plate_type){
    sample_type_code = dplyr::bind_rows(sample_type_code, sample_type_code, sample_type_code)
  } else if (384L %in% plate_type){
    sample_type_code = dplyr::bind_rows(sample_type_code, sample_type_code, sample_type_code,
                                        sample_type_code, sample_type_code, sample_type_code)
  } else if (1536L %in% plate_type){
    sample_type_code = dplyr::bind_rows(sample_type_code, sample_type_code, sample_type_code,
                                        sample_type_code, sample_type_code, sample_type_code,
                                        sample_type_code, sample_type_code, sample_type_code,
                                        sample_type_code, sample_type_code, sample_type_code)
  }

  #96
  if(96L %in% plate_type){
    sample_type_code = list(sample_type_code)
    rowkey <- tibble(well_row = factor(LETTERS[1:9]))
    xlim = nrow(rowkey) - 1
    #384
  } else if (384L %in% plate_type){
    sample_type_code = list(sample_type_code)
    rowkey <- tibble(well_row = factor(LETTERS[1:18]))
    xlim = nrow(rowkey) - 2
    #1536
  } else if (1536L %in% plate_type){
    rowkey <- tibble(well_row = factor(paste0(rep(LETTERS[1:9], each = 4), letters[1:4]) ))
    xlim = nrow(rowkey) - 4
  }

  if (!missing(...)) {
    tnp <- list(...) %>% as_tibble()
    assertthat::assert_that(nrow(tnp) == 3,
                            msg = "Sample types should be test,negative,positive")
    if(96L %in% plate_type ){
      tnp_full_plate <- dplyr::bind_rows(tnp, tnp, tnp)
    } else if (384L %in% plate_type){
      tnp_full_plate <- dplyr::bind_rows(tnp, tnp, tnp, tnp, tnp, tnp)
    } else if (1536L %in% plate_type){
      tnp_full_plate <- dplyr::bind_rows(tnp, tnp, tnp, tnp, tnp, tnp, tnp, tnp, tnp, tnp, tnp, tnp)
    }

    rowkey <- dplyr::bind_cols(rowkey, sample_type_code, tnp_full_plate)
    colnames(rowkey) = c('well_row', 'sample_type_code','sample_type')
  }
  return(rowkey[c(1:xlim),])
}
#run
sample_type_key(sample_type = c('test','negative','positive'), plate_type = 96L)

#LABEL PLATE DF
label_plate_rowcol <- function(plate, rowkey = NULL, coercefactors = TRUE) {
  assertthat::assert_that(
    assertthat::has_name(plate,
                         c("well_row","well_col")))

  if (!is.factor(plate$well_col) & coercefactors){
    warning("plate$well_col is not a factor. Automatically generating plate$well_col factor levels. May lead to incorrect plate plans.")
    plate <- plate %>%
      dplyr::mutate(well_col = as.factor(well_col))
  }

  if (!is.factor(plate$well_row) & coercefactors){
    warning("plate$well_row is not a factor. Automatically generating plate$well_row factor levels. May lead to incorrect plate plans.")
    plate <- plate %>%
      dplyr::mutate(well_row = as.factor(well_row))
  }

  if (!is.null(rowkey)) {
    assertthat::assert_that(assertthat::has_name(rowkey, "well_row"))
    if (!is.factor(rowkey$well_row) & coercefactors) {
      message("coercing well_row to a factor with levels from plate$well_row")
      rowkey <- dplyr::mutate(
        rowkey,
        well_row = factor(well_row,
                          levels = levels(plate$well_row))
      )
    }
    plate <- dplyr::left_join(plate, rowkey, by = "well_row")
  }
  #Just need the sample type for a start
  if (! "sample_type" %in% names(plate)) {
    message("plate does not contain variable sample_type")
  }
  return(dplyr::arrange(plate, well_row, well_col))
}
test = label_plate_rowcol(plate = create_blank_plate( well_row = LETTERS[1:8],
                                                      well_col = 1:12),
                          rowkey = sample_type_key(sample_type = c('test','negative','positive'), plate_type = 96))

#DISPLAY BLANK
display_blank_plate = function(plate){
  library(ggplot2)

  display_plate <- function(plate) {

    assertthat::assert_that(
      assertthat::has_name(plate,
                           c("well_row","well_col")))

    rowlevels <-
      dplyr::pull(plate, well_row) %>%
      as.factor() %>%
      levels()

    ggplot2::ggplot(data = plate,
                    ggplot2::aes(x = as.factor(well_col),
                                 y = as.factor(well_row))) +
      ggplot2::scale_x_discrete(expand = c(0, 2), position = "top") +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rowlevels) ) +
      ggplot2::coord_equal(ratio = 1) + #aspect ratio fix
      ggplot2::theme_void() + #must be void to stop ggplot from adding an extra grid line and formatting data wrong
      ggplot2::theme(axis.text = ggplot2::element_text(angle = 40),
                     panel.grid.major = ggplot2::element_blank(),
                     legend.position = "none",
                     plot.margin = grid::unit(rep(0.01, 4), "native"), #npc = normalized parent coordinates
                     panel.border = ggplot2::element_blank())
  }

  display_plate(plate) +
    geom_tile(aes(fill = ""), colour = "black")

}
display_blank_plate(test)

#DISPLAY GENERAL CONCEPT-TNP
display_plate_general_concept <- function(plate) {
  assertthat::assert_that(
    assertthat::has_name(plate,
                         c("sample_type",
                           "sample_type_code")))

  display_plate(plate) +
    ggplot2::geom_tile(ggplot2::aes(fill = sample_type),
                       alpha = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sample_type),
                       size = 5, lineheight = 1)
}
#run
display_plate_general_concept(test)

#MOD META-OLD
mod_meta = function(nfdmeta, nfddata, cols_to_remove = NULL,  bycol = "well", method = NULL){

  if(is.null(method) || method == 1L){
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

  } else { #just here for educational purposes, a poor method?levels to this stuff
    df_list <- list(nfdmeta, nfddata)
    df <- Reduce(function(x, y) merge(x, y, all=T), df_list)
    df %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    df

  }

}

