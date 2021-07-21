
#' Title
#'
#' @param img
#'
#' @return
#'
#' @examples
invert_image <- function(img){
  map_invert <- function(x,y) list(x=x, y=(y-imager::height(img))*-1)

  inv_img <- imager::imwarp(img, map_invert)

  return(inv_img)
}


#' Title
#'
#' @param num_wells
#'
#' @return
#'
#' @examples
get_well_properties <- function(num_wells) {
  ## SBS PLATE PROPERTIES
  sbs_dims <- c(127.76, 85.48)
  if (num_wells == 96) {
    plate_dims <- c(12, 8)
    A1_mm <- c(14.38, 11.24)
    inter_well_space <- 9
  } else if (num_wells == 384) {
    plate_dims <- c(24, 16)
    A1_mm <- c(12.13, 8.99)
    inter_well_space <- 4.5
  } else if (num_wells == 1536) {
    plate_dims <- c(48, 32)
    A1_mm <- c(11.005, 7.865)
    inter_well_space <- 2.25
  }

  return(list(num_wells = num_wells, plate_dims = plate_dims, A1_mm = A1_mm,
              inter_well_space = inter_well_space, sbs_dims = sbs_dims))
}


#' Title
#'
#' @param img
#'
#' @return
#'
#' @examples
get_img_scale_factor <- function(img){
  img_dims <- dim(img)
  max_dim <- max(img_dims)

  scale_factor <- max_dim / 720 # this is arbitrary; will use to limit any displayed image to max 720p in any axis

  return(scale_factor)
}


#' Title
#'
#' @param well
#'
#' @return
#'
#' @examples
well_to_row_col <- function(well){
  row_Ls <- grep("[a-zA-Z]", unlist(strsplit(well, "")), value = T)
  num_letters <- length(row_Ls)

  if(num_letters == 1){
    row_N <- which(LETTERS == row_Ls[1])
  } else if(num_letters == 2){
    row_N <- (which(LETTERS == row_Ls[1]) - 1) * 4 + which(letters == row_Ls[2])
  }

  col <- as.numeric(substr(well, start = num_letters + 1, stop = nchar(well)))

  return(list(row=row_N, col=col))
}


#' Title
#'
#' @param plate_img
#' @param plate_type
#' @param well_properties
#'
#' @return
#'
#' @examples
calculate_scale_wells <- function(plate_img, well_properties, plate_type){
  scale_factor <- get_img_scale_factor(plate_img)
  scaled_img <- imager::imresize(plate_img, 1/scale_factor)

  ## If 2 known well positions from the user
  identifiable_wells <- readline(prompt = "Can you identify 2 well positions in your alignment image? y/n: ")
  if(identifiable_wells == "y"){
    ## 1. ask user for which wells they will input and grab coordinates
    well_1 <- readline(prompt = "Please enter the well position of the first identifiable well (e.g. A1 or Cb24): ")
    well_1 <- well_to_row_col(well_1)
    print("Now select the center point of that well.")
    coord_1 <- imager::grabPoint(scaled_img) * scale_factor

    well_2 <- readline(prompt = "Please enter the well position of the second identifiable well (e.g. A1 or Cb24): ")
    well_2 <- well_to_row_col(well_2)
    print("Now select the center point of that well.")
    coord_2 <- imager::grabPoint(scaled_img) * scale_factor

    ## 2. calculate scale_properties from identified coordinates
    ##  i. calculate theoretical distance
    column_1 <- well_1$col
    column_2 <- well_2$col
    well_x_dist <- column_1 - column_2
    mm_x_dist <- well_x_dist * well_properties$inter_well_space

    row_1 <- well_1$row
    row_2 <- well_2$row
    well_y_dist <- row_1 - row_2
    mm_y_dist <- well_y_dist * well_properties$inter_well_space

    ##  ii. calculate image distance
    img_x_dist <- coord_1[1] - coord_2[1]
    img_y_dist <- coord_1[2] - coord_2[2]

    ## TODO: 3. check for consistency between axes
    px_per_mm <- ifelse(well_x_dist != 0, img_x_dist / mm_x_dist, img_y_dist / mm_y_dist)

    ## 4. get top left plate corner from well position
    ## i. calculate A1 position
    well_1_to_col_1 <- column_1 - 1
    well_1_to_row_A <- row_1 - 1

    x_coord_A1 <- coord_1[1] - (well_1_to_col_1 * well_properties$inter_well_space * px_per_mm)
    y_coord_A1 <- coord_1[2] - (well_1_to_row_A * well_properties$inter_well_space * px_per_mm)

    tl_corner <- c(x_coord_A1 - well_properties$A1_mm[1] * px_per_mm,
                   y_coord_A1 - well_properties$A1_mm[2] * px_per_mm)

    return(list(px_per_mm = px_per_mm, tl_corner = tl_corner))
  } else {
    ## Else if no known coordinate positions are known
    if (plate_type == "6-well") {
      print("Select the centerpoint of the plate")
      center_coord <- imager::grabPoint(scaled_img) * scale_factor

      print("Select the bottom of the '1'")
      one_coord <- imager::grabPoint(scaled_img) * scale_factor

      ## calculate scale
      x_dist <- center_coord[1] - one_coord[1]
      px_per_mm <- x_dist / 25.4  # TODO

      tl_corner <- c(center_coord[1] - px_per_mm * well_properties$sbs_dims[1] / 2 + 27.5,
                     center_coord[2] - px_per_mm * well_properties$sbs_dims[2] / 2 + 15)

      return(list(px_per_mm = px_per_mm, tl_corner = tl_corner))

    } else if (plate_type == "1-well") {
      print("We cannot currently autoscale 1-well plates. You will have to attempt this manually.")
      return(list(px_per_mm = 15, tl_corner = c(-100, 50)))
    }
  }
}


#' Title
#'
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#'
#' @return
#'
#' @examples
make_well_frame <- function(pp, px_per_mm, tl_corner) {
  ## make table of all well locations in the image
  wells <- expand.grid(well_column = 1:pp$plate_dims[1],
                       well_row = 1:pp$plate_dims[2])

  ## Get A1 position on image
  A1_px <- c(pp$A1_mm[1] * px_per_mm + tl_corner[1],
             pp$A1_mm[2] * px_per_mm + tl_corner[2])

  ## get all well positions on image
  wells$px_column <- round(A1_px[1] + pp$inter_well_space * px_per_mm * (wells$well_column - 1))
  wells$px_row <- round(A1_px[2] + pp$inter_well_space * px_per_mm * (wells$well_row - 1))

  return(wells)
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#' @param well_ratio
#'
#' @return
#'
#' @examples
draw_wells <- function(plate_img, pp, px_per_mm, tl_corner, well_ratio) {
  d <- (pp$inter_well_space * px_per_mm) * well_ratio / 2

  ## make table of all well locations in the image
  wells <- make_well_frame(pp, px_per_mm, tl_corner)

  scale_factor <- get_img_scale_factor(plate_img)
  imager::draw_circle(imager::imresize(plate_img, 1/scale_factor),
                      x = wells$px_column / scale_factor,
                      y = wells$px_row / scale_factor,
                      radius = d / scale_factor,
                      color = "red",
                      filled = T,
                      opacity = 0.3)
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param scale_properties
#'
#' @return
#'
#' @examples
interactive_scale_wells <- function(plate_img, pp, scale_properties) {
  cat("Manually align the wells:\n
      use arrow keys to move the grid around\n
      wasd keys will move the grid faster\n
      page up and page down keys will grow and shrink the grid\n
      1 and 2 keys will shrink and grow the well size\n
      Press Esc when you are finished\n")
  px_per_mm <- scale_properties$px_per_mm
  tl_corner <- scale_properties$tl_corner
  if(is.na(scale_properties$well_ratio)){
    well_ratio <- 0.5
  } else {
    well_ratio <- scale_properties$well_ratio
  }

  f <- function(state) {
    if (state$key == "space") {
      stop("User exited application; spacebar pressed")
    }
    if (state$key == "arrowleft") {
      tl_corner <<- c(tl_corner[1] - 1, tl_corner[2])
    }
    if (state$key == "arrowright") {
      tl_corner <<- c(tl_corner[1] + 1, tl_corner[2])
    }
    if (state$key == "arrowup") {
      tl_corner <<- c(tl_corner[1], tl_corner[2] - 1)
    }
    if (state$key == "arrowdown") {
      tl_corner <<- c(tl_corner[1], tl_corner[2] + 1)
    }
    if (state$key == "a") {
      tl_corner <<- c(tl_corner[1] - 10, tl_corner[2])
    }
    if (state$key == "d") {
      tl_corner <<- c(tl_corner[1] + 10, tl_corner[2])
    }
    if (state$key == "w") {
      tl_corner <<- c(tl_corner[1], tl_corner[2] - 10)
    }
    if (state$key == "s") {
      tl_corner <<- c(tl_corner[1], tl_corner[2] + 10)
    }
    if (state$key == "pageup") {
      px_per_mm <<- px_per_mm + min(px_per_mm * 0.01, 0.1)
      print(paste("px_per_mm: ", px_per_mm))
    }
    if (state$key == "pagedown") {
      px_per_mm <<- px_per_mm - min(px_per_mm * 0.01, 0.1)
      print(paste("px_per_mm: ", px_per_mm))
    }
    if (state$key == "2") {
      well_ratio <<- well_ratio + 0.02
      print(paste("well_ratio: ", well_ratio))
    }
    if (state$key == "1") {
      well_ratio <<- well_ratio - 0.02
      print(paste("well_ratio: ", well_ratio))
    }

    draw_wells(plate_img, pp, px_per_mm, tl_corner, well_ratio)
  }

  imager::interact(f,
                   title = "Press Esc to accept or Space to exit")

  return(list(px_per_mm = px_per_mm, tl_corner = tl_corner, well_ratio = well_ratio))
}


#' Title
#'
#' @param plate_img
#' @param num_wells
#' @param experiment_type
#' @param plate_type
#' @param well_ratio
#'
#' @return
#'
#' @examples
get_scale_and_location <- function(plate_img, num_wells, experiment_type, plate_type, well_ratio) {
  scale_success <- F
  pp <- get_well_properties(num_wells)

  scale_properties <- calculate_scale_wells(plate_img, pp, plate_type)
  scale_properties <- c(scale_properties, well_ratio=well_ratio)

  well_img <- as.data.frame(plate_img, wide = "c") %>%
    dplyr::mutate(c.2 = .data$c.1, c.3 = .data$c.1) %>%
    tidyr::pivot_longer(cols = 3:5, names_to = "cc", values_to = "value") %>%
    dplyr::mutate(cc = as.numeric(as.factor(.data$cc))) %>%
    imager::as.cimg()

  print(scale_properties)
  print("Please check for correct alignment")
  scale_properties <- interactive_scale_wells(well_img, pp, scale_properties)

  return(scale_properties)
}


#' Title
#'
#' @param plate_img
#' @param px_per_mm
#' @param tl_corner
#' @param well_ratio
#' @param num_wells
#'
#' @return
#'
#' @examples
get_scale_or_location <- function(plate_img, num_wells, px_per_mm, tl_corner, well_ratio) {
  pp <- get_well_properties(num_wells)

  if(is.na(px_per_mm)){
    px_per_mm <- 15
  }
  if(is.na(tl_corner)){
    tl_corner <- c(0, 0)
  }
  if(is.na(well_ratio)){
    well_ratio <- 0.5
  }
  scale_properties <- list(px_per_mm = px_per_mm, tl_corner = tl_corner, well_ratio = well_ratio)

  well_img <- as.data.frame(plate_img, wide = "c") %>%
    dplyr::mutate(c.2 = .data$c.1, c.3 = .data$c.1) %>%
    tidyr::pivot_longer(cols = 3:5, names_to = "cc", values_to = "value") %>%
    dplyr::mutate(cc = as.numeric(as.factor(.data$cc))) %>%
    imager::as.cimg()

  print("Please correct alignment")
  scale_properties <- interactive_scale_wells(well_img, pp, scale_properties)

  return(scale_properties)
}


#' Title
#'
#' @param img_idx
#' @param img_settings_df
#'
#' @return
#'
#' @examples
get_img_settings <- function(img_idx, img_settings_df){
  settings_idx <- (img_idx - 1) %% nrow(img_settings_df) + 1

  this_img_settings <- img_settings_df[settings_idx,]
  this_img_settings$iteration <- (img_idx - 1) %/% nrow(img_settings_df) + 1
  return(this_img_settings)
}


#' Title
#'
#' @param img_file
#' @param invert
#' @param well_frame
#' @param img_idx
#' @param img_settings_df
#' @param circ_stencil
#' @param rotate
#' @param normalise
#'
#' @return
#'
#' @examples
summarise_image <- function(img_file, normalise, invert, rotate, well_frame, img_idx,
                            img_settings_df, circ_stencil) {
  img <- imager::load.image(img_file)

  # normalise by negating image from same set at iteration 0
  if(normalise){
    settings_idx <- (img_idx - 1) %% nrow(img_settings_df)
    neg_img_file <- file.path(dirname(img_file),
                              paste(stringr::str_pad(settings_idx, width = 6,
                                                     side = "left", pad = "0"),
                                    ".png", sep = ""))
    neg_img <- imager::load.image(neg_img_file)
    img <- img - neg_img
  }

  # transform image if necessary
  if (invert) {
    img <- invert_image(img)
  }
  if (rotate) {
    img <- imager::imrotate(img, 180)
  }

  this_frame <- well_frame
  this_frame$id <- tools::file_path_sans_ext(basename(img_file))

  img_settings <- get_img_settings(img_idx, img_settings_df)
  this_frame <- cbind(this_frame, img_settings)

  this_frame <- this_frame %>%
    dplyr::rowwise() %>%
    dplyr::mutate(point_value = imager::at(img,
                                           x = .data$px_column,
                                           y = .data$px_row)) %>%
    dplyr::mutate(value = list(imager::get.stencil(img, circ_stencil,
                                                   x = .data$px_column,
                                                   y = .data$px_row))) %>%
    dplyr::mutate(mean = mean(.data$value),
                  median = median(.data$value),
                  sd = sd(.data$value),
                  # mad = stats::mad(.data$value),
                  max = max(.data$value),
                  min = min(.data$value),
                  n_saturated = sum(.data$value == 1)) %>%
    dplyr::select(-.data$value)
}


#' Process a folder of images
#'
#' @param dir_path path to directory containing plate images (.png images only)
#' @param align_filename filename of image used to align well grid
#' @param invert Boolean flag. Set to {TRUE} if the plate is inverted in the imager
#' @param layout_csv .csv with metadata for each position on the plate
#' @param experiment_type "colony" or "lawn"
#' @param plate_type "6-well" or "1-well"
#' @param num_wells Plate size used for output positions: 96, 384 or 1536
#' @param px_per_mm If you already know the `px_per_mm` value (from previous alignment)
#' @param tl_corner If you already know the `tl_corner` values (from previous alignment)
#' @param well_ratio Proportion of space taken up by each "well". Can be scaled interactively.
#' @param in_parallel process images in parallel (only available for R >= 4.0)
#' @param img_settings_list a list of named vectors with meta-data for images in the order they were taken i.e.
#' `img_settings_list = list(c(panel="blue", exposure=20000, intensity=1),c(panel="red", exposure=4000, intensity=0.7))`
#' @param rotate Boolean flag. Set to {TRUE} if the images are upside-down i.e. row A on bottom of image
#' @param normalise Attempts to remove background. Negates pixel values in the first iteration of images, from all images.
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
#'
#' @examples
process_img_dir <- function(dir_path, align_filename, invert=F, rotate=F,
                            layout_csv, experiment_type="colony", normalise=F,
                            plate_type="6-well", num_wells=384, px_per_mm=NA,
                            tl_corner=NA, well_ratio=NA, in_parallel=FALSE,
                            img_settings_list) {

  img_settings_df <- as.data.frame(split(unlist(img_settings_list),
                                         names(unlist(img_settings_list))))

  # Use single image to get pixel-to-well mapping ---------------------------

  align_img <- imager::load.image(file.path(dir_path, align_filename))
  if (invert) {
    align_img <- invert_image(align_img)  # rotate image if upside down
  }
  if (rotate) {
    align_img <- imager::imrotate(align_img, 180)  # rotate image if upside down
  }

  if (is.na(px_per_mm) & is.na(tl_corner)) {
    scale_properties <- get_scale_and_location(plate_img = align_img,
                                               num_wells = num_wells,
                                               experiment_type = experiment_type,
                                               plate_type = plate_type,
                                               well_ratio = well_ratio)
  } else if(is.na(px_per_mm) | is.na(tl_corner) | is.na(well_ratio)){
    scale_properties <- get_scale_or_location(plate_img = align_img,
                                              num_wells = num_wells,
                                              px_per_mm = px_per_mm,
                                              tl_corner = tl_corner,
                                              well_ratio = well_ratio)
  } else {
    scale_properties <- list(px_per_mm=px_per_mm, tl_corner=tl_corner, well_ratio=well_ratio)
  }
  print(scale_properties)

  pp <- get_well_properties(num_wells)
  print("Extracted microtitre plate properties")

  well_frame <- make_well_frame(pp = pp,
                                px_per_mm = scale_properties$px_per_mm,
                                tl_corner = scale_properties$tl_corner)

  print("Created digitial microtitre plate")

  ## make well mask
  d <- (pp$inter_well_space * scale_properties$px_per_mm) * scale_properties$well_ratio / 2
  stencil <- expand.grid(dx = -d:d,
                         dy = -d:d)
  circ_stencil <- round(subset(stencil, (dx^2 + dy^2) < d^2))
  print("Created a pixel-to-well mapping")

  # Summarise values in each well, across all images in a directory ---------

  ## get all images
  img_files <- list.files(path = dir_path, pattern = utils::glob2rx("*.png"),
                          full.names = T, recursive = F, include.dirs = T)

  print("Extracting summary from each image")
  if (!in_parallel) {
    # sequentially process
    pb <- utils::txtProgressBar(min = 0, max  = length(img_files), style = 3)
    all_data <- c()
    for (img_idx in seq_along(img_files)) {
      img_file <- img_files[img_idx]

      utils::setTxtProgressBar(pb, img_idx)

      this_frame <- summarise_image(img_file, normalise, invert, rotate,
                                    well_frame, img_idx, img_settings_df,
                                    circ_stencil)

      all_data <- rbind(all_data, this_frame)
    }
  } else if (in_parallel) {
    ## in parallel...
    doFuture::registerDoFuture()
    future::plan(future::multisession)

    # keep track of our progress
    progressr::handlers(global = TRUE)
    p <- progressr::progressor(along = img_files)

    ## summarise each well in each image
    all_data <- foreach::foreach(img_file = img_files,
                                 .combine = rbind,
                                 .inorder = F) %dopar% {
                                   p(sprintf("x=%s", img_file))

                                   img_idx <- which(img_file == img_files)

                                   this_frame <- summarise_image(img_file,
                                                                 normalise,
                                                                 invert,
                                                                 rotate,
                                                                 well_frame,
                                                                 img_idx,
                                                                 img_settings_df,
                                                                 circ_stencil)
                                 }
  }

  # write summary to .csv
  utils::write.csv(all_data,
                   file.path(dir_path, paste(format(Sys.time(), "%y%m%d_%H%M_"), "data_summary.csv", sep = "")),
                   row.names = F)

}
