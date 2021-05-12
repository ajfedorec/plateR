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
#' @param x
#' @param y
#' @param radius
#' @param plate_img
#'
#' @return
#'
#' @examples
draw_samples <- function(plate_img, x, y, radius) {
  scale_factor <- get_img_scale_factor(plate_img)

  imager::draw_circle(imager::imresize(plate_img, 1/scale_factor),
                      x = x/scale_factor,
                      y = y/scale_factor,
                      radius = radius/scale_factor,
                      color = 0)
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#'
#' @return
#'
#' @examples
draw_wells <- function(plate_img, pp, px_per_mm, tl_corner) {
  d <- (pp$inter_well_space * px_per_mm) / 2.2

  ## make table of all well locations in the image
  wells <- make_well_frame(pp, px_per_mm, tl_corner)

  scale_factor <- get_img_scale_factor(plate_img)
  imager::draw_circle(imager::imresize(plate_img, 1/scale_factor),
                      x = wells$px_column / scale_factor,
                      y = wells$px_row / scale_factor,
                      radius = d / scale_factor,
                      color = 0,
                      opacity = 0.5)
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#'
#' @return
#'
#' @examples
draw_well_grid <- function(plate_img, pp, px_per_mm, tl_corner) {
  ## make table of all well locations in the image
  wells <- make_well_frame(pp, px_per_mm, tl_corner)

  well_img <- data.frame(x = round(wells$px_column),
                         y = round(wells$px_row),
                         x0 = round(wells$px_column - pp$inter_well_space * px_per_mm / 2),
                         y0 = round(wells$px_row - pp$inter_well_space * px_per_mm / 2),
                         x1 = round(wells$px_column + pp$inter_well_space * px_per_mm / 2),
                         y1 = round(wells$px_row + pp$inter_well_space * px_per_mm / 2),
                         value = 1)

  scale_factor <- get_img_scale_factor(plate_img)

  imager::draw_rect(imager::imresize(plate_img, 1/scale_factor),
                    x0 = well_img$x0 / scale_factor,
                    y0 = well_img$y0 / scale_factor,
                    x1 = well_img$x1 / scale_factor,
                    y1 = well_img$y1 / scale_factor,
                    color = 0, filled = T, opacity = 0.3)
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
  px_per_mm <- scale_properties$px_per_mm
  tl_corner <- scale_properties$tl_corner

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
    }
    if (state$key == "pagedown") {
      px_per_mm <<- px_per_mm - min(px_per_mm * 0.01, 0.1)
    }

    draw_well_grid(plate_img, pp, px_per_mm, tl_corner)
  }
  imager::interact(f, title = "Press Esc to accept or Space to exit")

  return(list(px_per_mm = px_per_mm, tl_corner = tl_corner))
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param plate_type
#'
#' @return
#'
#' @examples
interactive_scale_lawn <- function(plate_img, pp, plate_type) {
  scale_factor <- get_img_scale_factor(plate_img)
  scaled_img <- imager::imresize(plate_img, 1/scale_factor)

  if (plate_type == "6-well") {
    print("Select the centerpoint of the plate")
    center_coord <- imager::grabPoint(scaled_img) * scale_factor

    print("Select the bottom of the '1'")
    one_coord <- imager::grabPoint(scaled_img) * scale_factor

    ## calculate scale
    x_dist <- center_coord[1] - one_coord[1]
    px_per_mm <- x_dist / 25.4  # TODO

    tl_corner <- c(center_coord[1] - px_per_mm * pp$sbs_dims[1] / 2 + 27.5,
                   center_coord[2] - px_per_mm * pp$sbs_dims[2] / 2 + 15)

    return(list(px_per_mm = px_per_mm, tl_corner = tl_corner))

  } else if (plate_type == "1-well") {
    print("Select well A1")
    A1_coord <- imager::grabPoint(scaled_img) * scale_factor

    print("Select well A24")
    A24_coord <- imager::grabPoint(scaled_img) * scale_factor

    ## calculate scale
    x_dist <- A24_coord[1] - A1_coord[1]
    px_per_mm <- x_dist / (pp$inter_well_space * (pp$plate_dims[1] - 1))

    tl_corner <- c(A1_coord[1] - pp$A1_mm[1] * px_per_mm,
                   A1_coord[2] - pp$A1_mm[2] * px_per_mm)

    return(list(px_per_mm = px_per_mm, tl_corner = tl_corner))
  }
}


#' Title
#'
#' @param plate_img
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#'
#' @return
#'
#' @examples
show_plate_overlay <- function(plate_img, pp, px_per_mm, tl_corner) {
  well_frame <- make_well_frame(pp, px_per_mm, tl_corner)

  df_plate_img <- as.data.frame(plate_img)

  plt <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = df_plate_img,
                         ggplot2::aes(.data$x, .data$y, fill = .data$value)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = tl_corner[1],
                                    xmax = tl_corner[1] + pp$sbs_dims[1] * px_per_mm,
                                    ymin = tl_corner[2],
                                    ymax = tl_corner[2] + pp$sbs_dims[2] * px_per_mm),
                       colour = "green", fill = NA) +
    ggplot2::geom_text(ggplot2::aes(label = "A1",
                                    tl_corner[1] + pp$A1_mm[1] * px_per_mm,
                                    tl_corner[2] + pp$A1_mm[2] * px_per_mm)) +
    ggplot2::geom_tile(data = well_frame,
                       ggplot2::aes(.data$px_column, .data$px_row),
                       colour = "blue", fill = NA) +
    # geom_point(data = wells, aes(px_column, px_row),
    #            colour = "blue", fill = NA) +
    ggplot2::scale_y_continuous(trans = scales::reverse_trans()) +
    ggplot2::scale_fill_gradient(high = "white", low = "black") +
    ggplot2::theme_bw()

  methods::show(plt)
}


#' Title
#'
#' @param plate_img
#' @param num_wells
#' @param experiment_type
#' @param plate_type
#'
#' @return
#'
#' @examples
get_scale <- function(plate_img, num_wells, experiment_type, plate_type) {
  scale_success <- F
  pp <- get_well_properties(num_wells)

  init_colony_scale_guess <- list(px_per_mm = 15, tl_corner = c(-100, 50))

  while (!scale_success) {
    if (experiment_type == "lawn") {
      scale_properties <- interactive_scale_lawn(plate_img, pp, plate_type)
    } else if (experiment_type == "colony") {
      scale_properties <- interactive_scale_wells(plate_img, pp,
                                                  init_colony_scale_guess)
    }

    # check if scaling looks good
    print("Please wait for some processing...")

    imager::display(draw_wells(plate_img, pp,
                               scale_properties$px_per_mm,
                               scale_properties$tl_corner))


    # show_plate_overlay(plate_img, pp,
    #                    scale_properties$px_per_mm,
    #                    scale_properties$tl_corner)

    success_in <- readline(prompt = "Does the grid align correctly? y/n: ")

    if (success_in == "y") {
      scale_success <- T
    } else {
      init_colony_scale_guess <- scale_properties
    }
  }

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


#' Process a folder of images
#'
#' @param dir_path path to directory containing plate images (.png images only)
#' @param align_filename filename of image used to align well grid
#' @param invert Boolean flag. Set to TRUE if the images are upside-down
#' @param layout_csv .csv with metadata for each position on the plate
#' @param experiment_type "colony" or "lawn"
#' @param plate_type "6-well" or "1-well"
#' @param num_wells Plate size used for output positions: 96, 384 or 1536
#' @param px_per_mm
#' @param tl_corner
#' @param in_parallel
#' @param img_settings_list
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
#'
#' @examples
process_img_dir <- function(dir_path, align_filename, invert=F, layout_csv,
                            experiment_type="colony", plate_type="6-well",
                            num_wells=384, px_per_mm=NA, tl_corner=NA,
                            in_parallel=FALSE, img_settings_list) {
  setwd(dir_path)

  img_settings_df <- as.data.frame(split(unlist(img_settings_list),
                                         names(unlist(img_settings_list))))

  # Use single image to get pixel-to-well mapping ---------------------------

  align_img <- imager::load.image(align_filename)
  if (invert) {
    align_img <- imager::imrotate(align_img, 180)  # rotate image if upside down
  }

  if (is.na(px_per_mm) | is.na(tl_corner)) {
    scale_properties <- get_scale(plate_img = align_img,
                                  num_wells,
                                  experiment_type,
                                  plate_type)
  } else {
    scale_properties <- list(px_per_mm=px_per_mm, tl_corner=tl_corner)
  }
  print(scale_properties)

  pp <- get_well_properties(num_wells)
  print("Extracted microtitre plate properties")

  well_frame <- make_well_frame(pp = pp,
                                px_per_mm = scale_properties$px_per_mm,
                                tl_corner = scale_properties$tl_corner)

  print("Created digitial microtitre plate")

  ## make well mask
  # d <- min((pp$inter_well_space * scale_properties$px_per_mm) / 4, 5)  # limit the well radius to 5 pixels
  d <- (pp$inter_well_space * scale_properties$px_per_mm) / 2.2
  # d <- 1
  stencil <- expand.grid(dx = -d:d,
                         dy = -d:d)
  circ_stencil <- round(subset(stencil, (dx^2 + dy^2) < d^2))
  # circ_stencil <- round(stencil)
  print("Created a pixel-to-well mapping")

  # Summarise values in each well, across all images in a directory ---------

  ## get all images
  img_files <- list.files(path = dir_path, pattern = utils::glob2rx("*.png"),
                          full.names = T, recursive = F, include.dirs = T)

  print("Extracting summary from each image")
  if (!in_parallel) {
    # sequentially process
    pb <- utils::txtProgressBar(max  = length(img_files), style = 3)
    all_data <- c()
    for (img_idx in seq_along(img_files)) {
      img_file <- img_files[img_idx]

      utils::setTxtProgressBar(pb, img_idx)
      # p(sprintf("x=%s", img_file))

      img <- imager::load.image(img_file)
      if (invert) {
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
      all_data <- rbind(all_data, this_frame)
    }
  } else if (in_parallel) {
    ## in parallel...
    doFuture::registerDoFuture()
    future::plan(future::multisession)

    # keep track of our progress
    # progressr::handlers(global = TRUE)
    # p <- progressr::progressor(along = img_files)

    ## summarise each well in each image
    all_data <- foreach::foreach(img_file = img_files,
                                 .combine = rbind,
                                 .inorder = F) %dopar% {
                                   p(sprintf("x=%s", img_file))
                                   img <- imager::load.image(img_file)
                                   if (invert) {
                                     img <- imager::imrotate(img, 180)
                                   }

                                   this_frame <- well_frame
                                   this_frame$id <- tools::file_path_sans_ext(basename(img_file))

                                   this_frame <- this_frame %>%
                                     dplyr::rowwise() %>%
                                     dplyr::mutate(value = list(imager::get.stencil(img,
                                                                                    circ_stencil,
                                                                                    x = .data$px_column,
                                                                                    y = .data$px_row))) %>%
                                     dplyr::mutate(mean = mean(value),
                                                   median = median(value),
                                                   sd = sd(value),
                                                   mad = stats::mad(value),
                                                   max = max(value),
                                                   min = min(value),
                                                   n_saturated = sum(value == 1))
                                 }
  }

  # write summary to .csv
  write.csv(all_data,
            paste(dir_path, format(Sys.time(), "%y%m%d_%H%M_"), "data_summary.csv", sep = ""),
            row.names = F)

}
