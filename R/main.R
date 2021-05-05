#' Title
#'
#' @param pp
#' @param px_per_mm
#' @param tl_corner
#'
#' @return
#'
#' @examples
make_well_frame <- function(pp, px_per_mm, tl_corner){
  ## make table of all well locations in the image
  wells <- expand.grid(well_column = 1:pp$plate_dims[1], well_row = 1:pp$plate_dims[2])

  ## Get A1 position on image
  A1_px <- c(pp$A1_mm[1] * px_per_mm + tl_corner[1], pp$A1_mm[2] * px_per_mm + tl_corner[2])

  ## get all well positions on image
  wells$px_column <- A1_px[1] + pp$inter_well_space * px_per_mm * (wells$well_column -1)
  wells$px_row <- A1_px[2] + pp$inter_well_space * px_per_mm * (wells$well_row -1)

  return(wells)
}


#TODO
#' Title
#'
#' @param num_wells
#'
#' @return
#'
#' @examples
get_well_properties <- function(num_wells){
  ## SBS PLATE PROPERTIES
  sbs_dims <- c(127.76, 85.48)
  if(num_wells == 96){
    plate_dims <- c(12, 8)
    A1_mm <- c(14.38, 11.24)
    inter_well_space = 9
  } else if(num_wells == 384){
    plate_dims <- c(24, 16)
    A1_mm <- c(12.13, 8.99)
    inter_well_space = 4.5
  } else if(num_wells == 1536){
    plate_dims <- c(48, 32)
    A1_mm <- c(11.005, 7.865)
    inter_well_space = 2.25
  }

  return(list(num_wells=num_wells, plate_dims=plate_dims, A1_mm=A1_mm,
              inter_well_space=inter_well_space, sbs_dims=sbs_dims))
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
draw_wells <- function(plate_img, pp, px_per_mm, tl_corner){
  ## make table of all well locations in the image
  wells <- make_well_frame(pp, px_per_mm, tl_corner)

  well_img <- data.frame(x=round(wells$px_column),
                         y=round(wells$px_row),
                         value=1)

  imager::draw_circle(plate_img, x=well_img$x, y=well_img$y, color = 0, radius = px_per_mm)
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
draw_well_grid <- function(plate_img, pp, px_per_mm, tl_corner){
  ## make table of all well locations in the image
  wells <- make_well_frame(pp, px_per_mm, tl_corner)

  well_img <- data.frame(x=round(wells$px_column),
                         y=round(wells$px_row),
                         x0=round(wells$px_column - pp$inter_well_space * px_per_mm / 2),
                         y0=round(wells$px_row - pp$inter_well_space * px_per_mm / 2),
                         x1=round(wells$px_column + pp$inter_well_space * px_per_mm / 2),
                         y1=round(wells$px_row + pp$inter_well_space * px_per_mm / 2),
                         value=1)

  imager::draw_rect(plate_img, x0=well_img$x0, y0=well_img$y0, x1=well_img$x1,
                    y1=well_img$y1, color = 0, filled = T, opacity = 0.3)
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
interactive_scale_wells <- function(plate_img, pp, scale_properties){
  px_per_mm <- scale_properties$px_per_mm # 5 # 16.7 #
  tl_corner <- scale_properties$tl_corner # c(0,0) # c(-37, -67) #

  f <- function(state){
    if(state$key=="arrowleft"){
      tl_corner <<- c(tl_corner[1] - 1, tl_corner[2])
    }
    if(state$key=="arrowright"){
      tl_corner <<- c(tl_corner[1] + 1, tl_corner[2])
    }
    if(state$key=="arrowup"){
      tl_corner <<- c(tl_corner[1], tl_corner[2] - 1)
    }
    if(state$key=="arrowdown"){
      tl_corner <<- c(tl_corner[1], tl_corner[2] + 1)
    }
    if(state$key=="pageup"){
      px_per_mm <<- px_per_mm + px_per_mm * 0.01
    }
    if(state$key=="pagedown"){
      px_per_mm <<- px_per_mm - px_per_mm * 0.01
    }

    draw_well_grid(plate_img, pp, px_per_mm, tl_corner)
  }
  imager::interact(f)

  return(list(px_per_mm=px_per_mm, tl_corner=tl_corner))
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
interactive_scale_lawn <- function(plate_img, pp, plate_type){
  if(plate_type == "6-well"){
    print("Select the centerpoint of the plate")
    center_coord <- imager::grabPoint(plate_img)

    print("Select the bottom of the '1'")
    one_coord <- imager::grabPoint(plate_img)

    ## calculate scale
    x_dist <- center_coord[1] - one_coord[1]
    px_per_mm <- x_dist / 25.2  # TODO

    tl_corner <- c(center_coord[1] - px_per_mm * pp$sbs_dims[1]/2,
                   center_coord[2] - px_per_mm * pp$sbs_dims[2]/2)

    return(list(px_per_mm=px_per_mm, tl_corner=tl_corner))

  } else if(plate_type == "1-well"){

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
show_plate_overlay <- function(plate_img, pp, px_per_mm, tl_corner){
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
get_scale <- function(plate_img, num_wells, experiment_type, plate_type){
  scale_success = F
  pp <- get_well_properties(num_wells)

  init_colony_scale_guess <- list(px_per_mm=10, tl_corner=c(0,0))

  while(!scale_success){
    if(experiment_type == "lawn"){
      scale_properties <- interactive_scale_lawn(plate_img, pp, plate_type)
    } else if(experiment_type == "colony"){
      scale_properties <- interactive_scale_wells(plate_img, pp, init_colony_scale_guess)
    }

    # check if scaling looks good
    print("Please wait for some processing...")
    show_plate_overlay(plate_img, pp, scale_properties$px_per_mm, scale_properties$tl_corner)
    success_in <- readline(prompt = "Does the grid align correctly? y/n: ")
    if(success_in == "y"){
      scale_success = T
    } else {
      init_colony_scale_guess <- scale_properties
    }
  }

  return(scale_properties)
}


#' Title
#'
#' @param img
#' @param well_position
#' @param px_per_mm
#' @param inter_well_space
#'
#' @return
#'
#' @examples
extract_well_values <- function(img, well_position, px_per_mm, inter_well_space){
  well_values <- imager::get.stencil(img, stencil, x=v['px_column'], y=v['px_row'])

  return(data.frame(mean=mean(well_values),
                    median=median(well_values),
                    sd=sd(well_values),
                    mad=stats::mad(well_values),
                    max=max(well_values),
                    min=min(well_values)))

  # dx <- min((inter_well_space * px_per_mm)/4, 5)
  # well_pixels <- expand.grid(px_x = round(well_position['px_column'] - dx):round(well_position['px_column'] + dx),
  #                            px_y = round(well_position['px_row'] - dx):round(well_position['px_row'] + dx))
  #
  # well_pixels$value <- imager::at(img, well_pixels$px_x, well_pixels$px_y)
  #
  # return(well_pixels %>%
  #          dplyr::summarise(mean=mean(value),
  #                           median=median(value),
  #                           sd=sd(value),
  #                           # mad=stats::mad(value),
  #                           max=max(value),
  #                           min=min(value)))

  # return(summarise_well_values(well_pixels))
}


#' Title
#'
#' @param well_pixels
#'
#' @return
#'
#' @examples
summarise_well_values <- function(well_pixels){
  return(data.frame(mean=mean(well_pixels$value),
                    median=median(well_pixels$value),
                    sd=sd(well_pixels$value),
                    # mad=stats::mad(well_pixels$value),
                    max=max(well_pixels$value),
                    min=min(well_pixels$value)))
}


#' Title
#'
#' @param img
#' @param num_wells
#' @param scale_properties
#'
#' @return
#'
#' @examples
extract_img_values <- function(img_file, p, invert, well_frame,  stencil, ){
  p(sprintf("x=%s", img_file))
  img <- imager::load.image(img_file)
  if(invert){ img <- imager::imrotate(img, 180)}


  well_frame$id <- tools::file_path_sans_ext(basename(img_file))


  apply(well_frame, 1, function(v) imager::get.stencil(img, stencil, x=v['px_column'], y=v['px_row']))












  new_frame <- well_frame %>%
    dplyr::group_by(.data$id, .data$well_column, .data$well_row) %>%
    tidyr::nest() %>%
    dplyr::mutate(out = purrr::map(data, ~ extract_well_values(img, unlist(.), scale_properties$px_per_mm, pp$inter_well_space))) %>%
    tidyr::unnest()

  return(new_frame)
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
                            num_wells=384){
  setwd(dir_path)
  # layout <- read.csv(layout_csv)

  ## get all images
  img_files <- list.files(path = dir_path, pattern = utils::glob2rx("*.png"),
                          full.names = T, recursive = F, include.dirs = T)

  ## set up plate layout from single plate
  align_img <- imager::load.image(align_filename)
  if(invert){ align_img <- imager::imrotate(align_img, 180)}

  # scale_properties <- get_scale(plate_img=align_img,
  #                               num_wells,
  #                               experiment_type,
  #                               plate_type)
  scale_properties <- list(px_per_mm=16.9, tl_corner=c(-107, 47))

  pp <- get_well_properties(num_wells)
  well_frame <- make_well_frame(pp = pp,
                                px_per_mm = scale_properties$px_per_mm,
                                tl_corner = scale_properties$tl_corner)

  ## calculate well mask
  d <- min((pp$inter_well_space * scale_properties$px_per_mm)/4, 5)
  stencil <- expand.grid(dx = -d:d,
                         dy = -d:d)
  circ.stencil <- subset(stencil,(dx^2 + dy^2) < d^2)

  ## extract pixel values for all images
  well_frame %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = list(imager::get.stencil(align_img, circ.stencil, x=.data$px_column, y=.data$px_row))) %>%
    dplyr::mutate(mean=mean(value),
                  median=median(value),
                  sd=sd(value),
                  mad=stats::mad(value),
                  max=max(value),
                  min=min(value))


  # stub_img_files <- img_files[1:10]
  #
  progressr::handlers(global = TRUE)
  p <- progressr::progressor(along=img_files)

  doFuture::registerDoFuture()
  future::plan(future::multisession)

  all_data <- foreach::foreach(img_file=img_files, .combine = rbind, .inorder = F) %dopar%
    {
      extract_img_values(img_file, p, invert, num_wells, scale_properties)
    }










  # all_data <- purrr::map(img_files,
  #                        ~ extract_img_values(., p, invert, num_wells, scale_properties)) %>%
  #   purrr::reduce(rbind)


  # future::plan(future::multisession)
  # all_data <- furrr::future_map(img_files,
  #                               ~ extract_img_values(., invert, num_wells, scale_properties),
  #                               .progress = T) %>%
  #   purrr::reduce(rbind)

  write.csv(all_data, paste(dir_path, "data_summary.csv", sep = ""), row.names = F)


  ## normalise

}
