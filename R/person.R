library(ggplot2)

# get_person <- function(buffer = 0.05, in_x = 1, in_y = 1, share = 1) {
#   implied_x <- sqrt(1 - buffer)
#   implied_y <- implied_x
#
#   # The origin of the person graph needs to be shifted to account for the size of the border
#   offset_x <- (1 - implied_x)/2 + (in_x - 1)
#   offset_y <- (1 - implied_y)/2 + (in_y - 1)
#
#   my_coords <- get_baseline_coords(share)
#   head_x <- offset_x + (my_coords[[1]] * implied_x)
#   head_y <- offset_y + (my_coords[[2]] * implied_y)
#   body_x <- offset_x + (my_coords[[3]] * implied_x)
#   body_y <- offset_y + (my_coords[[4]] * implied_y)
#
#   #print(my_coords)
#
#   ret_val <- data.frame(body_x,
#                         body_y,
#                         head_x = NA,
#                         head_y = NA)
#   n <- length(head_x)
#   ret_val$head_x[1:n] <- head_x
#   ret_val$head_y[1:n] <- head_y
#   ret_val
# }
#
# get_baseline_coords <- function(share) {
#   bottom <- function(x, h = 0, k = 0, r = 1) k - sqrt(r*r - (x - h)*(x - h))
#   top <- function(x, h = 0, k = 0, r = 1) k + sqrt(r*r - (x - h)*(x - h))
#
#   x <- seq(16,34)
#
#   k = 90
#   h = 25
#   r = 9
#
#   bottom_head_x <- seq(14, 36)
#   bottom_head_y <- bottom(bottom_head_x, 25, 114, 11)
#   top_head_x <- rev(bottom_head_x)
#   top_head_y <- top(top_head_x, 25, 114, 11)
#
#   head_x <- c(bottom_head_x, top_head_x)/50
#   head_y = c(bottom_head_y, top_head_y)/125
#
#   hand_x <- seq(0,9)
#   hand_y <- bottom(hand_x, max(hand_x)/2, 0, max(hand_x)/2)
#   foot_x <- seq(0,12)
#   foot_y <- bottom(foot_x, max(foot_x)/2, 0, max(foot_x)/2)
#
#   lower_body_x <- c( 0,  0 + hand_x,  9, 10, 11, 11 + foot_x, 23, 24, 25, 26, 27, 27 + foot_x, 39, 40, 41, 41 + hand_x, 50)
#   lower_body_y <- c(89, 54 + hand_y, 84, 84, 84,  6 + foot_y, 54, 54, 54, 54, 54,  6 + foot_y, 84, 84, 84, 54 + hand_y, 89)
#
#   shoulder_right_x <- seq(from = 50, to = 38, by = -1)
#   shoulder_right_y <- top(shoulder_right_x, h = 38, k = 89, r = 12)
#   shoulder_left_x <- seq(from = 12, to = 0, by = -1)
#   shoulder_left_y <- top(shoulder_left_x, h = 12, k = 89, r = 12)
#
#   upper_body_x <- c(shoulder_right_x, seq(37, 13, -1), shoulder_left_x)
#   upper_body_y <- c(shoulder_right_y,     rep(101, 25), shoulder_left_y)
#
#   body_x = c(lower_body_x, upper_body_x)/50
#   body_y = c(lower_body_y, upper_body_y)/125
#
#   # Pare out portion that are below share
#   body_y <- body_y[body_x <= share]
#   body_x <- body_x[body_x <= share]
#   if (min(head_x, na.rm = TRUE) > share) {
#     head_x <- NA
#     head_y <- NA
#   } else {
#     head_y <- head_y[head_x <= share]
#     head_x <- head_x[head_x <= share]
#   }
#
#   list(head_x, head_y, body_x, body_y)
# }

#' Two-Person Plots
#'
#' @params n 2x1 vector with the number of type 1 people and the number of type 2 people
#' @params nrows Number of rows
#' @params ncols Number of columns
#' @params color Vector of two colors to use for groups (defaults to 'blue' and 'red')
two_person_plot <- function(n, nrows = NULL, ncols = NULL, color = c('blue', 'red')) {
  # n needs to be a two element vector
  if (length(n) != 2 | !is.numeric(n)) stop('Argument N must have 2 elements.')

  total_n <- sum(n)

  # Validate nrow and ncol or create if necessary
  if (is.null(nrows) & is.null(ncols)) {
    # In this case neither is specified.  I will use equally sized rows
    ncols <- ceiling(sqrt(total_n))
    nrows <- ceiling(total_n/ncols)
  } else if (!is.null(nrows) & is.null(ncols)) {
    ncols <- ceiling(total_n/nrows)
  } else if (is.null(nrows) & !is.null(ncols)) {
    nrows <- ceiling(total_n/ncols)
  } else {
    if (nrows * ncols < total_n) {
      # In this case, I will assume the number of rows is accurage
      ncols <- ceiling(total_n/nrows)
    }
  }

  base_plot <- ggplot()
  count_person <- 1
  for (i in seq(nrows, 1, -1)) {
    for (j in seq(1, ncols)) {
      if (count_person <= total_n) {
        if (count_person <= n[[1]]) {
          use_data <- get_person(0.2, j, i, 1)
          base_plot <- base_plot +
            geom_polygon(data = use_data, aes(x = head_x, y = head_y + 0.5), color = color[[1]], fill = color[[1]], size = 0) +
            geom_polygon(data = use_data, aes(x = body_x, y = body_y + 0.5), color = color[[1]], fill = color[[1]], size = 0)
        } else if (count_person > ceiling(n[[1]])) {
          use_data <- get_person(0.2, j, i, 1)
          base_plot <- base_plot +
            geom_polygon(data = use_data, aes(x = head_x, y = head_y + 0.5), color = color[[2]], fill = color[[2]], size = 0) +
            geom_polygon(data = use_data, aes(x = body_x, y = body_y + 0.5), color = color[[2]], fill = color[[2]], size = 0)
        } else {
          # In this case, the person should be split between two colors

          # Second color
          use_data <- get_person(0.2, j, i, 1)
          base_plot <- base_plot +
            geom_polygon(data = use_data, aes(x = head_x, y = head_y + 0.5), color = color[[2]], fill = color[[2]], size = 0) +
            geom_polygon(data = use_data, aes(x = body_x, y = body_y + 0.5), color = color[[2]], fill = color[[2]], size = 0)

          # First color
          this_share <- n[[1]] - count_person + 1
          if (this_share > 0 & this_share < 1) {
            use_data <- get_person(0.2, j, i, this_share)
            base_plot <- base_plot +
              geom_polygon(data = use_data, aes(x = head_x, y = head_y + 0.5), color = color[[1]], fill = color[[1]], size = 0) +
              geom_polygon(data = use_data, aes(x = body_x, y = body_y + 0.5), color = color[[1]], fill = color[[1]], size = 0)
          } else {
            stop('Something wrong in split person section.  Share not between zero and 1.')
          }
        }
      }

      count_person <- count_person + 1
    }
  }

  base_plot <- base_plot +
    xlim(0, ncols) + ylim(0.5, nrows + 0.5) +
    xlab('') + ylab('') +
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())

  base_plot
}

person_plot <- function(x, y = NULL, color = 'gray') {
  # There will be two ways of supplying data to this function:
  #   If y is not supplied, x will be a vector that gives the number of people per line
  #   If y is supplied, it will be the number of people in each line
  #browser()
  per_line = x
  if (!is.null(y)) {
    if (length(y) != 1 | length(x) != 1) stop(sprintf('Invalid data supplied to person_plot:  ', length(x), length(y)))
    per_line <- rep(y, x)
  }

  base_plot <- ggplot()
  for (i in seq_along(per_line)) {
    to_do <- per_line[[i]]
    for (j in seq(ceiling(to_do))) {
      if (to_do - j + 1 >= 1) {
        use_data <- get_person(0.2, j, i, 1)
      } else {
        use_data <- get_person(0.2, j, i, to_do - j + 1)
      }
      base_plot <- base_plot +
        geom_polygon(data = use_data, aes(x = head_x, y = head_y + 0.5), color = 'blue', fill = 'blue') +
        geom_polygon(data = use_data, aes(x = body_x, y = body_y + 0.5), color = 'blue', fill = 'blue')
    }
  }

  base_plot <- base_plot +
    xlim(0, max(per_line)) + ylim(0.5, length(per_line) + 0.5) +
    xlab('') + ylab('') +
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())

  base_plot
}

