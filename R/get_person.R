#' Get Person Coordinates
#'
#' This function will return a dataframe with the coordinates of a single person on
#' a unit square.
get_baseline_coords <- function() {
  bottom <- function(x, h = 0, k = 0, r = 1) k - sqrt(r*r - (x - h)*(x - h))
  top <- function(x, h = 0, k = 0, r = 1) k + sqrt(r*r - (x - h)*(x - h))

  x <- seq(16,34)

  k = 90
  h = 25
  r = 9

  bottom_head_x <- seq(14, 36)
  bottom_head_y <- bottom(bottom_head_x, 25, 114, 11)
  top_head_x <- rev(bottom_head_x)
  top_head_y <- top(top_head_x, 25, 114, 11)

  head_x <- c(bottom_head_x, top_head_x)/50
  head_y = c(bottom_head_y, top_head_y)/125

  hand_x <- seq(0,9)
  hand_y <- bottom(hand_x, max(hand_x)/2, 0, max(hand_x)/2)
  foot_x <- seq(0,12)
  foot_y <- bottom(foot_x, max(foot_x)/2, 0, max(foot_x)/2)

  lower_body_x <- c( 0,  0 + hand_x,  9, 11, 11 + foot_x, 23, 27, 27 + foot_x, 39, 41, 41 + hand_x, 50)
  lower_body_y <- c(89, 54 + hand_y, 84, 84,  6 + foot_y, 54, 54,  6 + foot_y, 84, 84, 54 + hand_y, 89)

  shoulder_right_x <- seq(from = 50, to = 38, by = -1)
  shoulder_right_y <- top(shoulder_right_x, h = 38, k = 89, r = 12)
  shoulder_left_x <- seq(from = 12, to = 0, by = -1)
  shoulder_left_y <- top(shoulder_left_x, h = 12, k = 89, r = 12)

  upper_body_x <- c(shoulder_right_x, seq(37, 13, -1), shoulder_left_x)
  upper_body_y <- c(shoulder_right_y,     rep(101, 25), shoulder_left_y)

  body_x = c(lower_body_x, upper_body_x)/50
  body_y = c(lower_body_y, upper_body_y)/125

  data.frame(x = c(head_x, NA, body_x), y = c(head_y, NA, body_y))
}

#' Trim person
#'
#' This function will take the coordinates for a full person and trim them.
#' @param in_coords A data.frame of coordinates coming from get_baseline_coords
#' @param threshold Point at which to trim the person
#' @param right Boolean indicating whether to grab right side of threshold (default = FALSE)
#' @export
trim_person <- function(in_coords, threshold = 1.1, right = FALSE) {
  if (right) in_coords$discard <- (in_coords$x < threshold)
  else in_coords$discard <- (in_coords$x > threshold)

  if (!any(in_coords$discard)) return(in_coords)  # If all points are left of threshold, return in_coords whole

  in_coords$diff <- c(NA, diff(in_coords$discard))
  for (i in seq_along(in_coords$diff)) {
    if (!is.na(in_coords$diff[i]) & in_coords$diff[i] == 1) {
      # replace this observation with the end point
      this_pct <- (threshold - in_coords$x[i - 1]) / (in_coords$x[i] - in_coords$x[i - 1])
      in_coords$y[i] <- in_coords$y[i - 1] + (in_coords$y[i] - in_coords$y[i - 1]) * this_pct
      in_coords$x[i] <- threshold
      in_coords$discard[i] <- FALSE
    } else if (!is.na(in_coords$diff[i]) & in_coords$diff[i] == -1) {
      # In this case, change the previous observation
      this_pct <- (threshold - in_coords$x[i - 1]) / (in_coords$x[i] - in_coords$x[i - 1])
      in_coords$y[i - 1] <- in_coords$y[i - 1] + (in_coords$y[i] - in_coords$y[i - 1]) * this_pct
      in_coords$x[i - 1] <- threshold
      in_coords$discard[i - 1] <- FALSE
    }
  }

  subset(in_coords, !discard | is.na(discard))
}
