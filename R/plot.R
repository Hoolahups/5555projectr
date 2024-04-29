#' Create a star plot for a specified player
#'
#' This function creates a star plot for the specified player using selected
#' statistics. It normalizes player stats by team averages and displays them
#' in a star plot.
#'
#' @param team A list representing the team data.
#' @param player A string specifying the player's name, written as
#'  first_lastname.
#' @param vars A character vector of variables to plot, default values
#'  include several percentages and averages.
#' @param ... Additional arguments passed to other methods.
#'
#' @return A radar chart displaying the player's performance across specified
#'  metrics.
#'
#' @examples
#' player_star(usu_2023, "osobor_great")
#'
#' @export
player_star <- function(team, player,
                        vars = c("FG_pct", "X3PT_pct", "FT_pct",
                                 "scoring_avg", "avg_re", "AST", "STL", "BLK"),
                        ...) {
  # Default vars: 'FG%', '3PT%', 'FT%', scoring_avg, avg_re, AST, STL, BLK

  # Dividing by games played caused numbers to be 0 later on,
  # so commenting out for now
  # team[[player]]$AST <- as.numeric(team[[player]]$AST) /
  # as.numeric(team[[player]]$GP)
  # team[[player]]$STL <- as.numeric(team[[player]]$STL) /
  # as.numeric(team[[player]]$GP)
  # team[[player]]$BLK <- as.numeric(team[[player]]$BLK) /
  # as.numeric(team[[player]]$GP)

  # Divide each value by average team value
  for (i in 2:length(team[[player]])) {
    team[[player]][i] <- as.numeric(team[[player]][i]) /
      as.numeric(team$avg[i])
  }

  # Create data frame for radarchart, using first row as max values, second
  # row
  # as min values, and third row as actual values
  stat_min <- rep(0, length(vars))
  stat_max <- rep(max(as.numeric(team[[player]][2:length(team[[player]])])),
                  length(vars))
  player_row <- as.numeric(team[[player]][vars])
  star_data <- data.frame(rbind(stat_max, stat_min, player_row))
  names(star_data) <- vars

  # Create radar/star plot using fmsb package
  # https://r-graph-gallery.com/142-basic-radar-chart.html
  fmsb::radarchart(star_data, pfcol = grDevices::rgb(0.2, 0.5, 0.5, 0.5),
                   pty = NA, title = player)
}

# Dot plot function: Plot one statistic across all players (TODO: choose which
# players to plot statistic across)

#' Create a dot plot for players
#'
#' This function generates a dot plot showing the values of a given statistic
#' across all players. Players can be selected also be specified.
#'
#' @param team A list representing the team data.
#' @param stat A string specifying the statistic to plot.
#' @param players A character vector specifying which players to include
#' in the plot (coming soon).
#' @param ... Additional arguments passed to other methods.
#'
#' @return A baseR dotchart plot.
#'
#' @examples
#' team_dot(usu_2023, 'FG_pct')
#'
#' @export
team_dot <- function(team, stat, players, ...) {
  # Extract player names and stats into two vectors, then put them in a data
  # frame and sort
  player_name <- vector()
  stat_vec <- vector()
  for (i in seq_along(team)) {
    player_name <- c(player_name, names(team)[i])
    stat_vec <- c(stat_vec, as.numeric(team[[i]][stat]))
  }
  dot_data <- data.frame(stat_vec[-length(stat_vec)],
                         player_name[-length(player_name)])
  row.names(dot_data) <- player_name[-length(player_name)]
  dot_data <- dot_data[order(dot_data$stat_vec), ]

  # Create dot plot
  graphics::dotchart(dot_data$stat_vec,
                     labels = row.names(dot_data),
                     xlab = stat)
  graphics::abline(v = stat_vec[length(stat_vec)], lty = "dotted")
}

# Scatterplot function: Plot the relationship between two stats

#' Creates a scatter plot
#'
#' This function plots the relationship between two selected statistics across
#' all team members.
#'
#' @param team A list representing the team data.
#' @param stat1 A string specifying the first statistic.
#' @param stat2 A string specifying the second statistic.
#' @param ... Additional arguments passed to other methods.
#'
#' @return A base R plot showing the scatter plot of the two statistics.
#'
#' @examples
#' compare_stats(usu_2023, 'FG_pct', 'X3PT_pct')
#'
#' @export
compare_stats <- function(team, stat1, stat2, ...) {
  # Extract stats into two vectors
  stat1_vec <- vector()
  stat2_vec <- vector()
  for (i in seq_along(team)) {
    stat1_vec <- c(stat1_vec, as.numeric(team[[i]][stat1]))
    stat2_vec <- c(stat2_vec, as.numeric(team[[i]][stat2]))
  }

  # Plot scatterplot of two variables
  plot(stat1_vec, stat2_vec,
       xlab = stat1, ylab = stat2,
       main = paste0("Relationship Between ", stat1, " and ", stat2))
}
