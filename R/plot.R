# All plots are made assuming the team class is a list of lists, and the player
# class is a list hehe here is stuff for a new push


# example team class to mess around with
bask <- get_data("https://utahstateaggies.com/sports/mens-basketball/stats")

# Starplot function: Uhhhhhhh
player_star <- function(team, player, vars, ...) {
  # Default vars: 'FG%', '3PT%', 'FT%', scoring_avg, avg_re, AST, STL, BLK

  # Dividing by games played caused numbers to be 0 later on,
  # so commenting out for now
  # team[[player]]$AST <- as.numeric(team[[player]]$AST) / as.numeric(team[[player]]$GP)
  # team[[player]]$STL <- as.numeric(team[[player]]$STL) / as.numeric(team[[player]]$GP)
  # team[[player]]$BLK <- as.numeric(team[[player]]$BLK) / as.numeric(team[[player]]$GP)

  # Divide each value by average team value
  for (i in 2:length(team[[player]])) {
    team[[player]][i] <- as.numeric(team[[player]][i]) / as.numeric(team$avg[i])
  }

  # Create data frame for radarchart, using first row as max values, second row
  # as min values, and third row as actual values
  stat_min <- rep(0, 8)
  stat_max <- rep(max(as.numeric(team[[player]][2:length(team[[player]])])), 8)
  player_row <- as.numeric(team[[player]][c("FG_pct", "X3PT_pct", "FT_pct", "scoring_avg",
                         "avg_re", "AST", "STL", "BLK")])
  star_data <- data.frame(rbind(stat_max, stat_min, player_row))
  names(star_data) <- c("FG_pct", "X3PT_pct", "FT_pct", "scoring_avg",
                        "avg_re", "AST", "STL", "BLK")

  # Create radar/star plot using fmsb package
  # https://r-graph-gallery.com/142-basic-radar-chart.html
  fmsb::radarchart(star_data, pfcol = grDevices::rgb(0.2,0.5,0.5,0.5),
                   pty = NA)
}

# Barplot function: Plot one statistic across all players (TODO: choose which
# players to plot statistic across)
team_bar <- function(team, stat, players, ...) {
  # Extract player names and stats into two vectors, then put them in a data
  # frame and arrange by descending order
  player_name <- vector()
  stat_vec <- vector()
  for (i in seq_along(team)) {
    player_name <- c(player_name, names(team)[i])
    stat_vec <- c(stat_vec, as.numeric(team[[i]][stat]))
  }
  bar_data <- data.frame(player_name, stat_vec) |>
    dplyr::arrange(stat_vec)

  # Create horizontal bar plot
  ggplot2::ggplot(bar_data, ggplot2::aes(stat_vec, stats::reorder(player_name, stat_vec))) +
    ggplot2::geom_col() +
    ggplot2::xlab(stat) +
    ggplot2::ylab("Player") +
    ggplot2::ggtitle(paste0(stat, " Between All Players"))
}

# Scatterplot function: Plot the relationship between two stats
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
