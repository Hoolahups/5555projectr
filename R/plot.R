stars.player <- function(player, vars, ...) {
  stars(player)
}

barplot.team <- function(team, stat, players, ...) {
  player_name <- vector()
  stat_vec <- vector()
  for (i in seq_along(team)) {
    player_name <- c(player_name, as.character(team[[i]]["Player"]))
    stat_vec <- c(stat_vec, as.numeric(team[[i]][stat]))
  }
  bar_data <- data.frame(player_name, stat_vec)
  barplot(bar_data$stat_vec, names.arg = bar_data$player_name, ...)
}


# example team class to mess around with
team <- list()
for (i in 1:13) {
  player <- as.list(bask[i, ])
  team[[i]] <- player
}
