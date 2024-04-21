# All plots are made assuming the team class is a list of lists, and the player
# class is a list


# Starplot function: Uhhhhhhh
stars.player <- function(player, vars, ...) {
  stars(player)
}

# Barplot function: Plot one statistic across all players (TODO: choose which
# players to plot statistic across)
barplot.team <- function(team, stat, players, ...) {
  player_name <- vector()
  stat_vec <- vector()
  for (i in seq_along(team)) {
    player_name <- c(player_name, as.character(team[[i]]["Player"]))
    stat_vec <- c(stat_vec, as.numeric(team[[i]][stat]))
  }
  bar_data <- data.frame(player_name, stat_vec)
  barplot(bar_data$stat_vec, names.arg = bar_data$player_name,
          main = paste0(stat, " Between All Players"), ...)
}

# Scatterplot function: Plot the relationship between two stats
plot.team <- function(team, stat1, stat2, ...) {
  stat1_vec <- vector()
  stat2_vec <- vector()
  for (i in seq_along(team)) {
    stat1_vec <- c(stat1_vec, as.numeric(team[[i]][stat1]))
    stat2_vec <- c(stat2_vec, as.numeric(team[[i]][stat2]))
    plot(stat1_vec, stat2_vec,
         xlab = stat1, ylab = stat2,
         main = paste0("Relationship Between ", stat1, " and ", stat2), ...)
  }
}

# example team class to mess around with
bask <- get_data("https://utahstateaggies.com/sports/mens-basketball/stats")
team_ex <- list()
for (i in 1:13) {
  player <- as.list(bask[i, ])
  team[[i]] <- player
}
