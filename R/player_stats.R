#makes a list of a specific player's stats
player_stats <- function(team, player_name) {
  # get row with the player's name
  player_row <- team[team$Player == player_name, ]

  # If the player is not found return not found
  if (nrow(player_row) == 0) {
    return(list("Player not found"))
  }

  # convert these stats to integers and return them as a list
  player_stats <- as.list(as.numeric(unlist(player_row[,-(1:2)])))

  return(player_stats)
}
