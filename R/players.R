#makes a list of a specific team's players
players <- function(team) {
  # maybe filter out team, total, and opponents

  # Extract the player column and convert it to a list
  players <- as.list(team$Player)
  return(players)
}
