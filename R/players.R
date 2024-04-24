#' Create List of Players
#'
#' @param team
#'
#' @returns A list of players names on a specified team.
players <- function(team) {
  # maybe filter out team, total, and opponents

  # Extract the player column and convert it to a list
  players <- as.list(team$Player)
  return(players)
}
