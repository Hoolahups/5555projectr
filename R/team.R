new_team <- function(players) {
  team_lst <- list(players)
  structure(team_lst, class = "team")

}



new_team <- function(team_df) {
  for (i in seq_len(dim(team_df)[2])) {
    assign(paste0("player", as.character(team_df[i, 1])), as.list(team_df[1, ]))
  }
}
