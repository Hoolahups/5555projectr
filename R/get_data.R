#' Get  Basketball Data
#'
#' Scrapes player statistics from official NCAA/university basketball sites.
#'
#' A number of NCAA basketball teams (Men's and Women's) use the same webpage template across
#' all schools. This function implements a script that scrapes the individual player
#' statistics and formats them in a data.frame/tibble. It performs a basic check
#' to make sure the data conform to the expected format. Also the names of the players
#' need to be cleaned.
#'
#' @param url A URL of an official team statistics page.
#'
#' @returns A data.frame/tibble (still working on this part)
#'
#' @export
get_data <- function(url) {
  h <- rvest::read_html(url)

  nodes_1 <- h |>
    rvest::html_elements(c("#individual-overall td"))

  if (length(nodes_1) == 0) {
    stop("Page does not conform to expected format")
  }

  nodes_2 <- h |>
    rvest::html_elements(c("#individual-overall th"))

  if (length(nodes_2) == 0) {
    stop("Page titles does not conform to expected format")
  }

  player_stats <- rvest::html_text2(nodes_1)
  stat_names <- rvest::html_text2(nodes_2)

  num_players = length(player_stats) / 27

  if (num_players %% 1 != 0) {
    stop("Number of players is not an integer")
  }

  player_stats_split <- split(player_stats,
                              rep(1:27, times = num_players))

  player_stats_df <- data.frame(numeric(num_players))
  for (i in seq_len(length(player_stats_split))) {
    player_stats_df <- cbind(player_stats_df, player_stats_split[[i]])
  }

  stat_names_rev <- c("zero", "number", stat_names[2:4], "tot_min", "avg_min", stat_names[19:27],
                      "scoring_pts", "scoring_avg", "off_re", "def_re", "tot_re",
                      "avg_re", stat_names[11:15], "Bio")

  names(player_stats_df) <- stat_names_rev

  player_stats_tbl <- tibble::as_tibble(player_stats_df) |>
    dplyr::select(-zero) |>
    dplyr::select(-Bio) |>
    # dplyr::mutate(number = as.numeric(number)) |>
    dplyr::mutate(GP = as.numeric(GP)) |>
    dplyr::mutate(GS = as.numeric(GS)) |>
    dplyr::mutate(GS = ifelse(is.na(GS), 0, GS)) |>
    dplyr::mutate(tot_min = as.numeric(tot_min)) |>
    dplyr::mutate(avg_min = as.numeric(avg_min)) |>
    dplyr::mutate(FGM = as.numeric(FGM)) |>
    dplyr::mutate(FGA = as.numeric(FGA)) |>
    dplyr::rename(FG_pct = `FG%`) |>
    dplyr::mutate(FG_pct = as.numeric(FG_pct)) |>
    dplyr::rename(X3PT = `3PT`) |>
    dplyr::mutate(X3PT = as.numeric(X3PT)) |>
    dplyr::rename(X3PTA = `3PTA`) |>
    dplyr::mutate(X3PTA = as.numeric(X3PTA)) |>
    dplyr::rename(X3PT_pct = `3PT%`) |>
    dplyr::mutate(X3PT_pct = as.numeric(X3PT_pct)) |>
    dplyr::mutate(FTM= as.numeric(FTM)) |>
    dplyr::mutate(FTA = as.numeric(FTA)) |>
    dplyr::rename(FT_pct = `FT%`) |>
    dplyr::mutate(FT_pct = as.numeric(FT_pct)) |>
    dplyr::mutate(scoring_pts = as.numeric(scoring_pts)) |>
    dplyr::mutate(scoring_avg = as.numeric(scoring_avg)) |>
    dplyr::mutate(off_re = as.numeric(off_re)) |>
    dplyr::mutate(def_re = as.numeric(def_re)) |>
    dplyr::mutate(tot_re = as.numeric(tot_re)) |>
    dplyr::mutate(avg_re = as.numeric(avg_re)) |>
    dplyr::mutate(PF = as.numeric(PF)) |>
    dplyr::mutate(AST = as.numeric(AST)) |>
    dplyr::mutate(TO = as.numeric(TO)) |>
    dplyr::mutate(STL = as.numeric(STL)) |>
    dplyr::mutate(BLK = as.numeric(BLK))

  return (player_stats_tbl)

}
