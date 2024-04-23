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
#' @importFrom rlang .data
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

  player_stats_df <- player_stats_df |>
    dplyr::mutate(Player = stringr::str_extract(.data$Player, "[^\\r\\n]+")) |>
    dplyr::mutate(Player = gsub("\\s*\\r\\s*", "", .data$Player)) |>
    dplyr::mutate(Player = tolower(.data$Player)) |>
    dplyr::mutate(Player = gsub(".*?([a-z]+(?:\\s+[ivx]+)?),\\s*([a-z]+(?:\\s+[ivx]+)?).*", "\\1_\\2", .data$Player)) |>
    dplyr::mutate(Player = gsub("\\s+", "", .data$Player))


  player_stats_tbl <- tibble::as_tibble(player_stats_df) |>
    dplyr::select(-.data$zero) |>
    dplyr::select(-.data$Bio) |>
    # dplyr::mutate(number = as.numeric(number)) |>
    dplyr::mutate(GP = as.numeric(.data$GP)) |>
    dplyr::mutate(GS = as.numeric(.data$GS)) |>
    dplyr::mutate(GS = ifelse(is.na(.data$GS), 0, .data$GS)) |>
    dplyr::mutate(tot_min = as.numeric(.data$tot_min)) |>
    dplyr::mutate(avg_min = as.numeric(.data$avg_min)) |>
    dplyr::mutate(FGM = as.numeric(.data$FGM)) |>
    dplyr::mutate(FGA = as.numeric(.data$FGA)) |>
    dplyr::rename(FG_pct = .data$`FG%`) |>
    dplyr::mutate(FG_pct = as.numeric(.data$FG_pct)) |>
    dplyr::rename(X3PT = .data$`3PT`) |>
    dplyr::mutate(X3PT = as.numeric(.data$X3PT)) |>
    dplyr::rename(X3PTA = .data$`3PTA`) |>
    dplyr::mutate(X3PTA = as.numeric(.data$X3PTA)) |>
    dplyr::rename(X3PT_pct = .data$`3PT%`) |>
    dplyr::mutate(X3PT_pct = as.numeric(.data$X3PT_pct)) |>
    dplyr::mutate(FTM= as.numeric(.data$FTM)) |>
    dplyr::mutate(FTA = as.numeric(.data$FTA)) |>
    dplyr::rename(FT_pct = .data$`FT%`) |>
    dplyr::mutate(FT_pct = as.numeric(.data$FT_pct)) |>
    dplyr::mutate(scoring_pts = as.numeric(.data$scoring_pts)) |>
    dplyr::mutate(scoring_avg = as.numeric(.data$scoring_avg)) |>
    dplyr::mutate(off_re = as.numeric(.data$off_re)) |>
    dplyr::mutate(def_re = as.numeric(.data$def_re)) |>
    dplyr::mutate(tot_re = as.numeric(.data$tot_re)) |>
    dplyr::mutate(avg_re = as.numeric(.data$avg_re)) |>
    dplyr::mutate(PF = as.numeric(.data$PF)) |>
    dplyr::mutate(AST = as.numeric(.data$AST)) |>
    dplyr::mutate(TO = as.numeric(.data$TO)) |>
    dplyr::mutate(STL = as.numeric(.data$STL)) |>
    dplyr::mutate(BLK = as.numeric(.data$BLK)) |>
    dplyr::filter(!(.data$Player) %in% c("team", "total", "opponents"))
  average_stats <- player_stats_tbl |>
   dplyr::summarize(dplyr::across(dplyr::where(is.numeric), mean, na.rm = TRUE))
  player_stats_tbl <- dplyr::bind_rows(player_stats_tbl, c(Player = "avg", average_stats))


  player_list <- player_stats_tbl |>
    purrr::pmap(list)
  names(player_list) <- sapply(player_list, function(x) x$Player)
  player_list <- lapply(player_list, function(x) {x["Player"] <- NULL; x})



  return (player_list)

}
