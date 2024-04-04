library(tidyverse)
library(rvest)

h <- read_html("https://utahstateaggies.com/sports/mens-basketball/stats/2023-24")

nodes_1 <- h |>
  html_elements(c("#individual-overall td"))

nodes_2 <- h |>
  html_elements(c("#individual-overall th"))

player_stats <- html_text2(nodes_1)
player_stats <- sapply(nodes_1, html_text2)
stat_names <- html_text2(nodes_2)

player_stats_split <- split(player_stats, rep(1:27, times = 16))
player_stats_split[[3]]

player_stats_df <- data.frame(numeric(16))
for (i in seq_len(length(player_stats_split))) {
  player_stats_df <- cbind(player_stats_df, player_stats_split[[i]])
}

stat_names_rev <- c("zero", stat_names[1:4], "tot_min", "tot_avg", stat_names[19:27], 
                    "scoring_pts", "scoring_avg", "off_re", "def_re", "tot_re", 
                    "avg_re", stat_names[11:15], "Bio")
length(stat_names_rev)
names(player_stats_df) <- stat_names_rev

player_stats_tbl <- as_tibble(player_stats_df) |>
  select(-zero) |>
  select(-Bio)
player_stats_tbl

