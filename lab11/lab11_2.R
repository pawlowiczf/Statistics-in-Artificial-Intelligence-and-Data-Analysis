library(readr)

nba_df <- read_csv("nba-players-2022-3.csv", col_types = "icciciiiiidiidiiddiidiiiiiiiiic")
head(nba_df)

