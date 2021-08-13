library(nbastatR)
library(tidyverse)
library(sqldf)
library(ggplot2)

NBAData <- bref_players_stats(tables = c("per_game", "totals"), seasons = 2010:2021)

draft_data <- drafts(2010:2020)

data <- sqldf("SELECT ptsTotals, numberRoundPick, draft_data.yearDraft, NBAData.namePlayer, yearSeason, countGames FROM NBAData
              LEFT JOIN draft_data ON NBAData.namePlayer = draft_data.namePlayer")

df <- 
  data %>%
  filter(!is.na(yearDraft), numberRoundPick<=30) %>%
  group_by(numberRoundPick) %>%
  summarise(a_sum = sum(ptsTotals), b_sum = sum(countGames)) %>%
  mutate(avg_ppg = a_sum / b_sum) %>%
  select(numberRoundPick, avg_ppg, a_sum) %>%
  ggplot(aes(x = numberRoundPick, y = avg_ppg)) +
  geom_bar(stat= "identity", aes(fill = avg_ppg)) +
  scale_fill_gradient2(low = "#f7b733", 
                       high = "#fc4a1a")+
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )+
  ylab("Points Per Game") + xlab("Draft Pick Number") +
  labs(
    title = "Points Per Game for Each Draft Pick",
    subtitle = "2010-2021 Regular Seasons",
    caption = "Graph: @egecinar3"
  )+
  ylim(0, 20) +
  geom_text(aes(label = a_sum),  position = position_stack(vjust = 0.5),  color = "white", size = 2.8, srt = +90) 
df


