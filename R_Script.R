library("readr")
library("ggplot2")
library("dplyr")
library("scales")
# loading the data
data <- read_csv("candy_crush.csv")

# view top 6 rows
head(data,6)

# COUNTING UNIQUE PLAYERS
summarise(data,n_distinct(player_id))
length(unique(data$player_id))

# COUNTING DATES
length(unique(data$dt))
summarise(data,start=min(dt),end=max(dt))
range(data$dt)

difficulty <- data %>%
  group_by(level) %>%
  summarise(attempts=sum(num_attempts),wins=sum(num_success)) %>%
  mutate(p_win=wins/attempts)

difficulty

ggplot(data=difficulty,aes(x=level,y=p_win)) + geom_line() + geom_point() + 
  scale_x_continuous(breaks=1:15)+ scale_y_continuous(labels=scales::percent)+
  labs(title="Plotting Difficulty Profile",subtitle = "Difficulty of levels in 2014",x="Levels",y="Percentage of wins")+
  geom_hline(yintercept=0.10,linetype="dashed") + geom_errorbar(aes(ymin=p_win - error,ymax=p_win+error))

difficulty <- difficulty %>%
  mutate(error=sqrt(p_win*(1-p_win)/attempts))
difficulty

prod(difficulty$p_win)




