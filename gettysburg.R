library(tidyverse)
setwd("C:/Users/Cory/Desktop/nam")
df <- read_csv("gettysburg_oob.csv")
glimpse(df)

df %>% group_by(army, corps, division, brigade, type, state) %>%
  summarize(numUnits = n()) -> dfsummary

dfsummary
glimpse(dfsummary)

# write.csv(dfsummary, "gettyGroup.csv", row.names = F)

# table(df$division, df$state)
ggplot(df, aes(state)) +
  geom_bar()
