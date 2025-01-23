# week 2 scratch pad

library(tidyverse)

mydf <- read_csv("./data/oh_counties_DP2020.csv")
glimpse(mydf)

plot(mydf$poptotal, mydf$Hhtotal)


plot(mydf$poptotal, mydf$medianage)

hist(mydf$Hhtotal)
hist(mydf$Hhtotal, breaks = 20)


ggplot(mydf, aes(x = poptotal, y = medianage)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "glm", colour = "red") +
  theme_minimal() +
  labs(x = "Total Population", y = "Median Age", title = "My First ggplot")



mydf2 <- mydf %>% mutate(sizeCategory = ifelse(poptotal > 100000, "big", "small"))
summary(mydf2$sizeCategory)
summary(as.factor(mydf2$sizeCategory))


ggplot(mydf2, aes(x = poptotal, y = medianage)) +
  geom_point(aes(shape = sizeCategory, colour = sizeCategory), size = 3) +
  theme_minimal() +
  labs(x = "Total Population", y = "Median Age",
       title = "My formatted ggplot")


mydf2 %>% ggplot(., aes(x = sizeCategory, y = medianage)) +
  geom_boxplot(aes(fill = sizeCategory)) +
  theme_minimal() + 
  labs(x = "Categorical size",
       y = "Median Age", 
       title = "I made a boxplot",
       subtitle = "...it's handy for comparing groups")
