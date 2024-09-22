t <- ggplot(mpg, aes(cty, hwy)) + geom_point()
t
t + facet_grid(cols = vars(fl))
auto <- mpg



library(tidyverse)
library(patchwork)


p1 <- 
  mtcars %>% 
  ggplot(aes(x = factor(cyl), 
             y = mpg, 
             fill = factor(cyl)))+
  geom_boxplot()

p2 <- 
  mtcars %>% 
  ggplot(aes(x = mpg))+
  geom_histogram()

p3 <- 
  mtcars %>% 
  ggplot(aes(x = wt, y= mpg))+
  geom_point()+
  geom_smooth(method = "lm")

p1 | p2 / p3


layout(matrix(1:4, nrow=2))
p1
p2
p3
