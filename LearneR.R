dput(mtcars)
mtcars

library(tidyverse)


#Visualization
mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty)) + 
  facet_wrap(~ class, ncol = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty)) + 
  facet_grid(drv ~ cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = hwy, color = "blue"))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv)) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(mapping = aes(linetype = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = TRUE)
#the local arg data = filter overrides golbal arg data = mpg

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
bar + coord_polar()


diamonds


?mpg
?ggplot
?geom_smooth
?geom_bar
?stat_identity
?aes
?labs





#Transformation
library(nycflights13)
library(tidyverse)

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
novdec <- filter(flights, month %in% c(11, 12)); novdec
flights
modified <- rename(flights, tail_num = tailnum); modified

by_day <- group_by(flights, month, day); by_day
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
#group_by makes it so each category is flattened, so it shows the mean delay for each day, since each group is a date
View(by_day)

not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
byTailnum <- group_by(not_cancelled, tailnum)
delaysByTailnum <- summarise(byTailnum, delay = mean(dep_delay, na.rm = TRUE), n = n())
ggplot(data = delaysByTailnum, mapping = aes(x = delay)) + geom_freqpoly(binwidth = 1)
ggplot(data = delaysByTailnum, mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
ggplot(data = filter(delaysByTailnum, n > 25), mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
max(delaysByTailnum$delay)

batdata <- Lahman::Batting
batters <- group_by(batdata, playerID); View(batters)
battingavg <- summarize(batters, battingavg = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), atBat = sum(AB, na.rm = TRUE)); battingavg
View(battingavg)
ggplot(data = filter(battingavg, atBat > 100) , mapping = aes(x = atBat, y = battingavg)) + geom_point() + geom_smooth(se = FALSE)

n_distinct(batters$playerID[batters$AB > 100])

?group_by
?n




#EDA

count(diamonds, cut)

ggplot(data = filter(diamonds, carat < 3), mapping = aes(x = carat, color = cut)) + geom_freqpoly(binwidth = 0.1)


diamonds2 <- mutate(diamonds, y = ifelse(y < 3 | y > 20, NA, y)); diamonds2

ggplot(data = diamonds, mapping = aes(x = x, y = y)) + geom_point()
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point(na.rm = TRUE)

diamonds

withSize <- mutate(mutate(diamonds2, rect_vol = x *  y * z), scale = rect_vol^(1/3)); withSize

ggplot(data = withSize, mapping = aes(x = scale, y = price, color = color)) + 
  geom_point(na.rm = TRUE)

ggplot(data = withSize, mapping = aes(x = scale, y = price, color = color)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(na.rm = TRUE) +
  facet_grid(cut ~ color)

ggplot(data = withSize, mapping = aes(x = rect_vol, y = price, color = color, alpha = 1/50)) + 
  geom_point(na.rm = TRUE) +
  facet_wrap(~ color, nrow = 1)

ggplot(data = withSize, mapping = aes(x = rect_vol, y = price)) + geom_point(na.rm = TRUE)
ggplot(data = withSize, mapping = aes(x = carat, y = price)) + geom_point(na.rm = TRUE)
ggplot(data = withSize, alpha = 1/10) + 
  geom_point(mapping = aes(x = x, y = price), na.rm = TRUE, color = "blue") +
  geom_point(mapping = aes(x = y, y = price), na.rm = TRUE, color = "red") + 
  geom_point(mapping = aes(x = z, y = price), na.rm = TRUE, color = "green")

ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))
ggplot(data = count(diamonds, color, cut), mapping = aes(x = color, y = cut)) + 
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot()

?reorder
?diamonds

?count
