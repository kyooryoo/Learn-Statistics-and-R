library(tidyverse)
 
mpg

?mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

# use aesthetic of color, size, alpha or shape to add more info into plot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, stroke = cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

?geom_point

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

# use facets to subplot many subsets of the data
# comparing with aesthetic, facets work better with large dataset
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
# the variable used for facets should be discrete
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ cty, nrow = 2)