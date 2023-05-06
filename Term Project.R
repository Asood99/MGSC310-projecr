
ufo <- read.csv("datasets/ufo_sighting_data.csv")
library(tidyverse)
ufo %>% glimpse

library(forcats)

ufo_clean <- ufo %>%
  mutate(unique_shapes = fct_lump(UFO_shape, 3)) %>%
  filter(length_of_encounter_seconds > 60) %>% 
  select(Date_time, unique_shapes, length_of_encounter_seconds, latitude, longitude)

ufo_clean %>% glimpse

ggplot(ufo_clean, aes(x = longitude, y = latitude, color = UFO_shape)) + geom_point()

View(ufo_clean)
