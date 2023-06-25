pacman::p_load(tidyverse,
               gapminder,
               rio)

# Data Visualisation

data("gapminder")

plot1 <- gapminder %>% 
  ggplot(aes(lifeExp)) +
  geom_histogram()
  

# Data Wrangling

glimpse(starwars)
skimr::skim(starwars)

mysw <- starwars %>% 
  select(name, gender, height, mass)

mysw <- mysw %>% 
  mutate(bmi = mass/(height/100)^2)

mysw <- mysw %>% 
  mutate(bmi2 = bmi^2)

mysw_m <- mysw %>% 
  filter(gender == "masculine", bmi >= 40)
