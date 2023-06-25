pacman::p_load(
  rio,          # file import/export
  here,         # relative filepaths 
  lubridate,    # working with dates/epiweeks
  aweek,        # alternative package for working with dates/epiweeks
  incidence2,   # epicurves of linelist data
  i2extras,     # supplement to incidence2
  stringr,      # search and manipulate character strings
  forcats,      # working with factors
  RColorBrewer, # Color palettes from colorbrewer2.org
  tidyverse     # data management + ggplot2 graphics
) 

linelist <- import(here("data", "raw", "linelist_cleaned.xlsx"))
glimpse(linelist)
skimr::skim(linelist)
names(linelist)

count_data <- linelist %>% 
  group_by(hospital, date_hospitalisation) %>% 
  summarize(n_cases = n()) %>% 
  filter(date_hospitalisation > as.Date("2013-06-01")) %>% 
  ungroup()

count_data
data_date <- as.Date("2015-05-15")

# create the incidence object, aggregating cases by day
epi_day <- incidence(     # create incidence object
  x = linelist,             # dataset
  date_index = "date_onset",  # date column
  interval = "day"          # date grouping interval
)

class(epi_day)
plot(epi_day)

epi_week <- incidence(     # create incidence object
  x = linelist,             # dataset
  date_index = "date_onset",  # date column
  interval = "week"          # date grouping interval
)
plot(epi_week)

# Statistical Modelling

data0 <- haven::read_dta(here("data", "raw", "datamssm_a.dta"))
glimpse(data0)
pacman::p_load(broom,
               haven)

data0 <- data0 %>%
  mutate_if(is.labelled, ~as_factor(.))

modldl.gender <- lm(hba1c ~ ldl + gender, data = data0)
confint(modldl.gender)
summary(modldl.gender)

modldl.gender.ia <- lm(hba1c ~ ldl + gender + ldl*gender, data = data0)
confint(modldl.gender.ia)
summary(modldl.gender.ia)

res1 <- tidy(modldl.gender, conf.int = TRUE)
res1

augment(modldl.gender.ia) %>% tail()
plot(modldl.gender.ia)

# Binary Outcome

pup2 <- import(here("data", "raw" ,"PUP2.xlsx"))
glimpse(pup2)

pup2 <- pup2 %>% 
  mutate(oc2 = factor(outcome))

plot2 <- pup2 %>% 
  ggplot(aes(x = oc2)) + 
  geom_bar() + 
  xlab('outcome') + ylab('freq')
plot2

model_multivar <- glm(oc2 ~ age + gender + epigastric_pain + 
                        onset_more_24_hrs + diabetes + NSAIDS + 
                        factor(ASA) + PULP + perforation + hemoglobin, 
                      family = binomial(link = 'logit'), 
                      data = pup2)

model_ia <- glm(oc2 ~ age + gender + epigastric_pain + 
                  onset_more_24_hrs + diabetes + NSAIDS + 
                  factor(ASA) + PULP + perforation + 
                  hemoglobin + perforation:PULP, 
                family = binomial(link = 'logit'), data = pup2)

fitted_logodds <- augment(model_multivar, type.predict = 'link')
fitted_prob <- augment(model_multivar, type.predict = 'response')
