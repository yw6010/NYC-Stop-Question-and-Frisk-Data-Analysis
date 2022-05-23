library(tidyverse)
library(ROCR)
set.seed(1234)
sqf_full = read_csv('C:/Users/wangy/WD/sqf_08_16.csv')

# Question B1: Predicting weapon recovery----------------------------------------------------------------------------------------------

# Initial Data cleaning 
sqf = sqf_full %>% filter(suspected.crime=='cpw') %>%
                   filter(precinct!=121) %>%
                   select(found.weapon, precinct, location.housing, 
                          
                          additional.associating, additional.direction,additional.report,
                          additional.evasive, additional.proximity, additional.highcrime, additional.investigation, additional.other,
                          additional.sights, additional.time, 
                          
                          stopped.bc.bulge, stopped.bc.casing, stopped.bc.clothing, stopped.bc.desc, stopped.bc.lookout, stopped.bc.drugs,
                          stopped.bc.furtive, stopped.bc.object, stopped.bc.other, stopped.bc.violent,
                          
                          suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight, inside, radio.run, time.period,
                          day, month, year, observation.period) %>%
                          # restrict to complete cases, so no row has any missing information
                          drop_na()

# Create a temp tibble in order to do the standardization for new observation
sqf_2008_temp = sqf %>% filter(year == 2008)

# Create the training subset and standardize real-valued attributes
sqf_2008 = sqf %>% filter(year==2008) %>%
                   mutate(precinct = as.factor(precinct),
                          suspect.age = replace(suspect.age, 1:n(), (suspect.age-mean(suspect.age))/sd(suspect.age)),
                          suspect.height = replace(suspect.height, 1:n(),(suspect.height-mean(suspect.height))/sd(suspect.height)),
                          suspect.weight = replace(suspect.weight, 1:n(),(suspect.weight-mean(suspect.weight))/sd(suspect.weight)),
                          observation.period = replace(observation.period, 1:n(),(observation.period-mean(observation.period))/sd(observation.period)),
                          time.period = as.factor(time.period),
                          suspect.sex = as.factor(suspect.sex),
                          suspect.build = as.factor(suspect.build),
                          location.housing = as.factor(location.housing),
                          day = as.factor(day),
                          month = as.factor(month)) %>%
                          select(-year)


# Fit a logistic regression where the outcome variable is whether or not a weapon is found and the predictor variables are everything else
fit_sqf_2008 = glm(found.weapon ~., data = sqf_2008, family = 'binomial')
summary(fit_sqf_2008)


# prediction for a new male  (to be easy, I use sqf_2008 to create the new observation)
new_observation_male = sqf_2008 %>% slice(1) %>%
                  mutate(precinct = as.factor(6),
                         suspect.age = 30,
                         suspect.height = 6,
                         suspect.weight = 165,
                         suspect.sex = 'male',
                         suspect.build = 'medium',
                         location.housing = 'transit',
                         month = 'October',
                         day = 'Saturday',
                         time.period = 6,
                         stopped.bc.bulge = TRUE,
                         additional.highcrime = TRUE,
                         observation.period = 10,
                         additional.evasive = FALSE,
                         stopped.bc.desc = FALSE,
                         stopped.bc.furtive = FALSE,
                         stopped.bc.other = FALSE,
                         inside = TRUE)

# Standardize the observation by subtract the mean and divide the sd from sqf_2008_temp
new_observation_male = new_observation_male %>%
  mutate(precinct = as.factor(6),
         suspect.age = (30-mean(sqf_2008_temp$suspect.age))/sd(sqf_2008_temp$suspect.age),
         suspect.height = (6-mean(sqf_2008_temp$suspect.height))/sd(sqf_2008_temp$suspect.height),
         suspect.weight = (165-mean(sqf_2008_temp$suspect.weight))/sd(sqf_2008_temp$suspect.weight),
         suspect.sex = as.factor('male'),
         suspect.build = as.factor('medium'),
         location.housing = as.factor('transit'),
         month = as.factor('October'),
         day = as.factor('Saturday'),
         time.period = as.factor(6),
         stopped.bc.bulge = TRUE,
         additional.highcrime = TRUE,
         observation.period = (10-mean(sqf_2008_temp$observation.period))/sd(sqf_2008_temp$observation.period),
         additional.evasive = FALSE,
         stopped.bc.desc = FALSE,
         stopped.bc.furtive = FALSE,
         stopped.bc.other = FALSE)


cat("The prediction for male is about ", predict(fit_sqf_2008, newdata = new_observation_male, type='response'))

# Prediction form Female 
new_observation_Female = sqf_2008 %>% slice(1) %>%
  mutate(precinct = as.factor(6),
         suspect.age = 30,
         suspect.height = 6,
         suspect.weight = 165,
         suspect.sex = 'female',
         suspect.build = 'medium',
         location.housing = 'transit',
         month = 'October',
         day = 'Saturday',
         time.period = 6,
         stopped.bc.bulge = TRUE,
         additional.highcrime = TRUE,
         observation.period = 10,
         additional.evasive = FALSE,
         stopped.bc.desc = FALSE,
         stopped.bc.furtive = FALSE,
         stopped.bc.other = FALSE,
         inside = TRUE)

# Standardize the observation 
new_observation_Female = new_observation_Female %>%
  mutate(precinct = as.factor(6),
         suspect.age = (30-mean(sqf_2008_temp$suspect.age))/sd(sqf_2008_temp$suspect.age),
         suspect.height = (6-mean(sqf_2008_temp$suspect.height))/sd(sqf_2008_temp$suspect.height),
         suspect.weight = (165-mean(sqf_2008_temp$suspect.weight))/sd(sqf_2008_temp$suspect.weight),
         suspect.sex = as.factor('female'),
         suspect.build = as.factor('medium'),
         location.housing = as.factor('transit'),
         month = as.factor('October'),
         day = as.factor('Saturday'),
         time.period = as.factor(6),
         stopped.bc.bulge = TRUE,
         additional.highcrime = TRUE,
         observation.period = (10-mean(sqf_2008_temp$observation.period))/sd(sqf_2008_temp$observation.period),
         additional.evasive = FALSE,
         stopped.bc.desc = FALSE,
         stopped.bc.furtive = FALSE,
         stopped.bc.other = FALSE)
  
cat("The prediction for Female is about ", predict(fit_sqf_2008, newdata = new_observation_Female, type='response'))


# Create the test set for 2009 and compute the prediction probability
sqf_2009 = sqf %>% filter(year==2009) %>%
  mutate(precinct = as.factor(precinct),
         suspect.age = replace(suspect.age, 1:n(), (suspect.age-mean(suspect.age))/sd(suspect.age)),
         suspect.height = replace(suspect.height, 1:n(),(suspect.height-mean(suspect.height))/sd(suspect.height)),
         suspect.weight = replace(suspect.weight, 1:n(),(suspect.weight-mean(suspect.weight))/sd(suspect.weight)),
         observation.period = replace(observation.period, 1:n(),(observation.period-mean(observation.period))/sd(observation.period)),
         time.period = as.factor(time.period),
         suspect.sex = as.factor(suspect.sex),
         suspect.build = as.factor(suspect.build),
         location.housing = as.factor(location.housing),
         day = as.factor(day),
         month = as.factor(month)) %>%
         select(-year)

sqf_2009 = sqf_2009 %>% mutate(predicted.probability = predict(fit_sqf_2008, newdata = sqf_2009, type = 'response'))


# Compute the AUC of this model on all data from 2009
test.pred <- prediction(sqf_2009$predicted.probability, sqf_2009$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")


# Extra credits: 
# Create a tibble contain  1,000 random true (weapon is found) with replacement 
weapon_found = tibble(
  sample_n(sqf_2009 %>% filter(sqf_2009$found.weapon==TRUE), size = 1000, replace = TRUE)
)

# Create another tibble contain 1000 random false(weapon is not found) with replacement
weapon_notfound = tibble(
  sample_n(sqf_2009 %>% filter(sqf_2009$found.weapon==FALSE), size = 1000, replace = TRUE)
)

# computing the proportion of pairs where your model predicts that the true example 
# is more likely to find a weapon than the false example
sum(weapon_found$predicted.probability>weapon_notfound$predicted.probability)/1000


# Question B2 Assessing model performance-------------------------------------------------------------------------------------------------------------------
# Choose to predict whether a suspect is arrested with several predictors 
sqf_new = sqf_full %>% select(arrested,precinct, searched,location.housing, suspect.age, suspect.build, suspect.sex, suspect.height, 
                              suspect.weight, inside, radio.run, time.period,day, month, year, observation.period) %>%
                       drop_na()

# build the training dataset (2010) and standardize data
train = sqf_new %>% filter(year==2010)  %>%
                    mutate(precinct = as.factor(precinct),
                           suspect.age = (suspect.age-mean(suspect.age))/sd(suspect.age),
                           suspect.build = as.factor(suspect.build),
                           suspect.sex = as.factor(suspect.sex),
                           suspect.height = (suspect.height-mean(suspect.height))/sd(suspect.height),
                           suspect.weight = (suspect.weight-mean(suspect.weight))/sd(suspect.height),
                           time.period = as.factor(time.period),
                           day = as.factor(day),
                           month = as.factor(month),
                           observation.period = (observation.period-mean(observation.period))/sd(observation.period)) %>%
                     select(-year)

# fit a logistic retrogression on arrested using all other variables
fit_sqf_2010 = glm(arrested ~., data = train, family = 'binomial')
summary(fit_sqf_2008_2010)


# build test dataset(2011) and standardize data 
test = sqf_new %>% filter(year==2011)  %>%
  mutate(precinct = as.factor(precinct),
         suspect.age = (suspect.age-mean(suspect.age))/sd(suspect.age),
         suspect.build = as.factor(suspect.build),
         suspect.sex = as.factor(suspect.sex),
         suspect.height = (suspect.height-mean(suspect.height))/sd(suspect.height),
         suspect.weight = (suspect.weight-mean(suspect.weight))/sd(suspect.height),
         time.period = as.factor(time.period),
         day = as.factor(day),
         month = as.factor(month),
         observation.period = (observation.period-mean(observation.period))/sd(observation.period)) %>%
  select(-year)


# make a prediction on test dataset using the model generated from 2010 add column called predicted.probability
test = test %>% mutate(predicted.probability = predict(fit_sqf_2010, newdata = test, type = 'response'))


# Generate a recall-at-k% plot
plot.data <- test %>% arrange( desc(predicted.probability) ) %>%
  mutate(numstops = row_number(), percent.outcome = cumsum(arrested )/sum( arrested ),
         stops = numstops/n()) %>% select(stops, percent.outcome)

theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome))
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1),
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of arrested recovered", limits=c(0, 1), labels=scales::percent)
p
# ggsave(plot=p, file='recall_at_k.png', height=5, width=5)


# Generate a calibration plot
plot.data <- test %>% mutate(calibration = round(100*predicted.probability)) %>%
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(arrested))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1),
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1),
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p
# ggsave(plot=p, file='calibration.png', height=5, width=5)
