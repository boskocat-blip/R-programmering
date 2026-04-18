
# Paket

library(tidyverse)
library(janitor)


# Läs in data


data <- read.csv("data/insurance_costs.csv") %>%
  clean_names()

names(data)

# Dataförståelse


str(data)
summary(data)

# Saknade värden
colSums(is.na(data))

# Datastädning


data <- data %>%
  mutate(
    sex = as.factor(sex),
    region = as.factor(region),
    smoker = as.factor(smoker),
    chronic_condition = as.factor(chronic_condition),
    exercise_level = as.factor(exercise_level),
    plan_type = as.factor(plan_type)
  )


# Skapa nya variabler

data <- data %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"
    ),
    
    age_group = case_when(
      age < 30 ~ "Young",
      age < 50 ~ "Middle",
      TRUE ~ "Older"
    ),
    
    risk_score = prior_accidents + prior_claims
  )


# Deskriptiv analys


# Histogram (kostnader)
ggplot(data, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Fördelning av försäkringskostnader")

# Boxplot (rökning)
ggplot(data, aes(x = smoker, y = charges)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Kostnad vs rökning")

# Scatter BMI
ggplot(data, aes(x = bmi, y = charges)) +
  geom_point() +
  labs(title = "BMI vs kostnad")

# Scatter risk score
ggplot(data, aes(x = risk_score, y = charges)) +
  geom_point() +
  labs(title = "Risk score vs kostnad")


# Regression


# Modell 1 
model1 <- lm(charges ~ age + bmi + smoker + children + risk_score, data = data)
summary(model1)

# Modell 2 
model2 <- lm(charges ~ age + bmi + smoker + children + risk_score +
               exercise_level + chronic_condition + plan_type, data = data)

summary(model2)


#  Modelljämförelse


AIC(model1, model2)


#  Diagnostik


par(mfrow = c(2,2))
plot(model2)