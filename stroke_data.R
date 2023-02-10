#Setting up environment
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(skimr)
library(stringr)

#importing File 
stroke_data <- read_csv("pid_project/healthcare_dataset-stroke_data.csv")
skim_without_charts(stroke_data)
View(stroke_data)
# PREPARE AND PROCESSING PHASE:

# Data formatting to numeric
stroke_data %>% 
  mutate(bmi= as.numeric(bmi)) -> stroke_data

#Removal of NA values in bmi

stroke_data %>% 
  subset(!is.na(bmi)) ->stroke_data

skim_without_charts(stroke_data)

stroke_data <-read_csv("pid_project/stroke_data.csv")
View(stroke_data)
skim_without_charts(stroke_data)

#Replacing children with unemployed
stroke_data %>% 
  mutate(work_type = str_replace(work_type,"children", "unemployed")) -> stroke_data

# Classify Heart Disease variable  into "yes" or "No"
stroke_data %>% 
  mutate(heart_disease_state = case_when(
    heart_disease == 0 ~ "no",
    TRUE ~ "true"
  ), .before = ever_married) -> stroke_data

#classifying avg_glucose-level to ( normal, diabetes,hypoglycemia)
stroke_data %>% 
  mutate(avg_glucose_level_class = case_when(
    avg_glucose_level >= 70 & avg_glucose_level < 100 ~ "normal",
    avg_glucose_level >= 100 & avg_glucose_level <= 125 ~ "prediabetes",
    avg_glucose_level >= 126 ~ "diabetes",
    TRUE ~ "hypoglycemia"
  )) -> stroke_data

# Classify BMI variable into 
stroke_data %>% 
  mutate(hypertention_detail = case_when(
    hypertension == 0 ~ "no",
    TRUE ~ "yes"
  )) -> stroke_data
## classifying bmi into underweight, normal,overweight,obesity
stroke_data %>% 
  mutate(bmi_class = case_when(
    bmi < 18.5 ~ " under weight",
    bmi >= 18.5 & bmi < 25 ~ "normal weight",
    bmi >=25 & bmi < 30 ~ "over weight",
    TRUE ~ "obese"
  )) -> stroke_data

# DATA EXPLORATION; 
 # 1.Total number of male and female in the sample?
stroke_data %>% 
  group_by(gender) %>% 
  count(gender, name = "population") %>% 
  mutate(percent_gender = population*100/4909)
  

stroke_data %>% 
  group_by(gender) %>% 
  subset(!(gender=="Other")) %>% 
  count(gender, name = "population") %>% 
  ggplot(aes(x= gender, y=population ,fill= gender)) + geom_col() +
  labs(title = "Total number of male and female in the sample") + 
  scale_fill_manual(values = c("navy blue", "orange")) +
  theme_light()

#2.What % of each gender has Hypertension?
stroke_data %>% 
  group_by(gender) %>% 
  subset(!(gender=="Other")) %>% 
  summarise(cases = sum(hypertension), 
            percent= cases*100/451) %>% 
  ggplot(aes(x= gender,y=cases, fill = gender)) + 
  geom_bar(stat = "identity", alpha = 0.9) +
  labs(title = "What % of each gender has Hypertension",
       y= "Hypertension") + 
  scale_fill_manual(values = c("navy blue", "orange")) + theme_light()


#3.What % of each gender has Heart disease.
stroke_data %>% 
  group_by(gender) %>% 
  subset(!(gender=="Other")) %>%
  summarise(cases = sum(heart_disease), percent = cases*100/243) %>% 
  ggplot(aes(x= gender,y= cases, fill=gender)) +
  geom_col() +
  labs(title = "Gender vs Heart Disease",y = "Heart Disease") +
  scale_fill_manual(values = c("navy blue", "orange")) + theme_light()

# 4. What is the relationship between hypertension and heart disease.
#Limitations; Due to the data type ( Binary Data), d relationship can't be established by scatter plot.

# 5. How is the heart disease distributed within age?

stroke_data %>% 
  subset(!(gender=="Other")) %>% 
  ggplot(aes(x=age, fill= gender)) + geom_histogram(bins = 80) +
  labs(title = "Age Distribution", x = "Age", y= "Population") +
  scale_fill_manual(values = c("navy blue", "orange")) + theme_classic()
  
# 6. What % of ever_married status has stroke within each gender?
stroke_data %>% 
  group_by(ever_married, gender) %>%
  subset(!(gender=="Other")) %>% 
  summarise(cases = sum(stroke), percent= cases*100/209) %>% 
  ggplot(aes(x= ever_married, y= cases, fill= gender)) + geom_col(position = "dodge") +
  labs(title = "Marital Status Vs Stroke Cases In Each gender",
       y= "Stroke Cases", x= "Marital Status") +
  scale_fill_manual(name = "Gender", values = c("navy blue", "orange")) + theme_light()

# 7. How does the work_type  influences stroke (in each gender.)
stroke_data %>% 
  group_by(work_type, gender) %>% 
  subset(!(gender=="Other") & !(work_type =="Never_worked")) %>% 
  summarise(cases= sum(stroke), percent= cases*100/209) %>% 
  arrange(-cases, .by_group = FALSE) %>% 
  ggplot(aes(x=gender, y=cases, fill=gender)) +
  geom_col() +
  facet_wrap(~work_type) +
  labs(title = "Work Type Influence On Stroke", y= "Stroke cases") + 
  scale_fill_manual(name=" Gender", values = c("navy blue", "orange")) + theme_dark()

# 8. Which residence type has more occurrences of stroke and & why?
stroke_data %>% 
  group_by(Residence_type) %>% 
  summarise(cases = sum(stroke)) %>% 
  ggplot(aes(x= Residence_type, y= cases, fill= Residence_type)) + geom_col() +
  labs(title = "Residence Type vs Stroke", y= "Stroke Cases") + 
  theme_classic()

# 9. How does avg_glucose level class influence stroke?
stroke_data %>% 
  group_by(avg_glucose_level_class, gender) %>% 
  subset(!(gender=="Other")) %>% 
  summarise(cases = sum(stroke), percent= cases*100/209) %>% 
  ggplot(aes(x=avg_glucose_level_class, y=cases, fill= gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Effect Of Glucose Level On Stroke Cases", y = "Stroke Cases",
       x= " Average Glucose Level Class") + scale_fill_manual(values = c("navy blue","orange")) +
  theme_classic()


# 10.  Which BMI classification has the highest % of stroke occurrence and why?
stroke_data %>% 
  group_by(bmi_class,gender) %>% 
  subset(!(gender== "Other")) %>% 
  summarise(cases = sum(stroke),percent =cases*100/209) %>% 
  ggplot(aes(x= bmi_class, y= cases, fill= gender)) + geom_col(position = "dodge")+
  labs(title = "BMI Categories Vs Stroke Cases",
       y = "Stroke Cases", x= "Body Mass Index Categories") + 
  theme_classic() + scale_fill_manual(values = c("navy blue","orange","black"))

# 11. Relationship between smoking_status and stroke in each gender?
stroke_data %>% 
  group_by(smoking_status) %>%
  subset(!(gender=="Other")) %>% 
  summarise(cases= sum(stroke)) %>% 
  ggplot(aes(x= smoking_status , y= cases, fill= smoking_status)) + geom_col()

stroke_data %>% 
  group_by(smoking_status,gender) %>%
  subset(!(gender=="Other")) %>% 
  summarise(cases= sum(stroke), perecent = cases*100/209) %>% 
  ggplot(aes(x= gender, y= cases, fill= smoking_status)) + geom_col() +
  facet_wrap(~smoking_status) +
  labs(title = "Influence of Smoking Status on stroke Cases in Each Gender",
       y="Stroke Cases", x="Smoking Status") + theme_minimal()

# 12.What relationship exist  between Heart disease and stroke?

stroke_data %>% 
  group_by(heart_disease_state,gender) %>% 
  subset(!(gender== "Other")) %>% 
  summarise(stroke = sum(stroke)) %>% 
  ggplot(aes(x= heart_disease_state, y=stroke, fill = gender)) + geom_col( position = "dodge") +
  labs(title =  "Relationship between Heart Disease and Stroke Cases",
       y= " Stroke Cases", x="Heart Disease State") +  
  scale_fill_manual(values = c("navy blue", "orange")) + theme_linedraw()

# 13. What is the correlation between age and body mass index?
stroke_data %>% 
  select(gender,bmi, age) %>% 
  subset(!(gender=="Other")) %>% 
  ggplot(aes(x=age,y= bmi, color = gender)) + geom_point(alpha= 0.8) +
  geom_smooth(color = "black") +
  labs(title = "Correlation between Body Mass Index and Age",
       subtitle = "There is non- linear relationship between age and bmi",
       y= "Body Mass Index", x= "Age") + scale_color_manual(values = c("navy blue", "orange"))
  theme_update() 
  
  
