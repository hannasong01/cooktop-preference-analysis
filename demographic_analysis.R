# You should write code here to clean your pilot data and export it as a
# CSV file to the "data" folder



# Load libraries
#install.packages("fastDummies")
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)

# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

#work_dir <- "/cloud/project/data"
#setwd(work_dir)
here()

pathToData <- here('data', 'Cooktop_FS_P1.csv')
p1_full <- read_csv(pathToData)

pathToData <- here('data', 'Cooktop_FS_P2.csv')
p2_full <- read_csv(pathToData)

pathToData <- here('data', 'Cooktop_FS_P3.csv')
p3_full <- read_csv(pathToData)


# Format and join the three surveys -------

# Variables common to each survey: 
# session  = a unique ID for the Run - should be the same across all surveys
# created  = time stamp when survey was started
# modified = time stamp when survey was last modified by respondent
# ended    = time stamp when survey ended
# expired  = time stamp when survey expired (if respondent didn't reach end)

# Compute time values for each part
p1 <- p1_full %>% 
  mutate(
    created = ymd_hms(created, tz = "EST"),
    ended =  ymd_hms(ended, tz = "EST"),
    time_sec_p1 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p1, ownhome) 


p1 <- filter(p1, session != 'NA' )


p2 <- p2_full %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p2 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p2, respondentID, starts_with("cbc"), cbcAllSame)

p3 <- p3_full %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    age_in_years = 2023 - p3_full$yearOfBirth,
    time_sec_p3 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p3,induction, heatingeff,carbon, oftencook, utility, 
         yearOfBirth, gender, race, raceOther, education, income, zipcode,  age_in_years, feedback)

# Join all parts together using the session variable
data <- p1 %>% 
  left_join(p2, by = "session") %>% 
  left_join(p3, by = "session") %>% 
  # No longer need session variable
  select(-session)
head(data)




# Filter out bad responses ---------

nrow(data)

# Drop people who got screened out
data <- data %>% 
  filter(!is.na(screenout), screenout == 2)
nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>% 
  filter(!is.na(cbc1)) %>% 
  filter(!is.na(cbc2)) %>% 
  filter(!is.na(cbc3)) %>% 
  filter(!is.na(cbc4)) %>% 
  filter(!is.na(cbc5)) %>% 
  filter(!is.na(cbc6)) %>% 
  filter(!is.na(cbc7)) %>% 
  filter(!is.na(cbc8))
nrow(data)

# Drop respondents who went too fast
data <- data %>% 
  mutate(
    # First replace NA values with 0 seconds
    time_sec_p1 = ifelse(is.na(time_sec_p1), 0, time_sec_p1),
    time_sec_p2 = ifelse(is.na(time_sec_p2), 0, time_sec_p2),
    time_sec_p3 = ifelse(is.na(time_sec_p3), 0, time_sec_p3),
    # Now compute the total time
    time_min_total = (time_sec_p1 + time_sec_p2 + time_sec_p3) / 60
  )
# Look at summary of completion times
summary(data$time_min_total)
# Drop anyone who finished in under the 10th percentile of completion times
time_10 <- quantile(data$time_min_total, 0.1)
nrow(data)
data <- data %>% 
  filter(time_min_total >= time_10)
nrow(data)

induction_data <- data %>%
    filter(induction == "not_aware")
nrow(induction_data)

heating_data <- data %>%
    filter(heatingeff == "Not_Bothered")
nrow(heating_data)

emmission_data <- data %>%
    filter(carbon == "Not_Bothered")
nrow(emmission_data)

cooking_data <- data %>%
    filter(oftencook == "never")
nrow(cooking_data)

distinct_utility <- unique(data$utility)
distinct_utility

utility_data <- data %>%
    filter(utility == "electricity")
nrow(utility_data)

distinct_age <- unique(data$yearOfBirth)
distinct_age

induction_cookware <- c("Yes", "No", "Not_Aware")
number_of_people <- c(64, 99, 26)
df_induction <- data.frame(Induction = induction_cookware, Count = number_of_people)
print(df_induction)

heating_eff <- c("Concerned", "Neutral", "Not_Bothered")
number_of_people <- c(50, 60, 79)
df_heating <- data.frame(Heating = heating_eff, Count = number_of_people)
print(df_heating)

emissions <- c("Concerned", "Neutral", "Not_Bothered")
number_of_people <- c(50, 62, 77)

df_emissions <- data.frame(Emissions = emissions, Count = number_of_people)
print(df_emissions)

cooking_data <- c("often", "sometimes", "never")
number_of_people <- c(126, 62, 1)
df_Cooking_data <- data.frame(Cooking = cooking_data, Count = number_of_people)
print(df_Cooking_data)

utilities_info <- c("gas", "gas and electric", "electric")
number_of_people <- c(19, 90, 80)
df_Utilities <- data.frame(Utilities = utilities_info, Count = number_of_people)
print(df_Utilities)

distinct_Gender <- unique(data$gender)
distinct_Gender

Gender_data <- data %>%
    filter(gender == "prefer_not_say")
nrow(Gender_data)

gender_info <- c("Female", "Male", "Trans_Male", "gender-non_confirm", "prefer_not_say")
number_of_people <- c(92, 91, 2, 2, 2)
df_Gender <- data.frame(Gender = gender_info, Count = number_of_people)
print(df_Gender)

distinct_Race <- unique(data$race)
distinct_Race

Race_data <- data %>%
    filter(race == "white, pacific")
nrow(Race_data)

race_info <- c("Asian", "White, hispanic", "Black", "White", "Hispanic", "Black,Hispanic", "Asian, White", "Native", "Prefer not to say", "White, Native", "Asian, Black", "White, Pacific")
number_of_people <- c(13, 3, 15, 136, 11, 2, 1, 1, 2, 3, 1, 1)
df_Race <- data.frame(Race = race_info, Count = number_of_people)
print(df_Race)

write.csv(df_induction, "Induction.csv", row.names = FALSE)
write.csv(df_heating, "Heating.csv", row.names = FALSE)
write.csv(df_emissions, "Emissions.csv", row.names = FALSE)
write.csv(df_Cooking_data, "Cooking_frequency.csv", row.names = FALSE)
write.csv(df_Utilities, "Utilities.csv", row.names = FALSE)
write.csv(df_Gender, "Gender.csv", row.names = FALSE)
write.csv(df_Race, "Race.csv", row.names = FALSE)

distinct_Education <- unique(data$education)
distinct_Education

Education_data <- data %>%
    filter(education == "degree_bs")
nrow(data)

distinct_data <- unique(data$respID)
new_data = distinct_data
new_data

education_info <- c("bs", "vocational", "college_some", "HighSchool", "grad", "associate", "N/A")
number_of_people <- c(84, 6, 24, 17, 33, 24, 1)
df_Education <- data.frame(Education = education_info, Count = number_of_people)
edu_dis <- ggplot(df_Education, aes(x=reorder(Education, -Count), y=number_of_people)) +
  geom_bar(stat='identity')+
  geom_col(fill = "grey") +
  labs(x = 'Edu Level', y = 'Count')


ggsave(
  filename = here('figs', 'edu_dis3.png'), 
  width = 8, height = 7
)

distinct_Income <- unique(data$income)
distinct_Income

Income_data <- data %>%
    filter(income == "prefer_not_say")
nrow(Income_data)

income_info <- c("<10", "20-50", "50-100", "<100", "10-20", "N/A")
number_of_people <- c(5, 46, 80, 50, 7, 1)
df_Income <- data.frame(Income = income_info, Count = number_of_people)
ggplot(df_Income, aes(x=reorder(Income, -Count), y=number_of_people)) +
  geom_bar(stat='identity')+
  geom_col(fill = "grey") +
  labs(x = 'Income Level', y = 'Count')

ggsave(
  filename = here('figs', 'inc_dis.png'), 
  width = 8, height = 7
)

print(df_Income)

write.csv(df_Education, "Education1.csv", row.names = FALSE)
write.csv(df_Income, "Income.csv", row.names = FALSE)


distinct_AGE <- unique(data$age_in_years)
distinct_AGE

distinct_Zip<- unique(data$zipcode)
distinct_Zip

# Commenting this code as we do remove people for selecting higher priced Induction or Gas cooktop.
# Drop respondents that got the attention check question wrong
# data_3 <- data %>%
#   filter(cbcPractice == 3)
# nrow(data_3)



# Create choice data ---------

# First convert the data to long format
choiceData <- data %>% 
  pivot_longer(
    cols = cbc1:cbc8,
    names_to = "qID",
    values_to = "choice") %>% 
  # Convert the qID variable to a number
  mutate(qID = parse_number(qID))

head(choiceData)

# Read in choice questions and join it to the choiceData
survey <- read_csv("https://raw.githubusercontent.com/tejasphirke/Cooktops/main/Updated%20Cost%20and%20Price/choice_questions_updated_cost_price.csv")
choiceData <- choiceData %>% 
  rename(respID = respondentID) %>% 
  left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData <- choiceData %>% 
  mutate(choice = ifelse(choice == altID, 1, 0)) %>% 
  # Drop unused variables
  select(-image, -cbcPractice, -cbcAllSame)

head(choiceData)

# Create new values for respID & obsID
nRespondents <- nrow(data)
nAlts <- max(survey$altID)
nQuestions <- max(survey$qID)
choiceData$respID <- rep(seq(nRespondents), each = nAlts*nQuestions)
choiceData$obsID <- rep(seq(nRespondents*nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choiceData <- choiceData %>% 
  select(ends_with("ID"), "choice", everything())

head(choiceData)

# Save cleaned data for modeling
write_csv(choiceData, here("data", "choiceData.csv"))


