# Shifting the Needle: Abortion Support and Policy Feedback Loops A Test of the Thermostatic Model of Public Opinion
# Replication Code
# Bradley Wood-MacLean

GSS2021 <- rio::import("2021/GSS2021.dta")
GSS2022 <- rio::import("2022/GSS2022.dta")

library(ggplot2)
library(dplyr)
library(patchwork)
library(vcd)
library(stringr)
library(tidyr)
library(ggeffects)
library(ggalluvial)
library(officer)
library(flextable)
library(broom)

###############################################################################
# GSS 2021 Abortion Variables
###############################################################################

# Variables ought to be coded so that higher numbers mean support for abortion (pro-choice)

# the questions coded with "-g" at the end signify they were presented in a grid together instead of separately 

# PROCHOIC prochoic (this needs to be reverse coded so the higher numbers are pro-choice)
# (We hear a lot of talk these days about abortion. Please indicate to what extent you agree or disagree with each of the following statements.) 
# I consider myself pro-choice.
  # STRONGLY AGREE 1 
  # AGREE 2 
  # NEITHER AGREE NOR DISAGREE 3 
  # DISAGREE 4 
  # STRONGLY DISAGREE 5 

# PROLIFE prolife (this does not need to be reverse coded because the higher numbers are already pro-choice)
# (We hear a lot of talk these days about abortion. Please indicate to what extent you agree or disagree with each of the following statements.) 
# I consider myself pro-life.
  # STRONGLY AGREE 1 
  # AGREE 2 
  # NEITHER AGREE NOR DISAGREE 3 
  # DISAGREE 4 
  # STRONGLY DISAGREE 5 

# ABANY abany / ABANYG abanyg
# If the woman wants it for any reason?
  # YES 1 
  # NO 2

# ABDEFECT abdefect / ABDEFECTG abdefectg
# (Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if. . .) 
# If there is a strong chance of serious defect in the baby?
  # YES 1 
  # NO 2

# ABHLTH abhlth / ABHLTHG abhlthg
# If the woman's own health is seriously endangered by the pregnancy?
  # YES 1 
  # NO 2

# ABNOMORE abnomore / ABNOMOREG abnomoreg
# If she is married and does not want any more children?
  # YES 1 
  # NO 2

# ABPOOR abpoor / ABPOORG abpoorg
# If the family has a very low income and cannot afford any more children?
  # YES 1 
  # NO 2

# ABRAPE abrape / ABRAPEG abrapeg
# If she becomes pregnant as a result of rape?
  # YES 1 
  # NO 2

# ABSINGLE absingle / ABSINGLEG absingleg
# If she is not married and does not want to marry the man?
  # YES 1 
  # NO 2

###############################################################################
# GSS 2022 Abortion Variables
###############################################################################

# the questions coded with "-g" at the end signify they were presented in a grid together instead of separately 

# GESTATE gestate
# Please tell us whether or not you think it should be possible for a pregnant woman to obtain a legal abortion 
# if... ...she is beyond 4.5 months, that is, about halfway through the pregnancy.
  # YES 1 
  # NO 2

# ABBELIEF abbelief (this needs to be reverse coded so higher numbers are pro-choice like the Y/N questions)
# Which of the following best represents you in terms of your belief about abortion? I am…
  #   STRONGLY PRO-CHOICE 1 
  #   SLIGHTLY PRO-CHOICE 2 
  #   BOTH PRO-CHOICE AND PRO-LIFE 3 
  #   NEITHER PRO-CHOICE NOR PRO-LIFE 4 
  #   SLIGHTLY PRO-LIFE 5 
  #   STRONGLY PRO-LIFE 6 

# ABDEFECT abdefect / ABDEFECTG abdefectg
# (Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if. . .) 
# If there is a strong chance of serious defect in the baby?
  # YES 1 
  # NO 2

# ABGENDER abgender
# (Please tell us whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if...) ...
# she is pregnant with a boy but wants a girl, or is pregnant with a girl but wants a boy.
  # YES 1 
  # NO 2

# ABHLTH abhlth / ABHLTHG abhlthg
# If the woman's own health is seriously endangered by the pregnancy?
  # YES 1 
  # NO 2

# ABNOMORE abnormore / ABNOMOREG abnomoreg
# If she is married and does not want any more children?
  # YES 1 
  # NO 2

# ABPOOR abpoor / ABPOORG abpoorg
# If the family has a very low income and cannot afford any more children?
  # YES 1 
  # NO 2

# ABRAPE abrape / ABRAPEG abrapeg
# If she becomes pregnant as a result of rape?
  # YES 1 
  # NO 2

# ABSINGLE absingle / ABSINGLEG absingleg
# If she is not married and does not want to marry the man?
  # YES 1 
  # NO 2

###############################################################################
# Cleaning
###############################################################################

GSS2021 <- GSS2021 %>%
  mutate(
    abany = ifelse(!is.na(abany), abany, abanyg),
    abdefect = ifelse(!is.na(abdefect), abdefect, abdefectg),
    abhlth = ifelse(!is.na(abhlth), abhlth, abhlthg),
    abnomore = ifelse(!is.na(abnomore), abnomore, abnomoreg),
    abpoor = ifelse(!is.na(abpoor), abpoor, abpoorg),
    abrape = ifelse(!is.na(abrape), abrape, abrapeg),
    absingle = ifelse(!is.na(absingle), absingle, absingleg)
  ) %>%
  select(-ends_with("g")) %>%
  mutate(
    # Reverse coding to align higher numbers with pro-choice support
    abany = ifelse(abany == 1, 1, ifelse(abany == 2, 0, NA)),
    abdefect = ifelse(abdefect == 1, 1, ifelse(abdefect == 2, 0, NA)),
    abhlth = ifelse(abhlth == 1, 1, ifelse(abhlth == 2, 0, NA)),
    abnomore = ifelse(abnomore == 1, 1, ifelse(abnomore == 2, 0, NA)),
    abpoor = ifelse(abpoor == 1, 1, ifelse(abpoor == 2, 0, NA)),
    abrape = ifelse(abrape == 1, 1, ifelse(abrape == 2, 0, NA)),
    absingle = ifelse(absingle == 1, 1, ifelse(absingle == 2, 0, NA))
  )

GSS2022 <- GSS2022 %>%
  mutate(
    abdefect = ifelse(!is.na(abdefect), abdefect, abdefectg),
    abhlth = ifelse(!is.na(abhlth), abhlth, abhlthg),
    abnomore = ifelse(!is.na(abnomore), abnomore, abnomoreg),
    abpoor = ifelse(!is.na(abpoor), abpoor, abpoorg),
    abrape = ifelse(!is.na(abrape), abrape, abrapeg),
    absingle = ifelse(!is.na(absingle), absingle, absingleg)
  ) %>%
  select(-ends_with("g")) %>%
  mutate(
    # Reverse coding to align higher numbers with pro-choice support
    abdefect = ifelse(abdefect == 1, 1, ifelse(abdefect == 2, 0, NA)),
    abhlth = ifelse(abhlth == 1, 1, ifelse(abhlth == 2, 0, NA)),
    abnomore = ifelse(abnomore == 1, 1, ifelse(abnomore == 2, 0, NA)),
    abpoor = ifelse(abpoor == 1, 1, ifelse(abpoor == 2, 0, NA)),
    abrape = ifelse(abrape == 1, 1, ifelse(abrape == 2, 0, NA)),
    absingle = ifelse(absingle == 1, 1, ifelse(absingle == 2, 0, NA))
  )


# for 2022 it need to be during or after July (Dobbs June 24th 2022, but leaked May)
unique(GSS2022$dateintv) # check for the format, it is month-date format

# Convert the date variable to a more usable format by separating month and day
GSS2022$month_day <- as.numeric(GSS2022$dateintv)

# Filter for observations with dates after July (month 07)
GSS2022_after_july <- subset(GSS2022, month_day >= 701) 

GSS2022 <- GSS2022_after_july

###############################################################################
# Graphs to inspect the distribution
###############################################################################

## 2021

# Histogram for prochoic
ggplot(GSS2021, aes(x = prochoic)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Distribution of prochoic", x = "prochoic", y = "Frequency") +
  theme_minimal()

# Histogram for prolife
ggplot(GSS2021, aes(x = prolife)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Distribution of prolife", x = "prolife", y = "Frequency") +
  theme_minimal()

# Histogram for abany
a <- ggplot(GSS2021, aes(x = abany)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Any reason", x = "abany", y = "Frequency") +
  theme_minimal()

# Histogram for abdefect
b <- ggplot(GSS2021, aes(x = abdefect)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Fetus Defect", x = "abdefect", y = "Frequency") +
  theme_minimal()

# Histogram for abhlth
c <- ggplot(GSS2021, aes(x = abhlth)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Woman's health risked", x = "abhlth", y = "Frequency") +
  theme_minimal()

# Histogram for abnomore
d <- ggplot(GSS2021, aes(x = abnomore)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "doesn't want kids", x = "abnomore", y = "Frequency") +
  theme_minimal()

# Histogram for abpoor
e <- ggplot(GSS2021, aes(x = abpoor)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "poor", x = "abpoor", y = "Frequency") +
  theme_minimal()

# Histogram for abrape
f <- ggplot(GSS2021, aes(x = abrape)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "raped", x = "abrape", y = "Frequency") +
  theme_minimal()

# Histogram for absingle
g <- ggplot(GSS2021, aes(x = absingle)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "single", x = "absingle", y = "Frequency") +
  theme_minimal()

h <- a + b + c + d + e + f + g
combined_plot21 <- h + plot_annotation(
  title = "2021: A woman should be able to get an abortion if...",
)

i <- mosaic(~ abrape + prolife, data = GSS2021,
            highlighting = "abrape",
            highlighting_fill = c("blue", "red"),
            main = "Pro-Life Stance x Rape Exception")
i

## 2022

# Histogram for abbelief
ggplot(GSS2022, aes(x = abbelief)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Pro-Choice and Pro-Life", x = "abbelief", y = "Frequency") +
  theme_minimal()

# Histogram for abany
a2 <- ggplot(GSS2022, aes(x = abany)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Any reason", x = "abany", y = "Frequency") +
  theme_minimal()
a2

# Histogram for abdefect
b2 <- ggplot(GSS2022, aes(x = abdefect)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Fetus Defect", x = "abdefect", y = "Frequency") +
  theme_minimal()
b2

# Histogram for abhlth
c2 <- ggplot(GSS2022, aes(x = abhlth)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Woman's health risked", x = "abhlth", y = "Frequency") +
  theme_minimal()
c2

# Histogram for abnomore
d2 <- ggplot(GSS2022, aes(x = abnomore)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "doesn't want kids", x = "abnomore", y = "Frequency") +
  theme_minimal()
d2

# Histogram for abpoor
e2 <- ggplot(GSS2022, aes(x = abpoor)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "poor", x = "abpoor", y = "Frequency") +
  theme_minimal()
e2

# Histogram for abrape
f2 <- ggplot(GSS2022, aes(x = abrape)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "raped", x = "abrape", y = "Frequency") +
  theme_minimal()
f2

# Histogram for absingle
g2 <- ggplot(GSS2022, aes(x = absingle)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "single", x = "absingle", y = "Frequency") +
  theme_minimal()
g2


# Historgram from abgender
h2 <- ggplot(GSS2022, aes(x = abgender)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "gender selective", x = "absingle", y = "Frequency") +
  theme_minimal()
h2

# Historgram from gestate
j2 <- ggplot(GSS2022, aes(x = gestate)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
  labs(title = "legal in state?", x = "absingle", y = "Frequency") +
  theme_minimal()
j2

i2 <- a2 + b2 + c2 + d2 + e2 + f2 + g2 +h2 + j2
combined_plot22 <- i2 + plot_annotation(
  title = "2022: A woman should be able to get an abortion if...",
)

combined_plot <- combined_plot21 / combined_plot22
combined_plot

k <- mosaic(~ abrape + gestate, data = GSS2022,
            highlighting = "abrape",
            highlighting_fill = c("blue", "red"),
            main = "Abortion Legal x Rape Exception")
k

l <- mosaic(~  prolife + prochoic, data = GSS2021,
            highlighting = "prolife",
            highlighting_fill = c("blue", "red"),
            main = "prolife x prochoice")
l

###############################################################################
# Analysis
###############################################################################

###### PART 1 Make Abortion Support Constructs Using Original Questions

# GSS2021 Abortion Support Construct
# Reverse code prochoic to align with pro-choice support, higher numbers should indicate higher abortion support
GSS2021$prochoic_rev <- 6 - GSS2021$prochoic  

# Standardize all variables
GSS2021 <- GSS2021 %>%
  mutate(
    prolife_std = scale(prolife),
    prochoic_rev_std = scale(prochoic_rev),
    abany_std = scale(abany),
    abdefect_std = scale(abdefect),
    abhlth_std = scale(abhlth),
    abnomore_std = scale(abnomore),
    abpoor_std = scale(abpoor),
    abrape_std = scale(abrape),
    absingle_std = scale(absingle)
  )

# Calculate composite score with 10 items
GSS2021$abortion_support <- rowSums(GSS2021[c("prolife_std", "prochoic_rev_std", 
                                              "abany_std", "abdefect_std", "abhlth_std", 
                                              "abnomore_std", "abpoor_std", 
                                              "abrape_std", "absingle_std")], na.rm = TRUE) / 10

# GSS2022 Abortion Support Construct
# Reverse code abbelief to align with pro-choice support
GSS2022$abbelief_rev <- 7 - GSS2022$abbelief  

# Standardize all variables, weighting abbelief double
GSS2022 <- GSS2022 %>%
  mutate(
    abbelief_std = scale(abbelief_rev),
    abdefect_std = scale(abdefect),
    abgender_std = scale(abgender),
    abhlth_std = scale(abhlth),
    abnomore_std = scale(abnomore),
    abpoor_std = scale(abpoor),
    abrape_std = scale(abrape),
    absingle_std = scale(absingle)
  )

# Calculate composite score with 10 effective items (abbelief double-weighted)
GSS2022$abortion_support <- rowSums(GSS2022[c("abbelief_std", # "abbelief_std",  # Double weight
                                              "abdefect_std", "abgender_std", 
                                              "abhlth_std", "abnomore_std", 
                                              "abpoor_std", "abrape_std", 
                                              "absingle_std")], na.rm = TRUE) / 10
hist(GSS2021$abortion_support)
hist(GSS2022$abortion_support)

# This code creates an "abortion_support" variable by combining multiple questions about abortion views. 
# 1. Reverse coding in 2021 aligns the pro-life and pro-choice variables so that higher values indicate stronger pro-choice support.
# 2. Standardization adjusts the scales of all variables so they contribute equally to the final score.
# 3. The composite score is calculated by averaging all the standardized values, creating a single score for each person.
# 4. In GSS2022, the "abbelief" variable is weighted double to ensure it has the same importance as the two separate pro-choice/pro-life questions in GSS2021.
# The resulting "abortion_support" variable can now be compared across years

###### PART 2 Prepare Control Variables

## Region 2021 region
  # NEW ENGLAND 1 
  # MIDDLE ATLANTIC 2 
  # EAST NORTH CENTRAL 3 
  # WEST NORTH CENTRAL 4 
  # SOUTH ATLANTIC 5 
  # EAST SOUTH ATLANTIC 6 
  # WEST SOUTH CENTRAL 7 
  # MOUNTAIN 8 
  # PACIFIC 9

table(GSS2021$region)
table(GSS2021$region)

# States with abortion bans as of 2022:
# Alabama
# Arkansas
# Idaho
# Kentucky
# Louisiana
# Mississippi
# Missouri
# Oklahoma
# South Dakota
# Tennessee
# Texas
# West Virginia
# Wisconsin

# Load state population data

state_pops <- rio::import("/Users/bradleywood-maclean/Desktop/POLI 514/Paper/Data/POLI514/Final Populations.xlsx")

# clean data
state_pops <- state_pops %>%
  select(-`2023`, -`2020`) %>% 
  filter(!State %in% c("United States", "Northeast", "Midwest", "South", "West","Puerto Rico")) %>% 
  mutate(State = str_replace(State, "^\\.", "")) %>% 
  drop_na()

# assign GSS regions 

state_pops <- state_pops %>%
  mutate(
    region = case_when(
      State %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island") ~ 1,
      State %in% c("New York", "New Jersey", "Pennsylvania") ~ 2,
      State %in% c("Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio") ~ 3,
      State %in% c("Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas") ~ 4,
      State %in% c("Delaware", "Maryland", "West Virginia", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia") ~ 5,
      State %in% c("Kentucky", "Tennessee", "Alabama", "Mississippi") ~ 6,
      State %in% c("Arkansas", "Oklahoma", "Louisiana", "Texas") ~ 7,
      State %in% c("Montana", "Idaho", "Wyoming", "Nevada", "Utah", "Colorado", "Arizona", "New Mexico") ~ 8,
      State %in% c("Washington", "Oregon", "California", "Alaska", "Hawaii") ~ 9,
      TRUE ~ NA_real_  # Assign NA to any states not covered
    )
  )

banned_states <- c("Alabama", "Arkansas", "Idaho", "Kentucky", "Louisiana", 
                   "Mississippi", "Missouri", "Oklahoma", "South Dakota", 
                   "Tennessee", "Texas", "West Virginia", "Wisconsin")

state_pops <- state_pops %>%
  mutate(abortion_ban = ifelse(State %in% banned_states, 1, 0))

# make abortion ban dummy, 1 for ban, 0 for no ban

abortion_ban_population <- state_pops %>%
  group_by(region) %>%
  summarise(
    population_under_abortion_ban_2022 = sum(`2022`[abortion_ban == 1], na.rm = TRUE),
    population_not_under_abortion_ban_2022 = sum(`2022`[abortion_ban == 0], na.rm = TRUE),
    total_population_2022 = sum(`2022`, na.rm = TRUE)
  ) %>%
  mutate(
    percent_population_under_abortion_ban = (population_under_abortion_ban_2022 / total_population_2022) * 100
  )

# Calculate total population under an abortion ban by region in 2022
abortion_ban_population <- state_pops %>%
  group_by(region) %>%
  summarise(
    population_under_abortion_ban_2022 = sum(`2022`[abortion_ban == 1], na.rm = TRUE),
    population_not_under_abortion_ban_2022 = sum(`2022`[abortion_ban == 0], na.rm = TRUE)
  )

# Treatment (has ban) group is regions 6, 7
# control (no ban) group is regions 1, 2, 5, 8, 9
# excluding groups 3, 4

# add this information to the original state_pops as variable

state_pops <- state_pops %>%
  mutate(
    group = case_when(
      region %in% c(6, 7) ~ "Treatment",
      region %in% c(1, 2, 5, 8, 9) ~ "Control",
      region %in% c(3, 4) ~ NA_character_
    )
  )

# make separate treatment/control dataset for merging with survey data

region_groups <- data.frame(
  region = c(1, 2, 5, 6, 7, 8, 9),
  group = c("Control", "Control", "Control", "Treatment", "Treatment", "Control", "Control")
)

# Add group information to GSS2021
GSS2021 <- GSS2021 %>%
  left_join(region_groups, by = "region")

# Add group information to GSS2022
GSS2022 <- GSS2022 %>%
  left_join(region_groups, by = "region")


####### Other control variables

# GSS2021 Religion RELIGIMP religimp
# How important is religion in your life – very important, somewhat important, not too important, or not at all important?
  # VERY IMPORTANT 1 
  # SOMEWHAT IMPORTANT 2 
  # NOT TOO IMPORTANT 3 
  # NOT AT ALL IMPORTANT 4

# Reverse code religimp so that higher values indicate higher religiosity
GSS2021$religimp_rev <- 5 - GSS2021$religimp

# Rename the recoded religiosity variable in GSS2021
GSS2021$religion <- GSS2021$religimp_rev

# GSS2022 Religion RELPERSN relpersn
  # VERY RELIGIOUS 1 
  # MODERATELY RELIGIOUS 2 
  # SLIGHTLY RELIGIOUS 3 
  # NOT RELIGIOUS AT ALL 4 

# Reverse code relpersn so that higher values indicate higher religiosity
GSS2022$relpersn_rev <- 5 - GSS2022$relpersn

# Rename the recoded religiosity variable in GSS2022
GSS2022$religion <- GSS2022$relpersn_rev

# GSS2021 and GSS2022 Gender SEXNOW1 sexnow1
# Do you describe yourself as male, female, or transgender?
  # MALE 1 
  # FEMALE 2 
  # TRANSGENDER 3 
  # NONE OF THESE 4

# Convert gender variable to a factor in GSS2021 and 2022
GSS2021$gender <- factor(GSS2021$sexnow1,
                         levels = c(1, 2, 3, 4),
                         labels = c("Male", "Female", "Transgender", "None of These"))

GSS2022$gender <- factor(GSS2022$sexnow1,
                         levels = c(1, 2, 3, 4),
                         labels = c("Male", "Female", "Transgender", "None of These"))


# GSS2021 and GSS2022 Party PARTYID
# Generally speaking, do you usually think of yourself as a Republican, Democrat, Independent, or what?
  # STRONG DEMOCRAT 0 
  # NOT VERY STRONG DEMOCRAT 1
  # INDEPENDENT, CLOSE TO DEMOCRAT 2 
  # INDEPENDENT (NEITHER, NO RESPONSE) 3 
  # INDEPENDENT, CLOSE TO REPUBLICAN 4 
  # NOT VERY STRONG REPUBLICAN 5 
  # STRONG REPUBLICAN 6 
  # OTHER PARTY 7

# Recode PARTYID into three categories
GSS2021$party3 <- factor(case_when(
  GSS2021$partyid %in% 0:2 ~ "Democrat",
  GSS2021$partyid == 3 ~ "Independent",
  GSS2021$partyid %in% 4:6 ~ "Republican",
  GSS2021$partyid == 7 ~ NA_character_  # Treat 'Other Party' as NA
),
levels = c("Democrat", "Independent", "Republican"))

GSS2022$party3 <- factor(case_when(
  GSS2022$partyid %in% 0:2 ~ "Democrat",
  GSS2022$partyid == 3 ~ "Independent",
  GSS2022$partyid %in% 4:6 ~ "Republican",
  GSS2022$partyid == 7 ~ NA_character_  # Treat 'Other Party' as NA
),
levels = c("Democrat", "Independent", "Republican"))

# GSS2021 and GSS2022 Education EDUC educ
  # NO FORMAL SCHOOLING 0 
  # 1ST GRADE 1 
  # 2ND GRADE 2 
  # 3RD GRADE 3 
  # 4TH GRADE 4 
  # 5TH GRADE 5 
  # 6TH GRADE 6
  # 7TH GRADE 7 
  # 8TH GRADE 8 
  # 9TH GRADE 9  
  # 10TH GRADE 10 
  # 11TH GRADE 11 
  # 12TH GRADE 12 
  # 1 YEAR OF COLLEGE 13
  # 2 YEARS OF COLLEGE 14 
  # 3 YEARS OF COLLEGE 15 
  # 4 YEARS OF COLLEGE 16 
  # 5 YEARS OF COLLEGE 17 
  # 6 YEARS OF COLLEGE 18 
  # 7 YEARS OF COLLEGE 19 
  # 8 YEARS OF COLLEGE 20

GSS2021$education <- factor(case_when(
  GSS2021$educ %in% 0:11 ~ "Less than High School",  # 0–11
  GSS2021$educ == 12 ~ "High School Graduate",       # 12
  GSS2021$educ %in% 13:15 ~ "Some College",          # 13–15
  GSS2021$educ >= 16 ~ "College Graduate or Higher"  # 16–20
),
levels = c("Less than High School", "High School Graduate", "Some College", "College Graduate or Higher"))

GSS2022$education <- factor(case_when(
  GSS2022$educ %in% 0:11 ~ "Less than High School",  # 0–11
  GSS2022$educ == 12 ~ "High School Graduate",       # 12
  GSS2022$educ %in% 13:15 ~ "Some College",          # 13–15
  GSS2022$educ >= 16 ~ "College Graduate or Higher"  # 16–20
),
levels = c("Less than High School", "High School Graduate", "Some College", "College Graduate or Higher"))

########### Part 3: Make Dataset with only required variables

GSS2021_filtered <- GSS2021 %>%
  select(education, gender, religion, party3, abortion_support, group) %>%
  drop_na()  # Remove rows with missing values

GSS2022_filtered <- GSS2022 %>%
  select(education, gender, religion, party3, abortion_support, group) %>%
  drop_na()  # Remove rows with missing values

# Add year variable to each dataset
GSS2021_filtered <- GSS2021_filtered %>%
  mutate(year = 2021)

GSS2022_filtered <- GSS2022_filtered %>%
  mutate(year = 2022)

# Combine datasets
combined_data <- bind_rows(GSS2021_filtered, GSS2022_filtered)

######## Part 4: Do the Difference-in-Difference Analysis

# Add PostBan variable
combined_data <- combined_data %>%
  mutate(PostBan = ifelse(year == 2022, 1, 0),
         TreatmentGroup = ifelse(group == "Treatment", 1, 0))

# Run the regression model
did_model <- lm(abortion_support ~ PostBan * TreatmentGroup + education + gender + religion + party3, 
                data = combined_data)

# Summarize the results
summary(did_model)

# Create a tidy data frame of the model summary
did_model_summary <- tidy(did_model) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))  # Use "<0.001" for very small p-values
  )

# Convert the summary into a flextable
ft <- flextable(did_model_summary)

# Adjust the appearance of the flextable
ft <- ft %>%
  set_header_labels(
    term = "Variable",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  ) %>%
  theme_vanilla() %>%
  autofit()  # Automatically adjust column widths

# Create a Word document
doc <- read_docx() %>%
  body_add_par("Summary of DID Model", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")

# Save the document
print(doc, target = "did_model_summary_rounded.docx")

###############################################################################
# Graphs and Robusteness Checks
###############################################################################
## Marginal effects graph

# Calculate predicted values with confidence intervals
predicted_data <- combined_data %>%
  group_by(PostBan, TreatmentGroup) %>%
  summarise(mean_support = mean(abortion_support, na.rm = TRUE),
            se = sd(abortion_support, na.rm = TRUE) / sqrt(n())) %>%
  mutate(lower = mean_support - 1.96 * se,
         upper = mean_support + 1.96 * se)

# Create the plot with improved labels
fig1 <- ggplot(predicted_data, aes(x = factor(PostBan, labels = c("Before Ban", "After Ban")), 
                           y = mean_support, color = factor(TreatmentGroup, labels = c("Control (No Ban)", "Treatment (Ban)")))) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.5)) +
  labs(title = "Figure 3. Predicted Abortion Support by Post-Ban and Treatment Group",
       x = "Ban Status",
       y = "Predicted Abortion Support",
       color = "Region") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))
fig1

ggsave(filename = "predicted_abortion_support.png", 
       plot = fig1, 
       width = 8, 
       height = 6, 
       dpi = 300)

# Make copy of dataset for graphing
copy_dat21 <- GSS2021

# Bar graph for prochoic with survey response labels
fig2 <- ggplot(copy_dat21 %>% filter(!is.na(prochoic_rev)), 
       aes(x = factor(prochoic_rev, labels = c(
         "Strongly Disagree", 
         "Disagree", 
         "Neutral", 
         "Agree", 
         "Strongly Agree")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = '2021:"I Consider Myself Pro-Choice"',
    x = "Survey Response",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "prochoic.png", 
       plot = fig2, 
       width = 8, 
       height = 6, 
       dpi = 300)

# Bar graph for abany with "Yes" and "No" labels, excluding NA values
a <- ggplot(GSS2021 %>% filter(!is.na(abany)), 
       aes(x = factor(abany, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Any Reason",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
a

# Bar graph for abdefect with "Yes" and "No" labels, excluding NA values
b <- ggplot(GSS2021 %>% filter(!is.na(abdefect)), 
       aes(x = factor(abdefect, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Fetus Defect",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
b

# Bar graph for abhlth with "No" and "Yes" labels (higher numbers are pro-choice), excluding NA values
c <- ggplot(GSS2021 %>% filter(!is.na(abhlth)), 
            aes(x = factor(abhlth, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Mother's Health Risked",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
c

# Bar graph for abnomore with "No" and "Yes" labels (higher numbers are pro-choice), excluding NA values
d <- ggplot(GSS2021 %>% filter(!is.na(abnomore)), 
            aes(x = factor(abnomore, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Doesn't Want Kids",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
d

# Bar graph for abpoor with "No" and "Yes" labels (higher numbers are pro-choice), excluding NA values
e <- ggplot(GSS2021 %>% filter(!is.na(abpoor)), 
            aes(x = factor(abpoor, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Can't Afford Kids",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
e

# Bar graph for abrape with "No" and "Yes" labels (higher numbers are pro-choice), excluding NA values
f <- ggplot(GSS2021 %>% filter(!is.na(abrape)), 
            aes(x = factor(abrape, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Pregnant From Rape",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
f

# Bar graph for absingle with "No" and "Yes" labels (higher numbers are pro-choice), excluding NA values
g <- ggplot(GSS2021 %>% filter(!is.na(absingle)), 
            aes(x = factor(absingle, labels = c("No", "Yes")))) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    title = "Mother is Single and Does Not Want to Marry the Father",
    x = "Response",
    y = "Frequency"
  ) +
  theme_minimal()
g

# Bar plot for abgender
# Bar plot for abgender with "No" on the left
h2 <- GSS2022 %>%
  filter(!is.na(abgender)) %>%
  mutate(abgender = factor(abgender, levels = c(2, 1), labels = c("No", "Yes"))) %>%
  ggplot(aes(x = abgender)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Gender Selective Abortions", x = "Response", y = "Frequency") +
  theme_minimal()

h2

# Bar plot for gestate
# Bar plot for gestate with "No" on the left
j2 <- GSS2022 %>%
  filter(!is.na(gestate)) %>%
  mutate(gestate = factor(gestate, levels = c(2, 1), labels = c("No", "Yes"))) %>%
  ggplot(aes(x = gestate)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Legal Abortion After 4.5 Months", x = "Response", y = "Frequency") +
  theme_minimal()

j2

h <- a + b + c + d + e + f + g + h2 + j2
combined_plot21 <- h + plot_annotation(
  title = "Figure 2: A woman should be able to get an abortion if...",
)
combined_plot21

ggsave(filename = "exceptions.png", 
       plot = combined_plot21, 
       width = 8, 
       height = 6, 
       dpi = 300)

# Remove rows with NAs in relevant columns
sankey_data <- GSS2021 %>%
  filter(!is.na(prochoic_rev), !is.na(abrape)) %>%
  group_by(prochoic_rev, abrape) %>%
  summarize(count = n(), .groups = "drop")

# Create Sankey diagram
prochoic_abrape_sankey <- ggplot(sankey_data, 
                                 aes(axis1 = factor(prochoic_rev, labels = c("Strongly Disagree", "Disagree", 
                                                                             "Neutral", "Agree", "Strongly Agree")), 
                                     axis2 = factor(abrape, labels = c("No", "Yes")), 
                                     y = count)) +
  geom_alluvium(aes(fill = factor(prochoic_rev, labels = c("Strongly Disagree", "Disagree", 
                                                           "Neutral", "Agree", "Strongly Agree"))), 
                alpha = 0.8) +
  geom_stratum(width = 1/5, fill = "white", color = "black") +  
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)), 
            size = 8) +  
  scale_x_discrete(limits = c("Pro-Choice Stance", "Rape Exception Response"), 
                   expand = c(0, 0.05)) +
  scale_fill_manual(
    values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"), 
    name = "Pro-Choice Stance"
  ) +
  labs(
    title = "Figure 1. Pro-Choice Stance to Rape Exception Response",
    x = "",
    y = "Number of Respondents"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )

print(prochoic_abrape_sankey)

# Save the diagram
ggsave("prochoic_abrape_sankey.png", prochoic_abrape_sankey, width = 8, height = 6, dpi = 300)

combined_data %>%
  group_by(PostBan, TreatmentGroup) %>%
  summarise(mean_support = mean(abortion_support, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(PostBan, labels = c("Before Ban", "After Ban")), 
             y = mean_support, group = factor(TreatmentGroup), color = factor(TreatmentGroup))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Mean Abortion Support Before and After the Ban",
       x = "Time (Pre/Post-Ban)",
       y = "Mean Abortion Support",
       color = "Group") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()
##
# religion graph
combined_data %>%
  group_by(PostBan, religion) %>%
  summarise(mean_support = mean(abortion_support, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(PostBan, labels = c("Before Ban", "After Ban")), 
             y = mean_support, group = religion, color = religion)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Abortion Support by Religious Importance",
       x = "Time (Pre/Post-Ban)",
       y = "Mean Abortion Support",
       color = "Religion") +
  theme_minimal()

# Ensure religion_label column is added to combined_data
combined_data$religion_label <- factor(combined_data$religion, 
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Not Religious", "Slightly Religious", 
                                                  "Moderately Religious", "Very Religious"))

# Create a summary table for plotting
religion_trends <- combined_data %>%
  group_by(PostBan, religion_label) %>%
  summarise(
    mean_support = mean(abortion_support, na.rm = TRUE),
    .groups = "drop"
  )

# Create the plot
abortion_x_religion <- ggplot(religion_trends, aes(x = factor(PostBan, labels = c("Before Ban", "After Ban")), 
                            y = mean_support, group = religion_label, 
                            color = religion_label)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = c("lightblue", "blue", "darkblue", "black")) +
  labs(
    title = "Abortion Support by Religious Importance",
    x = "Time (Pre/Post-Ban)",
    y = "Mean Abortion Support",
    color = "Religiosity Level"
  ) +
  theme_minimal()

ggsave("abortion_x_religion.png", abortion_x_religion, width = 8, height = 6, dpi = 300)

# library(GGally)
# ggpairs(combined_data[, c("abortion_support", "religion", "education", "party3")],
#         aes(color = factor(TreatmentGroup, labels = c("Control", "Treatment"))),
#         title = "Correlations Between Key Variables")

# abortion x party
abortion_x_party <- ggplot(combined_data, aes(x = abortion_support, fill = party3)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Abortion Support by Party Affiliation",
       x = "Abortion Support Score",
       y = "Density",
       fill = "Party Affiliation") +
  scale_fill_manual(values = c("blue", "purple", "red")) +
  theme_minimal()

ggsave("abortion_x_party.png", abortion_x_party, width = 8, height = 6, dpi = 300)

# religion x education graph

RelxEd <- combined_data %>%
  group_by(religion, education) %>%
  summarise(mean_support = mean(abortion_support, na.rm = TRUE)) %>%
  ggplot(aes(x = education, y = factor(religion, labels = c("Not Religious", "Slightly Religious", "Moderately Religious", "Very Religious")), fill = mean_support)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(title = "Abortion Support by Religion and Education",
       x = "Education Level",
       y = "Religious Importance",
       fill = "Mean Support") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("religion_x_education.png", RelxEd, width = 8, height = 6, dpi = 300)

# abortion_support_x_treatment_group
abortion_support_x_treatment_group <- ggplot(combined_data, aes(x = abortion_support, fill = factor(TreatmentGroup, labels = c("Control", "Treatment")))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Abortion Support Density by Treatment Group",
    x = "Pro-Choice Stance",
    y = "Density",
    fill = "Group"
  ) +
  facet_wrap(~ factor(TreatmentGroup, labels = c("Control", "Treatment")), ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("red", "blue"))
abortion_support_x_treatment_group 
ggsave("abortion_support_x_treatment_group.png", abortion_support_x_treatment_group, width = 8, height = 6, dpi = 300)


just_ed <- ggplot(combined_data, aes(y = education, x = abortion_support, color = education)) +
  geom_point() +
  geom_jitter(alpha = 0.3) +
  labs(y = "Education Level", 
       x = "Abortion Support", 
       title = "Abortion Support by Education Level") +
  scale_color_discrete(guide = "none") +  # Suppress the legend directly here
  theme_minimal() +
  theme(legend.position = "none")  # Extra safeguard if needed
ggsave("just_ed.png", just_ed, width = 8, height = 6, dpi = 300)


# Select relevant variables for correlation
correlation_data <- GSS2021 %>%
  select(prochoic_rev, abany, abdefect, abhlth, abnomore, abpoor, abrape, absingle) %>%
  drop_na()  # Remove rows with NA values

# Compute the correlation matrix
cor_matrix <- cor(correlation_data)

# Load the ggcorrplot package for visualization
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}
library(ggcorrplot)

# Create the heatmap
ggcorrplot(cor_matrix, 
           method = "circle",  # Use circles for visualization
           lab = TRUE,         # Display correlation coefficients
           lab_size = 3,       # Adjust label size
           colors = c("blue", "white", "red"), # Gradient colors
           title = "Correlation Matrix of Abortion Variables",
           legend.title = "Correlation") +
  theme_minimal()

# Create a named vector for renaming variables
variable_labels <- c(
  prochoic_rev = "Pro-Choice",
  abany = "Any Reason",
  abdefect = "Fetal Defect",
  abhlth = "Health Risk",
  abnomore = "No More Kids",
  abpoor = "Too Poor",
  abrape = "Rape",
  absingle = "Single Mother"
)

# Rename the columns and rows in the correlation matrix
colnames(cor_matrix) <- variable_labels[colnames(cor_matrix)]
rownames(cor_matrix) <- variable_labels[rownames(cor_matrix)]

# Generate the heatmap with rotated x-axis labels
cormatrix <- ggcorrplot(cor_matrix, 
           method = "circle", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("blue", "white", "orange"), 
           title = "Correlation Matrix of Abortion Variables",
           legend.title = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
cormatrix
ggsave("cormatrix.png", cormatrix, width = 8, height = 6, dpi = 300)

# Install/Load the 'corrplot' package if not already installed
if (!require(corrplot)) install.packages("corrplot", dependencies = TRUE)
library(corrplot)

# Rename variables for clarity
colnames(cor_matrix) <- variable_labels[colnames(cor_matrix)]
rownames(cor_matrix) <- variable_labels[rownames(cor_matrix)]

# Generate the half-correlation matrix plot
corrplot(cor_matrix,
         method = "circle",        # Use circles for visual representation
         type = "upper",           # Show only the upper triangle
         addCoef.col = "black",    # Add numeric values to the lower triangle
         tl.col = "black",         # Color for labels
         tl.srt = 45,              # Rotate x-axis labels
         number.cex = 0.8,         # Text size for numbers
         diag = FALSE)             # Hide diagonal

# Make percent population under ban table

# Calculate the total population for each region
abortion_ban_population$total_population <- 
  abortion_ban_population$population_under_abortion_ban_2022 +
  abortion_ban_population$population_not_under_abortion_ban_2022

# Calculate percentages
abortion_ban_population_percentage <- abortion_ban_population
abortion_ban_population_percentage$percent_under_abortion_ban <- 
  (abortion_ban_population$population_under_abortion_ban_2022 / 
     abortion_ban_population$total_population) * 100

abortion_ban_population_percentage$percent_not_under_abortion_ban <- 
  (abortion_ban_population$population_not_under_abortion_ban_2022 / 
     abortion_ban_population$total_population) * 100

# Create the final dataset with required columns
abortion_ban_population_percentage <- abortion_ban_population_percentage[, c("region", "percent_under_abortion_ban", "percent_not_under_abortion_ban")]

# View the new dataset
print(abortion_ban_population_percentage)

library(stargazer)
# Export the model summary to an HTML file
stargazer(did_model, 
          type = "html", 
          out = "did_model_summary.html", 
          title = "Difference-in-Difference Model Results",
          dep.var.labels = "Support for Abortion",
          covariate.labels = c("PostBan", "TreatmentGroup", "PostBan * TreatmentGroup", 
                               "Education", "Gender", "Religion", "Party (3-level)"),
          single.row = TRUE)

# Cronbach's alpha

#install.packages("psych") 
library(psych)

# Create a data frame of the standardized variables
GSS2021_vars <- GSS2021 %>%
  select(prolife_std, prochoic_rev_std, abany_std, abdefect_std, abhlth_std,
         abnomore_std, abpoor_std, abrape_std, absingle_std)

# Calculate Cronbach's Alpha
alpha_2021 <- alpha(GSS2021_vars)
print(alpha_2021)

# Create a data frame of the standardized variables
GSS2022_vars <- GSS2022 %>%
  select(abbelief_std, abdefect_std, abgender_std, abhlth_std, abnomore_std,
         abpoor_std, abrape_std, absingle_std)

# Note: Since `abbelief` is double-weighted, you need to include it twice
GSS2022_vars <- cbind(GSS2022_vars, abbelief_std2 = GSS2022$abbelief_std)

# Calculate Cronbach's Alpha
alpha_2022 <- alpha(GSS2022_vars)
print(alpha_2022)

# factor analysis

#install.packages("factoextra")
library(factoextra)

# Subset your SAAI items
SAAI_items <- GSS2021 %>%
  select(prolife_std, prochoic_rev_std, abany_std, abdefect_std, abhlth_std, 
         abnomore_std, abpoor_std, abrape_std, absingle_std)

# Check KMO measure of sampling adequacy
KMO(SAAI_items)

# Check correlation matrix
cor_matrix <- cor(SAAI_items, use = "pairwise.complete.obs")
print(cor_matrix)

# Scree plot and parallel analysis
fa.parallel(SAAI_items, fa = "fa", n.iter = 100, show.legend = TRUE)

fa.parallel(SAAI_items, 
            fa = "fa", 
            n.iter = 100, 
            show.legend = TRUE, 
            main = "Figure 4: Parallel Analysis Scree Plots")


# Perform a three-factor analysis
fa_three <- fa(SAAI_items, nfactors = 3, rotate = "oblimin", fm = "ml")

# Print and visualize the results
print(fa_three)
fa.diagram(fa_three)

# Renaming factors
rownames(fa_three$loadings) <- c("Single Mother", "Poor", "No more kids", "Any Reason", 
                                 "Health", "Rape", "Defect", "Pro-choice", "Pro-life")

# Redrawing the diagram with updated labels
fa.diagram(fa_three, main = "Figure 5: Factor Analysis")

# diff-in-diff tests

did_model <- lm(abortion_support ~ PostBan * TreatmentGroup + education + gender + religion + party3, 
                data = combined_data)
summary(did_model)

# Extract coefficient and standard error for the interaction term
interaction_coef <- coef(did_model)["PostBan:TreatmentGroup"]
interaction_se <- sqrt(diag(vcov(did_model)))["PostBan:TreatmentGroup"]

# Print coefficient and standard error
interaction_coef
interaction_se

# One-tailed p-value
one_tailed_p <- pt(interaction_coef / interaction_se, df.residual(did_model), lower.tail = FALSE)
one_tailed_p

## Residual Analysis 

# Plot residuals
plot(did_model, which = 1)  # Residuals vs. fitted values
plot(did_model, which = 2)  # Q-Q plot of residuals

plot(did_model, which = 1, main = "Figure 6", sub.caption = "")
plot(did_model, which = 2, main = "Figure 7: Q-Q Plot of Residuals", sub.caption = "")

library(lmtest)

# Perform Breusch-Pagan test
bptest(did_model)


# Calculate Cook's Distance
cooksD <- cooks.distance(did_model)

# Plot Cook's Distance
plot(cooksD, type = "h", main = "Figure 8. Cook's Distance", ylab = "Cook's Distance")

# Identify potential influential points
influential_points <- which(cooksD > (4 / nrow(combined_data)))
influential_points

library(sandwich)

# Calculate robust standard errors
robust_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC3"))
robust_se

robust_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC3"))

# Extracting coefficients and robust SE
coef <- robust_se[, 1]  # Coefficients
robust_se_values <- robust_se[, 2]  # Robust Standard Errors

# Use stargazer for output
stargazer(did_model,
          type = "html",
          se = list(robust_se_values),
          title = "Model with Robust Standard Errors (HC3)",
          align = TRUE,
          no.space = TRUE,
          out = "model_with_robust_se.html")

# Delta coefficient / max effect
max_effect <- coef(did_model)["PostBan:TreatmentGroup"] * range(combined_data$PostBan)
max_effect

range(combined_data$abortion_support)

# Simulated normal curve data
x <- seq(-3, 3, length.out = 1000)
y <- dnorm(x, mean = 0, sd = 1)  # Normal distribution

# Policy levels
policy_level <- -1  # Current policy level (e.g., restrictive bans)
preferred_level <- 1  # Preferred policy level (e.g., moderate or permissive)

# Data frame for the curve
data <- data.frame(x, y)

# Plot
ggplot(data, aes(x, y)) +
  geom_line(size = 1, color = "black") +
  geom_vline(xintercept = policy_level, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = preferred_level, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = -1.5, y = 0.25, label = expression(P[t]), color = "black", angle = 90, vjust = -0.5) +
  annotate("text", x = 1.5, y = 0.25, label = expression(P^"*"), color = "black", angle = 90, vjust = -0.5) +
  annotate("text", x = 0, y = 0.25, label = expression(R[t]), size = 5, color = "black") +
  annotate("text", x = min(data$x), y = -0.02, label = "Less Policy", hjust = 0, size = 4) +
  annotate("text", x = max(data$x), y = -0.02, label = "More Policy", hjust = 1, size = 4) +
  labs(
    title = "Figure 9. Thermostatic Model of Public Opinion",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )

# Load required library
library(flextable)

# Prepare the data for the table
kmo_results <- data.frame(
  Item = c("Pro-Life", "Pro-Choice", "Any Reason", "Defect", "Health", 
           "No more kids", "Poor", "Rape", "Single"),
  MSA = c(0.95, 0.94, 0.95, 0.92, 0.88, 0.93, 0.92, 0.90, 0.90)
)

# Create a flextable
ft <- flextable(kmo_results) %>%
  set_header_labels(
    Item = "Item",
    MSA = "MSA"
  ) %>%
  add_header_row(
    values = c("Kaiser-Meyer-Olkin (KMO) Factor Adequacy", "Overall MSA: 0.92"),
    colwidths = c(1, 1)  # Fix: Ensure the sum matches the number of columns
  ) %>%
  theme_vanilla() %>%
  autofit()

# Display the table
ft
