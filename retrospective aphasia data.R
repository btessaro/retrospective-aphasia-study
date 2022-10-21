## 1 Script Header  #### 
#
# Date:10/05/2022
# Author: Bruna Tessaro
# Filename: Associative errors and Pyramids and Palm Trees Test
# Description:this script contains data preparation and analysis for the paper
# entitled "Associative errors and cognitive control in aphasia"
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 2 Library declarations  ####  
pacman::p_load(ggmosaic, ggplot2, here, openxlsx, data.table, tidyverse,
               dplyr, psych, correlation, cocor, reshape2, lme4, lmerTest,
               sjPlot, MuMIn, broom, gt, car, effects, Hmisc)

options(scipen=999)
#-----------------------------------------------------------------------#

## 3 Open and prepare data ####  

rootFolderPath <- here:: here()
original_recoded_dataset <- openxlsx::read.xlsx("data_categorise_errors_4Aug2022.xlsx")
recoded_dataset <- as_tibble (original_recoded_dataset)

## keep only people with aphasia and olny the first iteration 
##  this is to eliminate multiple data administration sessions from the same participant
recoded_dataset<-recoded_dataset %>% 
  filter(Diagnosis!='Control') %>% 
  filter(Test_iteration == 1)

## calculate naming accuracy (the original dataset does not have a column for total number of correct answers)
recoded_dataset$naming_total <- ifelse(recoded_dataset$Conventional_response_code == 'C', 1, 0)
naming <-recoded_dataset %>% 
  group_by (Subject)  %>% 
  dplyr::summarise(naming_total = sum(naming_total))

pnt_items <- recoded_dataset %>% distinct(Test_word) 

data <- merge(recoded_dataset,naming,  
              by.x = c('Subject'),
              by.y = c('Subject'))#,all.x=T)

## Select only Semantic naming errors from manual coding
data %>% distinct(Recoding)

data <- data %>% filter(Recoding %in% c("A", "CO", "NNA", "SB", "SO", "SP"))

data <- rename(data, 
               participant = Subject,
               PPT = Pyramids_and_Palms_Test,
               naming_total = naming_total.y)

#select relevant columns to work with 
data <- data %>% dplyr::select('participant',
                               'Months_post_onset',
                               'Age_when_tested',
                               'Diagnosis',
                               'WAB_Aphasia_Quotient',
                               'Education_level',
                               'Gender',
                               'Recoding',
                               'Test_word',
                               'Regular_response',
                               'PPT',
                               'naming_total',
                               'PPT_originalscore',
                               'Lexical_frequency',
                               'Age_of_acquisition',
                               'Visual_complexity',
                               'Imageability',
                               'Word_length')


# only include participants with Pyramids and Palm trees Test scores and data entries with regular response given
data <- data %>% filter(!is.na(PPT)) %>% filter(!is.na(Regular_response))
head(data)

# read Eskimo as eskimo to be able to get ratings from other corpus
data$Test_word[grepl('Eskimo', data$Test_word)] <- 'eskimo'

data %>% filter(Recoding == "A") %>% select (Test_word, Regular_response) %>% unique()

## 3.1 Get data from other corpora ####

## 3.1.1 Category Cohort Size ####
ccs_ratings <- openxlsx::read.xlsx("Competitors_ratings.xlsx")
ccs_ratings <- as_tibble (ccs_ratings)
ccs_ratings$Test_word[grepl('cheerleader', ccs_ratings$Test_word)] <- 'cheerleaders'
ccs_ratings$Test_word[grepl('crutch', ccs_ratings$Test_word)] <- 'crutches'
ccs_ratings$Test_word[grepl('Eskimo', ccs_ratings$Test_word)] <- 'eskimo'

model_data <- merge(data, ccs_ratings[,c("Test_word","Competitors")], 
                    by.x = c('Test_word'),
                    by.y = c('Test_word'), all.x = T)

model_data_alternative <- merge(data, ccs_ratings[,c("Test_word","Competitors")], 
                                by.x = c('Test_word'),
                                by.y = c('Test_word'), all.x = T)

#combine Non noun associates with associates
model_data$Recoding[model_data$Recoding == "NNA"] <- "A" 
model_data_alternative$Recoding[model_data_alternative$Recoding == "NNA"] <- "A" 
check_data <- model_data %>% filter(is.na(Competitors))


## 3.1.2 Small World of Words Corpora ####

##read data
swow <- read_csv("strength.SWOW-EN.R1.csv")
swow<- as_tibble(swow)

## adapt words in order to be able to merge with our dataset
swow$cue[grepl('cheerleader', swow$cue)] <- 'cheerleaders'
swow$cue[grepl('crutch', swow$cue)] <- 'crutches'

## merge model_data with swow data, and add associative strenght per word pairs
model_data <- merge(model_data,swow[,c("cue","response","R1.Strength")],
                    by.x = c('Test_word','Regular_response'),
                    by.y = c('cue','response'),all.x=T) 

# model data does not have the item "nose" because the participants that have semantic errors in this items do not have PPT scores,
# so the participants that have PPT scores did not make a semantic error for this particular item
test_item_nose<- recoded_dataset %>%
  filter(Test_word == "nose" & Conventional_response_code == "S") %>% 
  select(Test_word, Subject, Pyramids_and_Palms_Test)

# example ratings for the word "crown" for presentations
swow %>% filter(cue == "crown") %>% 
  select(c(cue, response, R1.Strength)) %>% 
  rename(StrengthofAssociation = R1.Strength)

## create strength of associates ratings
top3_associative_strength_measure <- swow %>% group_by(cue) %>% 
  top_n(3, R1.Strength) %>% #gets top three highest values
  slice_head(n=3) %>% #eliminates a fourth value when it's the same as previous ones. This way it sums exactly three values for all items
  summarise(strength_of_associates = sum(R1.Strength)) #sums top three values per  item

model_data <- merge(model_data, top3_associative_strength_measure[,c("cue", "strength_of_associates")],
                    by.x = 'Test_word',
                    by.y = 'cue', all.x = T)


## 3.1.3 International Picture-Naming Project Database #### 

## read data
picture_naming_project <- readxl::read_xls("USpno.xls", sheet = 2)
picture_naming_project<- as_tibble(picture_naming_project)

## select appropriate columns
picture_naming_project <- picture_naming_project %>% 
  select(ename, ovcjpg, lnefreq, ecdi) %>% 
  rename(Test_word = ename,
         pnp_visual_complexity = ovcjpg,
         pnp_lexical_freq = lnefreq,
         pnp_age_of_acquisition = ecdi)

## adapt words in order to be able to merge with our dataset
picture_naming_project$Test_word[grepl('strawberry', picture_naming_project$Test_word)] <- 'strawberries'


## 3.1.4 Meteyard and Bose (2018) ####
## read data
mette_ratings <- openxlsx::read.xlsx("Lotte word properties PNT_2018.xlsx")
mette_ratings <- as_tibble(mette_ratings)

## select appropriate columns
mette_ratings <- mette_ratings %>% 
  rename(Test_word = Target.word,
         mette_lenght = `Length.(phonemes)`,
         mette_visual_complexity = 'Visual.complexity',
         mette_lexical_freq = `Lemma.(base.10).log.frequency.per.million.(CELEX)`,
         mette_imageability = Imageability,
         mette_name_agreement = Name.agreement) %>% 
  select(Test_word, mette_lenght, mette_imageability,
         mette_lexical_freq, mette_name_agreement, 
         mette_visual_complexity) 

mette_ratings <- mette_ratings%>% 
  mutate(mette_lenght = as.numeric(mette_lenght),
         mette_lexical_freq = as.numeric(mette_lexical_freq),
         mette_imageability = as.numeric(mette_imageability),
         mette_name_agreement = as.numeric(mette_name_agreement),
         mette_visual_complexity = as.numeric(mette_visual_complexity))


## adapt words in order to be able to merge with our dataset
mette_ratings$Test_word[grepl('moustache', mette_ratings$Test_word)] <- 'mustache'

## merge with model_data 
model_data <- merge(model_data, mette_ratings,
                    by.x = c('Test_word'),
                    by.y = c('Test_word')) 


## 3.1.5 Johnston et al (2010) ####
## read data
johnston_ratings <- readxl::read_xls("Johnston_etal_suppl.xls", sheet = 1)
johnston_ratings <- as_tibble(johnston_ratings)

## select appropriate columns
johnston_ratings <- johnston_ratings %>% 
  rename(Test_word = 'US NAME',
         john_age_of_acquisition = AOA,
         john_familiarity = FAM) %>% 
  select(File_n, Test_word, john_age_of_acquisition, john_familiarity)

# repeated items in the PNT: hat, glass, bottle,
# I checked the pictures on International picture naming project and PNT to see which one is most appropriate
johnston_ratings <- johnston_ratings %>% filter(!(File_n == 511)) # eliminate duplicated glass
johnston_ratings <- johnston_ratings %>% filter(!(File_n == 18)) # eliminate duplicated bottle
johnston_ratings <- johnston_ratings %>% filter(!(File_n == 199)) # eliminate duplicated hat

## merge with model_data
model_data <- merge(model_data, johnston_ratings,
                    by.x = c('Test_word'),
                    by.y = c('Test_word'), all.x = TRUE) 

# Age of acquisition measures
age_of_acquisition <- model_data %>% 
  group_by(Test_word) %>% 
  summarise(mappd_AoA = mean(Age_of_acquisition),
            john_AoA = mean(john_age_of_acquisition)) %>% 
  select(Test_word, mappd_AoA,
         john_AoA)

174-sum(is.na(age_of_acquisition$mappd_AoA)) #data for 156 items
174-sum(is.na(age_of_acquisition$john_AoA)) # data for 154 items

# Familiarity measures: 154 ratings
familiarity <- model_data %>% 
  group_by(Test_word) %>% 
  select(Test_word, john_familiarity) %>% 
  summarise(familiarity = mean(john_familiarity)) 
174-sum(is.na(familiarity$familiarity))

# Imageability
imageability <- model_data %>% 
  group_by(Test_word) %>% 
  summarise(mappd_imageability = mean(Imageability),
            mette_imageability = mean(mette_imageability)) %>% 
  select(Test_word, mappd_imageability,
         mette_imageability)
174-sum(is.na(imageability$mappd_imageability)) # data for 140 items
174-sum(is.na(imageability$mette_imageability)) # data for all items

# Lexical frequency
lexical_frequency <- model_data %>% 
  group_by(Test_word) %>% 
  summarise(mappd_freq = mean(Lexical_frequency),
            mette_freq = mean(mette_lexical_freq)) %>% 
  select(Test_word, mappd_freq,
         mette_freq)

174-sum(is.na(lexical_frequency$mappd_freq)) # data for 174 items
174-sum(is.na(lexical_frequency$mette_freq)) # data for 174 items

# Name agreement
name_agreement <- model_data %>% 
  group_by(Test_word) %>% 
  summarise(mette_name_agreement = mean(mette_name_agreement)) %>% 
  select(Test_word, mette_name_agreement)
174-sum(is.na(name_agreement$mette_name_agreement)) # data for 174 items

# Visual complexity measures
visual_complexity <- model_data %>% 
  group_by(Test_word) %>% 
  summarise(mappd_vc = mean(Visual_complexity),
            mette_vc = mean(mette_visual_complexity)) %>% 
  select(Test_word, mappd_vc,
         mette_vc)

174-sum(is.na(visual_complexity$mappd_vc))  # data for 174 items
174-sum(is.na(visual_complexity$mette_vc))  # data for 174 items

# control variables table

item_variables <- merge(pnt_items,age_of_acquisition,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,familiarity,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,imageability,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,lexical_frequency,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,name_agreement,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,visual_complexity,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,snd_ratings,
                        by.x = c('Test_word'),
                        by.y = c('Test_word'), all.x = TRUE) 
item_variables <- merge(item_variables,top3_associative_strength_measure,
                        by.x = c('Test_word'),
                        by.y = c('cue'), all.x = TRUE) 
item_variables <- item_variables %>% select(Test_word, mappd_AoA,
                                            familiarity, mette_imageability,
                                            mappd_freq, mette_name_agreement,
                                            mappd_vc, Competitors,
                                            strength_of_associates) %>% 
  mutate_if(is.numeric, round,2)

table_item_variables <- item_variables %>% gt()
gt::gtsave(data = table_item_variables, filename = 'table_item_ratings.rtf')

## 3.2 Determine associative errors #####
#-----------------------------------------------------------------------#
## 3.2.1 Associative errors recoded manually ####
# associative errors coded as 1, other semantic errors coded as 0

model_data$associative_recoding <-  ifelse(model_data$Recoding == 'A', 1, 0)

## 3.2.2 Associative errors according to associative corpus ####
# associative errors coded as 1, other semantic errors coded as 0
model_data <- model_data %>%
  mutate(associative_swow = if_else(is.na(R1.Strength), 0, R1.Strength))  %>%
  mutate(associative_swow = if_else(associative_swow > 0, 1, associative_swow))

associates_coordinates <- model_data %>% filter(Recoding == "CO" & 
                                                  associative_swow == 1)

associates_coordinates_unique <- model_data %>% filter(Recoding == "CO" & 
                                                         associative_swow == 1) %>% 
  select(Test_word, Regular_response) %>% 
  unique()

#-----------------------------------------------------------------------#
#reorganise columns
model_data <- model_data %>% 
  relocate(participant, 
           Age_when_tested,
           Months_post_onset, 
           Diagnosis,
           WAB_Aphasia_Quotient,
           Education_level,
           Gender,
           naming_total,
           PPT,
           Test_word,
           Regular_response,
           Recoding,
           associative_recoding,
           R1.Strength,
           associative_swow)


## 3.5 Scale data for model ####
## calculate means per participant AND standardise (this is so we have only one
## observation per participant to scale) 
scale_pyramids<- model_data %>% group_by(participant) %>%
  summarise(pyramids = mean(PPT)) %>% 
  transmute(
    participant = participant,
    scaled_pyramids = scale(pyramids))

scale_by_item <- model_data %>% group_by(Test_word) %>% 
  summarise(category_cohort_size = mean(Competitors),
            strength_of_associates = mean(strength_of_associates),
            mette_imageability = mean(mette_imageability),
            mappd_age_of_acquisition = mean(Age_of_acquisition),
            john_familiarity = mean(john_familiarity),
            mappd_lexical_frequency = mean(Lexical_frequency),
            mette_name_agreement = mean(mette_name_agreement),
            mappd_visual_complexity = mean(Visual_complexity))


scaled_item_predictors <- scale_by_item %>% 
  transmute(Test_word = Test_word,
            scaled_category_cohort_size = scale(category_cohort_size),
            scaled_strength_associates = scale(strength_of_associates),
            scaled_imageability = scale(mette_imageability),
            scaled_aoa = scale(mappd_age_of_acquisition),
            scaled_familiarity = scale(john_familiarity),
            scaled_frequency = scale(mappd_lexical_frequency),
            scaled_name_agreement = scale(mette_name_agreement),
            scaled_visual_complex = scale(mappd_visual_complexity))


#Merge standardised scaled values with model data 
model_data <- merge(model_data, scale_pyramids,
                    by.x = c('participant'),
                    by.y = c('participant'),all.x=T)

model_data <- merge(model_data, scaled_item_predictors,
                    by.x = c('Test_word'),
                    by.y = c('Test_word'),all.x=F)

model_data <- model_data %>% 
  select(participant, 
         Age_when_tested,
         Months_post_onset, 
         Diagnosis,
         WAB_Aphasia_Quotient,
         Education_level,
         Gender,
         PPT,
         scaled_pyramids,
         naming_total,
         Test_word,
         Regular_response,
         Recoding,
         associative_recoding,
         R1.Strength,
         associative_swow,
         scaled_category_cohort_size,
         scaled_strength_associates,
         scaled_imageability,
         scaled_familiarity,
         scaled_frequency,
         scaled_name_agreement,
         scaled_visual_complex,
         scaled_aoa)


## 3.6 Create data without associative-coordinates ####
#create dataset to run models with swow data
model_data_no_coordinates <- model_data[!(model_data$Recoding == "CO" & model_data$associative_swow == 1), ]

#write.xlsx(model_data_no_coordinates, "model_data_no_coordinates_130922.xlsx")

## the following lines check error codings
## number of semantic errors that were manualy coded as 
## Coordinates but were also associates in swow: 765
coordinate_swow<-model_data %>% 
  filter(Recoding == "CO" & associative_swow == 1) 


## 3.7 Agreements and disagreements sets of error coding ####
all_target_error_combinations <- model_data %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(!(Recoding == "CO" & associative_swow == 1)) %>% 
  unique()

associative_agreement_set<-model_data %>% 
  filter(associative_recoding == 1 & associative_swow == 1) 

non_associative_agreement_set<- model_data %>% 
  filter(associative_recoding == 0 & associative_swow == 0)

disagreement_set1<-model_data %>% 
  filter(associative_recoding == 0 & associative_swow == 1)

disagreement_set2<-model_data %>% 
  filter(associative_recoding == 1 & associative_swow == 0)

items_disagreement_set2<-model_data %>% 
  filter(associative_recoding == 1 & associative_swow == 0) %>% 
  select(Test_word) %>% unique()


## check data - how many associates and non associates for each type of coding?
model_data %>% filter(associative_recoding == 1) %>% nrow()

model_data %>% filter(associative_recoding == 0) %>% nrow()

model_data %>% filter(associative_swow == 1) %>% nrow()

model_data %>% filter(associative_swow == 0) %>% nrow()

model_data %>% filter(Recoding == "CO") %>% nrow()

model_data_no_coordinates %>%  filter(associative_swow == 1) %>% nrow()

model_data_no_coordinates %>%  filter(associative_swow == 0) %>% nrow()

#-----------------------------------------------------------------------#

## 4 Modelling ####
## Optimizers:
Max <- glmerControl(optCtrl=list(maxfun=2e4))
Bob <- glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
Ned <- glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5))
#-----------------------------------------------------------------------#
## 4.1 Analysis 1 ####

## Model 1.MC #### 
model1mc <- glmer(associative_recoding ~ scaled_pyramids + 
                    (1|participant) + (1|Test_word), data = model_data,
                  family = binomial)

summary(model1mc)

## testing the contribution of random effects
analysis1_recoding_test_words <- glmer(associative_recoding ~ scaled_pyramids + 
                                         (1|Test_word), data = model_data,
                                       family = binomial)

summary(analysis1_recoding_test_words)

analysis1_recoding_participant <- glmer(associative_recoding ~ scaled_pyramids + 
                                          (1|participant), data = model_data,
                                        family = binomial)
summary(analysis1_recoding_participant)

anova(model1mc, 
      analysis1_recoding_test_words)
anova(model1mc, 
      analysis1_recoding_participant)
anova(model1mc,
      analysis1_recoding_participant,
      analysis1_recoding_test_words)

x_axis = list(stemp = seq(-2, 1))


plot(allEffects(model1mc),
     xlim = x_axis,
     xlab = "Pyramids and Palm Trees",
     ylab = "Associative error",
     title = " ",
     axis.lim = x_axis) 


## Table S3 ####

tab_model(analysis1_recoding_test_words, 
          analysis1_recoding_participant,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Model 1.MC - Items as Random Effects", 
                        "Model 1.MC - Participants as Random Effects"), 
          p.style = "numeric", 
          file = "TableS3.html")

## Model 1.AC ####
model1ac <- glmer(associative_swow ~ scaled_pyramids + 
                    (1|participant) + (1|Test_word), 
                  data = model_data_no_coordinates,
                  family = binomial)

summary(model1ac)
plot(allEffects(model1ac),
     xlab = "Pyramids and Palm Trees",
     ylab = "Associative error",
     title = " ")


analysis1_swow_test_words <- glmer(associative_swow ~ scaled_pyramids + 
                                     (1|Test_word), 
                                   data = model_data_no_coordinates,
                                   family = binomial)

summary(analysis1_swow_test_words)


analysis1_swow_participant <- glmer(associative_swow ~ scaled_pyramids + 
                                      (1|participant), 
                                    data = model_data_no_coordinates,
                                    family = binomial)
summary(analysis1_swow_participant)

## Table S4 ####
tab_model(analysis1_swow_test_words, 
          analysis1_swow_participant,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Model 1.AC - Items as Random Effects", 
                        "Model 1.AC - Participants as Random Effects"), 
          p.style = "numeric", 
          file = "TableS4.html")

anova(model1ac, 
      analysis1_swow_test_words)

anova(model1ac, 
      analysis1_swow_participant)

plot((analysis1_swow_full_model))

# Table 3 #### 
tab_model(model1mc, 
          model1ac,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Manual recoding (Model 1.MC)", "Associative corpus (Model 1.AC)"), 
          p.style = "numeric", 
          file = "Models PPT as predictor of associative error.html")

## 4.2 Analysis 2 ####

## 4.2.1 Correlation between new predictors #### 

correlation <- model_data %>% 
  select(Test_word, 
         scaled_category_cohort_size,
         scaled_strength_associates) %>% 
  group_by(Test_word) %>% 
  summarise(scaled_category_cohort_size = mean(scaled_category_cohort_size),
            scaled_strength_associates = mean(scaled_strength_associates))

cor.test(correlation$scaled_category_cohort_size, correlation$scaled_strength_associates)

ggplot(model_data, aes(x=scaled_category_cohort_size, y=scaled_strength_associates)) +
  geom_point()

cor.test(model_data$scaled_category_cohort_size, model_data$scaled_strength_associates)


# why reduced number of obs??
# because there is no "eskimo" item on the corpus so the model does not have a 
# strength of associates value for these words so the model has 27 less observations 
# than the full dataset with 2587
NO_NA_model_data <- model_data %>% filter(!is.na(scaled_strength_associates))
NA_model_data <- model_data %>% filter(is.na(scaled_strength_associates))

# 4.2.2. Models ####
options(na.action = na.exclude)
# recoding
main_effects_recoding <- glmer(associative_recoding ~ scaled_pyramids + 
                                 scaled_category_cohort_size +
                                 scaled_strength_associates +
                                 (1|participant) + (1|Test_word), 
                               data = model_data,
                               family = binomial)
summary(main_effects_recoding)
plot(allEffects(main_effects_recoding))

# same results with na.action = na.exclude 
main_effects_recoding_na <- glmer(associative_recoding ~ scaled_pyramids + 
                                    scaled_category_cohort_size +
                                    scaled_strength_associates +
                                    (1|participant) + (1|Test_word), 
                                  data = model_data,
                                  family = binomial,
                                  na.action = na.exclude)
summary(main_effects_recoding_na)

# Model 2.MC - Model reduction ####
options(na.action = "na.omit")

#create null model 
analysis2_recoding0 <- glmer(associative_recoding ~  
                               1 +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding0)

analysis2_recoding1 <- glmer(associative_recoding ~ scaled_pyramids*  
                               scaled_category_cohort_size* 
                               scaled_strength_associates +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding1)
plot(allEffects(analysis2_recoding1))


# step 1 to simplify the model: remove 3-way interaction  
analysis2_recoding2 <- glmer(associative_recoding ~ 
                               scaled_pyramids*scaled_category_cohort_size +
                               scaled_pyramids*scaled_strength_associates +  
                               scaled_category_cohort_size*scaled_strength_associates +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding2)
plot(allEffects(recoding1))

anova(analysis2_recoding1, analysis2_recoding2)
tab_model(analysis2_recoding1,
          analysis2_recoding2,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = c("Manual recoding - 1",
                        "Manual recoding - 2"), 
          p.style = "numeric_stars", 
          file = "Step1.html")

# step 2 to simplify the model: remove third least significant interaction (scaled_pyramids*scaled_category_cohort_size)
analysis2_recoding3 <- glmer(associative_recoding ~ 
                               scaled_pyramids*scaled_strength_associates +  
                               scaled_category_cohort_size*scaled_strength_associates +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding3) 
plot(allEffects(recoding1))

anova(analysis2_recoding2, analysis2_recoding3)
tab_model(analysis2_recoding2,
          analysis2_recoding3,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = c("Manual recoding - 1",
                        "Manual recoding - 2"), 
          p.style = "numeric_stars", 
          file = "Step2.html")

## Model 2MC - Final model ####
# step 3 to simplify the model: remove second least significant interaction (scaled_pyramids*scaled_strength_associates)
analysis2_recoding4 <- glmer(associative_recoding ~  scaled_pyramids +
                               scaled_category_cohort_size*scaled_strength_associates +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding4) ## FINAL MODEL
round(vif(analysis2_recoding4),2)
plot_model(analysis2_recoding4, type = "pred",
           terms = "scaled_strength_associates",
           axis.title = "Associative error")

plot(allEffects(analysis2_recoding4))
xlab = "Pyramids and Palm Trees",
ylab = "Associative error",
title = " ")


anova(analysis2_recoding3, analysis2_recoding4)
tab_model(analysis2_recoding3,
          analysis2_recoding4,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = c("Manual recoding - 3",
                        "Manual recoding - 4"), 
          p.style = "numeric_stars", 
          file = "Step3.html")

# Figure 1A ####
interaction1 <- plot_model(analysis2_recoding4, type="int",
                           mdrt.values =  "meansd",
                           axis.title = c("Category Cohort Size", "Probability of an associative error"),
                           title = " ",
) + aes(linetype=group, color=group) +
  labs(linetype = "Strength of Associates", color = "Strength of Associates")
interaction1


# table with all models full group recoding
tab_model(analysis2_recoding1,
          analysis2_recoding2,
          analysis2_recoding3,
          analysis2_recoding4,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept", 
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density", 
                          "Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density", 
                          "Pyramids and Palm Trees:Strength of Associates",
                          "Semantic Neighbourhood Density:Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density:Strength of Associates",
                          "Strength of Associates:Semantic Neighbourhood Density"),
          dv.labels = c("Manual recoding - 1",
                        "Manual recoding - 2",
                        "Manual recoding - 3",
                        "Manual recoding - 4",
                        "Manual recoding - 5"), 
          p.style = "numeric_stars", 
          file = "Analysis 2 - recoding.html")

# Model 2.AC ####

#create null model 
analysis2_swow0 <- glmer(associative_swow ~  
                           1 +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow0)

analysis2_swow1 <- glmer(associative_swow ~ scaled_pyramids* 
                           scaled_category_cohort_size*
                           scaled_strength_associates + 
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow1)

## step 1 to simplify the model: remove 3-way interaction
analysis2_swow2 <- glmer(associative_swow ~ scaled_pyramids*scaled_category_cohort_size +
                           scaled_pyramids*scaled_strength_associates +  
                           scaled_category_cohort_size*scaled_strength_associates + 
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow2)

anova(analysis2_swow1,analysis2_swow2)

## step 2 to simplify the model: remove third least significant interaction (scaled_pyramids*scaled_category_cohort_size) 
analysis2_swow3 <- glmer(associative_swow ~ scaled_pyramids*scaled_strength_associates +  
                           scaled_category_cohort_size*scaled_strength_associates +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow3)

anova(analysis2_swow2, analysis2_swow3)

## step 3 to simplify the model: remove second least significant interaction (scaled_strength_associates*scaled_category_cohort_size) 

analysis2_swow4 <- glmer(associative_swow ~ scaled_pyramids*scaled_strength_associates +  
                           scaled_category_cohort_size + scaled_strength_associates +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow4)
anova(analysis2_swow3, analysis2_swow4)

## step 4 to simplify the model: remove second least significant interaction (scaled_strength_associates*scaled_pyramids) 

analysis2_swow5 <- glmer(associative_swow ~ scaled_pyramids +  
                           scaled_category_cohort_size + scaled_strength_associates +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow5)
round(vif(analysis2_swow5),2)
anova(analysis2_swow4, analysis2_swow5)

## step 5 to simplify the model: remove non significant predictor scaled_strength_associates

analysis2_swow6 <- glmer(associative_swow ~ scaled_pyramids +  
                           scaled_category_cohort_size +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_swow6)
anova(analysis2_swow5, analysis2_swow6)



# table with all models full group online corpus
tab_model(analysis2_swow1,
          analysis2_swow2,
          analysis2_swow3,
          analysis2_swow4,
          analysis2_swow5,
          analysis2_swow6,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE, 
          pred.labels = c("Intercept", 
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density", 
                          "Pyramids and Palm Trees:Strength of Associates",
                          "Semantic Neighbourhood Density:Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density:Strength of Associates",
                          "Strength of Associates:Semantic Neighbourhood Density"),
          dv.labels = c("Association corpus - 1 ",
                        "Association corpus -  2",
                        "Association corpus -  3",
                        "Association corpus - 4",
                        "Association corpus - 5",
                        "Association corpus - 6"), 
          p.style = "numeric_stars", 
          file = "Analysis 2 - swow.html")

# Table 5 ####

tab_model(analysis2_recoding4,
          analysis2_swow5,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE, 
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Category Cohort Size",
                          "Strength of Associates",
                          "Category Cohort Size:Strength of Associates"),
          dv.labels = c("Model 2.MC",
                        "Model 2.AC"), 
          p.style = "numeric_stars", 
          file = "Analysis 2 - best models.html")


## Analysis 3 - control variables ####

#correlation between item predictors
item_predictors <- scaled_item_predictors %>% select (-Test_word)
item_predictors <- as.data.frame(item_predictors)

correlation_matrix <- rcorr(as.matrix(item_predictors))
correlation_matrix


## best fit model recoding
options(na.action="na.omit")
analysis3_recoding1 <- glmer(associative_recoding ~  
                               scaled_pyramids +
                               scaled_category_cohort_size*scaled_strength_associates +
                               scaled_aoa +
                               scaled_familiarity +
                               scaled_imageability +
                               scaled_frequency +
                               scaled_name_agreement +
                               scaled_visual_complex +
                               (1|participant) + (1|Test_word), 
                             data = model_data,
                             family = binomial,
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis3_recoding1)
round(vif(analysis3_recoding1),2)

tab_model(analysis3_recoding1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Category Cohort Size",
                          "Strength of Associates",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity",
                          "Category Cohort Size:Strength of Associates"),
          dv.labels = "Manual recoding - best fit model with added control variables", 
          p.style = "numeric_stars", 
          file = "Analysis 3 - full group recoding - best fit model with added control variables.html")

## best fit model swow
analysis3_swow1 <- glmer(associative_swow ~
                           scaled_pyramids +
                           scaled_category_cohort_size + 
                           scaled_strength_associates +
                           scaled_aoa +
                           scaled_familiarity +
                           scaled_imageability +
                           scaled_frequency +
                           scaled_name_agreement +
                           scaled_visual_complex +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis3_swow1)
round(vif(analysis3_swow1),2)
tab_model(analysis3_swow1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity"),
          dv.labels = "Associative corpus - best fit model with added control variables", 
          p.style = "numeric_stars", 
          file = "Analysis 3 - full group swow - best fit model with added control variables.html")


tab_model(analysis3_recoding1, 
          analysis3_swow1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = c("Model A2.3",
                        "Model A2.4"),
          pred.labels = c("Intercept",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity",
                          "Semantic Neighbourhood Density:Strength of Associates",
                          "Pyramids and Palm Trees"),
          p.style = "numeric_stars", 
          file = "Analysis 3 - best fit models with added control variables.html")

analysis3_swow2 <- glmer(associative_swow ~
                           scaled_pyramids +
                           scaled_category_cohort_size + 
                           scaled_aoa +
                           scaled_familiarity +
                           scaled_imageability +
                           scaled_frequency +
                           scaled_name_agreement +
                           scaled_visual_complex +
                           (1|participant) + (1|Test_word), 
                         data = model_data_no_coordinates,
                         family = binomial, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis3_swow2)

tab_model(analysis3_swow2,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity"),
          dv.labels = "Associative corpus - best fit model with added control variables", 
          p.style = "numeric_stars", 
          file = "Analysis 3 - full group swow - best fit model 2 with added control variables.html")


#-----------------------------------------------------------------------#


#-----------------------------------------------------------------------#

## SUBGROUP ANALYSIS  #### 
# This subset includes participants that produced less than
# 5% of phonological errors and made more semantic errors than
# any other error type in the Philadelphia Naming Test. 

#-----------------------------------------------------------------------#

##  1 Create subset ####
model_data_subset<-original_recoded_dataset %>% 
  filter(Diagnosis!='Control') %>% #exlcude controls
  filter(Test_iteration == 1) %>% #include only first iteration
  filter(!is.na(Pyramids_and_Palms_Test)) #include only participants with Pyramids_and_Palms_Test scores

model_data_subset<-rename(model_data_subset, 
                          participant = Subject, 
                          PPT = Pyramids_and_Palms_Test)

## create proportion tables with all responses in naming
response_per_ppt<- xtabs(formula= ~ participant + 
                           Conventional_response_code, data=model_data_subset)

proportion_table<-round(prop.table(response_per_ppt, margin=1), digits=3)
proportion_table<-as_tibble(proportion_table)

### collapse (S + M + NR) in one proportion per participant
total_semix<- proportion_table  %>% 
  filter(Conventional_response_code %in% c("S", "M", "NR")) %>%
  group_by(participant) %>% 
  mutate(semix_sum = sum(n)) %>%
  group_by(participant) %>% 
  summarise(total_semix = mean(semix_sum))


## collapse (F + N + AN) in one proportion per participant
total_formal<- proportion_table  %>% 
  filter(Conventional_response_code %in% c("F", "N", "AN")) %>%
  group_by(participant) %>% 
  mutate(formal_sum = sum(n)) %>%
  group_by(participant) %>% 
  summarise(total_formal = mean(formal_sum))

## merge semix and formal proportions in one dataset
subset<- merge(total_semix, total_formal,
               by.x = c('participant'),
               by.y = c('participant'),all.x=T)

## keep only participants whose semix proportion is bigger than formal 
subset <- subset %>% filter(total_semix > total_formal)

#filter out those participants who have more than 5% of (F + N + AN) errors
subset <- subset %>% filter(total_formal<0.05)

model_data_subset<- merge(subset,model_data,
                          by.x = c('participant'),
                          by.y = c('participant'),all.x=T)

model_data_subset_no_coordinates <- model_data_subset[!(model_data_subset$Recoding == "CO" & model_data_subset$associative_swow == 1), ]
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 2 Participant's demographic information #### 

subset_demographics <- model_data_subset %>% 
  mutate(naming_total = naming_total * 100/175) %>% 
  summarise(n = n_distinct(participant),
            mean_naming = mean(naming_total),
            sd_naming = sd(naming_total),
            median_naming = median(naming_total),
            min_naming = min (naming_total),
            max_naming = max(naming_total),
            median_naming = median(naming_total),
            mean_ppt = mean(PPT*52/100),
            sd_ppt = sd(PPT*52/100),
            median_ppt = median(PPT*52/100),
            min_ppt = min (PPT*52/100),
            max_ppt = max(PPT*52/100),
            median_ppt = median(PPT*52/100)
  )

(table_descriptive_subset <- subset_demographics %>%
    gt(subset_demographics) %>%
    tab_spanner(
      label = "Pyramids and Palm Trees Test",
      columns = c(mean_ppt, sd_ppt, median_ppt, min_ppt, max_ppt)
    ) %>%
    cols_merge(
      columns = c(mean_ppt, sd_ppt),
      hide_columns = c(sd_ppt),
      pattern = "{1} ({2})"
    ) %>%
    cols_merge(
      columns = c(min_ppt, max_ppt),
      hide_columns = c(max_ppt),
      pattern = "{1} - {2}"
    ) %>%
    cols_label(
      mean_ppt = "Mean (SD)",
      min_ppt = "Range",
      median_ppt = "Median"
    ) %>%
    tab_spanner(
      label = "Philadelphia Naming Test",
      columns = c(mean_naming, sd_naming, median_naming, min_naming, max_naming)
    ) %>%
    cols_merge(
      columns = c(mean_naming, sd_naming),
      hide_columns = c(sd_naming),
      pattern = "{1} ({2})"
    ) %>%
    cols_merge(
      columns = c(min_naming, max_naming),
      hide_columns = c(max_naming),
      pattern = "{1} - {2}"
    ) %>%
    cols_label(
      mean_naming = "Mean (SD)",
      min_naming = "Range",
      median_naming = "Median"
    ) %>%
    fmt_number(
      columns = c(mean_ppt, sd_ppt, median_ppt, min_ppt, max_ppt, mean_naming, 
                  sd_naming, median_naming, min_naming, max_naming
      ),
      decimals = 1
    ) %>%
    tab_header(
      title = "Naming and Semantic association test scores"
    )  %>%
    tab_footnote(
      footnote = 'Test scores given in percentages',
      locations = cells_column_spanners(c("Pyramids and Palm Trees Test", "Philadelphia Naming Test"))
    ))
table_descriptive_subset
#-----------------------------------------------------------------------#
## number of semantic errors that were manualy coded as 
## Coordinates but were also associates in swow: 
coordinate_swow_subset<-model_data_subset %>% 
  filter(Recoding == "CO" & associative_swow == 1) %>% 
  group_by(Test_word)
## percentage 35.75%
round(237*100/663,2)

## number of semantic errors that were associates (non-coordinates)
## according to swow:
associates_non_coordinates_swow_subset<-model_data_subset %>% 
  filter(Recoding != "CO" & associative_swow == 1) %>% 
  group_by(Test_word)
## percentage 18.55%
round(123*100/663,2)

## number of semantic errors that were not associates 
## according to swow: 
non_associates_swow_subset<-model_data_subset %>% 
  filter(associative_swow == 0) %>% 
  group_by(Test_word)
## percentage 47.62%
round(303*100/663,2)


## 3.7 Agreements and disagreements sets of error coding ####
all_target_error_combinations <- model_data_subset %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(!(Recoding == "CO" & associative_swow == 1)) %>% 
  unique()
write.xlsx(all_target_error_combinations, "agreement and disagreements set.xlsx")

test<-model_data %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter((Recoding == "CO" & associative_swow == 1)) %>% 
  unique()

associative_agreement_set<-all_target_error_combinations %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(Recoding == "A" & associative_swow == 1) %>% 
  unique()

non_associative_agreement_set<-all_target_error_combinations %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(Recoding != "A" & associative_swow == 0) %>% 
  unique()

disagreement_set<-model_data %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(Recoding != "A" & associative_swow == 1) %>% 
  unique()

disagreement_set2<-all_target_error_combinations %>% 
  select(Test_word, Regular_response, Recoding, associative_swow, R1.Strength) %>% 
  filter(Recoding != "A" & associative_swow == 1) %>% 
  unique()

#-----------------------------------------------------------------------#
#### 3 Modelling ####

#### 3.1 Analysis 1 ####
## Model SG1.MC ####
subset_glm_recoding_full_model <- glmer(associative_recoding ~ scaled_pyramids + 
                                          (1|participant) + (1|Test_word),
                                        data = model_data_subset,
                                        family = binomial)

summary(subset_glm_recoding_full_model)


subset_glm_recoding_test_words <- glmer(associative_recoding ~ scaled_pyramids + 
                                          (1|Test_word), data = model_data_subset,
                                        family = binomial)

summary(subset_glm_recoding_test_words)

subset_glm_recoding_participant <- glmer(associative_recoding ~ scaled_pyramids + 
                                           (1|participant), data = model_data_subset,
                                         family = binomial)
summary(subset_glm_recoding_participant)

## Table S6 ####
tab_model(subset_glm_recoding_test_words, 
          subset_glm_recoding_participant,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Model SG1.AC - Items as Random Effects", 
                        "Model SG1.AC - Participants as Random Effects"), 
          p.style = "numeric", 
          file = "TableS6.html")

round(anova(subset_glm_recoding_full_model, 
            subset_glm_recoding_test_words),3)
round(anova(subset_glm_recoding_full_model, 
            subset_glm_recoding_participant),3)
anova(subset_glm_recoding_full_model,
      subset_glm_recoding_participant,
      subset_glm_recoding_test_words)

plot(allEffects(subset_glm_recoding_full_model))

## Model SG1.AC ####
subset_glm_swow_full_model <- glmer(associative_swow ~ scaled_pyramids + 
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset_no_coordinates,
                                    family = binomial)

summary(subset_glm_swow_full_model)

subset_glm_swow_test_words <- glmer(associative_swow ~ scaled_pyramids + 
                                      (1|Test_word), 
                                    data = model_data_subset_no_coordinates,
                                    family = binomial)

summary(subset_glm_swow_test_words)

subset_glm_swow_participant <- glmer(associative_swow ~ scaled_pyramids + 
                                       (1|participant), 
                                     data = model_data_subset_no_coordinates,
                                     family = binomial)
summary(analysis1_swow_participant)

## Table S7 ####
tab_model(subset_glm_swow_test_words, 
          subset_glm_swow_participant,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Model SG1.AC - Items as Random Effects", 
                        "Model SG1.AC - Participants as Random Effects"), 
          p.style = "numeric", 
          file = "TableS7.html")

round(anova(subset_glm_swow_full_model, 
            subset_glm_swow_test_words),3)

round(anova(subset_glm_swow_full_model, 
            subset_glm_swow_participant),3)

plot(allEffects(subset_glm_swow_full_model))

plot(residuals(subset_glm_swow_full_model))
plot(residuals(subset_glm_swow_full_model))

# Table 4 #### 
tab_model(subset_glm_recoding_full_model, 
          subset_glm_swow_full_model,
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          show.aic = TRUE,
          pred.labels = c("Intercept", "Pyramids and Palm Trees"),
          dv.labels = c("Manual recoding (Model A1.1) ", "Associative corpus (Model A1.2)"), 
          p.style = "numeric", 
          file = "subset analysis 1.html")

### 4.2 Analysis 2 ####
## Category Cohort Size, Associative and PPT strength as predictors

# Model SG2.MC ####
options(na.action = "na.omit")

subset_analysis2_recoding0 <- glmer(associative_recoding ~  
                                      1 +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_recoding0)

subset_analysis2_recoding1 <- glmer(associative_recoding ~ scaled_pyramids*  
                                      scaled_category_cohort_size* 
                                      scaled_strength_associates +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_recoding1)
plot(allEffects(recoding1))

## simplyfing model: step 1: remove 3-way interaction 
subset_analysis2_recoding2 <- glmer(associative_recoding ~ scaled_pyramids*scaled_category_cohort_size +  
                                      scaled_pyramids*scaled_strength_associates +
                                      scaled_category_cohort_size*scaled_strength_associates +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_recoding2)

anova(subset_analysis2_recoding1, subset_analysis2_recoding2)

## simplyfing model: step 2: remove least significant 2-way interaction scaled_pyramids:scaled_category_cohort_size
subset_analysis2_recoding3 <- glmer(associative_recoding ~  
                                      scaled_pyramids*scaled_strength_associates +
                                      scaled_category_cohort_size * scaled_strength_associates +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(analysis2_recoding3) 
anova(subset_analysis2_recoding2, subset_analysis2_recoding3)

## simplyfing model: step 3: remove second least significant 2-way interaction scaled_pyramids:scaled_category_cohort_size
subset_analysis2_recoding4 <- glmer(associative_recoding ~  
                                      scaled_pyramids +
                                      scaled_category_cohort_size * scaled_strength_associates +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_recoding4) ##FINAL MODEL
anova(subset_analysis2_recoding4, subset_analysis2_recoding3)

#Figure 1B ####
plot_model(subset_analysis2_recoding5, 
           type="int", 
           mdrt.values =  "meansd")

interaction2<- plot_model(subset_analysis2_recoding5, type="int",
                          mdrt.values =  "meansd",
                          axis.title = c("Category Cohort Size", "Probability of an associative error"),
                          title = " ",
                          legend.title = "Strength of Associates"
) +
  aes(linetype=group, color=group) + 
  labs(linetype = "Strength of Associates", color = "Strength of Associates")

interaction2
?plot_model

# Figure 1 - complete A & B ####
cowplot::plot_grid(interaction1,
                   interaction2,
                   labels = "AUTO")

# table all models analysis 2 subgroup manual recoding
tab_model(subset_analysis2_recoding1,
          subset_analysis2_recoding2,
          subset_analysis2_recoding3,
          subset_analysis2_recoding4,
          subset_analysis2_recoding5,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density",
                          "Pyramids and Palm Trees:Strength of Associates",
                          "Semantic Neighbourhood Density:Strength of Associates",
                          "Pyramids and Palm Trees:Semantic Neighbourhood Density:Strength of Associates",
                          "Strength of Associates: Semantic Neighbourhood Density"),
          dv.labels = c("Manual recoding - Subgroup - Model 1 ",
                        "Manual recoding - Subgroup - Model 2",
                        "Manual recoding - Subgroup - Model 3",
                        "Manual recoding - Subgroup - Model 4"), 
          p.style = "numeric_stars", 
          file = "Analysis 2 - subgroup - manual recoding.html")


# Model SG2.AC ####

subset_analysis2_swow0 <- glmer(associative_swow ~  
                                  1 +
                                  (1|participant) + (1|Test_word), 
                                data = model_data_subset_no_coordinates,
                                family = binomial,
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_swow0)

subset_analysis2_swow1 <- glmer(associative_swow ~ 
                                  scaled_category_cohort_size*
                                  scaled_strength_associates*scaled_pyramids + 
                                  (1|participant) + (1|Test_word), 
                                data = model_data_subset_no_coordinates,
                                family = binomial, 
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis2_swow1)
set_theme(base = theme_bw())

# Figure 2 - 3 way interaction ####
figure2 <- plot_model(subset_analysis2_swow1,
                      type="int",
                      mdrt.values =  "meansd",
                      axis.title = c("Category Cohort Size", "Probability of an associative error"),
                      title = " ",
                      legend.title = "Strength of Associates" 
) 
figure2 [[4]] +
  aes(linetype=group, color=group, labels = "AUTO") +
  labs(linetype = "Strength of Associates",
       color = "Strength of Associates",
       labels = c("A", "B", "C")) 

?plot_model

# Table 6 ####
tab_model(subset_analysis2_recoding4,
          subset_analysis2_swow1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = c("Model SG2.MC",
                        "Model SG2.AC"), 
          p.style = "numeric_stars", 
          file = "Analysis 2 - subgroup.html")
round(vif(subset_analysis2_recoding5),2)
round(vif(subset_analysis2_swow1),2)

### Analysis 3 - control variables ####
options(na.action="na.omit")
## best fit model recoding
subset_analysis3_recoding1 <- glmer(associative_recoding ~ scaled_pyramids +
                                      scaled_category_cohort_size * scaled_strength_associates +
                                      scaled_aoa +
                                      scaled_familiarity +
                                      scaled_imageability +
                                      scaled_frequency +
                                      scaled_name_agreement +
                                      scaled_visual_complex +
                                      (1|participant) + (1|Test_word), 
                                    data = model_data_subset,
                                    family = binomial,
                                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis3_recoding1)
round(vif(subset_analysis3_recoding1),2)
tab_model(subset_analysis3_recoding1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          dv.labels = "Manual recoding - best fit model with added control variables", 
          p.style = "numeric_stars", 
          file = "Analysis 3 - sub group recoding - best fit model with added control variables.html")


## best fit model swow
subset_analysis3_swow1 <- glmer(associative_swow ~
                                  scaled_pyramids *
                                  scaled_category_cohort_size *
                                  scaled_strength_associates +
                                  scaled_aoa +
                                  scaled_familiarity +
                                  scaled_imageability +
                                  scaled_frequency +
                                  scaled_name_agreement +
                                  scaled_visual_complex +
                                  (1|participant) + (1|Test_word), 
                                data = model_data_subset_no_coordinates,
                                family = binomial, 
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(subset_analysis3_swow1)
round(vif(subset_analysis3_swow1),2)
tab_model(subset_analysis3_swow1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Pyramids and Palm Trees",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity",
                          "Pyramids and Palm Trees: Semantic Neighbourhood Density",
                          "Pyramids and Palm Trees: Strength of Associates",
                          "Semantic Neighbourhood Density: Strength of Associates",
                          "Pyramids and Palm Trees: Semantic Neighbourhood Density: Strength of Associates"),
          dv.labels = "Manual recoding -subset -  best fit model with added control variables", 
          p.style = "numeric_stars", 
          file = "Analysis 3 - sub group recoding - best fit model with added control variables.html")

# table of best models with added control variables
tab_model(subset_analysis3_recoding1,
          subset_analysis3_swow1,
          show.se = TRUE,  
          show.stat = TRUE,
          show.aic = TRUE,
          show.p = TRUE,
          show.est = TRUE,
          pred.labels = c("Intercept",
                          "Semantic Neighbourhood Density",
                          "Strength of Associates",
                          "Age of Acquisition",
                          "Familiarity",
                          "Imageability",
                          "Lexical Frequency",
                          "Name Agreement",
                          "Visual Complexity",
                          "Semantic Neighbourhood Density: Strength of Associates",
                          "Pyramids and Palm Trees",
                          "Pyramids and Palm Trees: Semantic Neighbourhood Density",
                          "Pyramids and Palm Trees: Strength of Associates",
                          "Pyramids and Palm Trees: Semantic Neighbourhood Density: Strength of Associates"),
          dv.labels = c("Model A2.7",
                        "Model A2.8"), 
          p.style = "numeric_stars", 
          file = "sub group - best fit models with added control variables.html")

#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
## 3.1 FULL GROUP DEMOGRAPHIC INFORMATION AND FIGURES #### 

## Table 1 Participant Characteristics ####
table1_data <- model_data %>% group_by(participant) %>% 
  summarise(Age = mean (Age_when_tested, na.rm = TRUE),
            Education = mean (Education_level, na.rm = TRUE),
            Months.post.onset = mean(Months_post_onset, na.rm = TRUE),
            WAB_Aphasia_Quotient = mean(WAB_Aphasia_Quotient, na.rm = TRUE),
            Naming = mean(naming_total),
            pyramids = mean(PPT)) 
table1<-describe(table1_data)
table1 <- round(table1)
table1<- table1 %>% gt(table1) 
table1

naming_ppt <- data %>% 
  mutate(naming_total = naming_total * 100/175) %>% 
  summarise(n = n_distinct(participant),
            mean_naming = mean(naming_total),
            sd_naming = sd(naming_total),
            median_naming = median(naming_total),
            min_naming = min (naming_total),
            max_naming = max(naming_total),
            median_naming = median(naming_total),
            mean_ppt = mean(PPT*52/100),
            sd_ppt = sd(PPT*52/100),
            median_ppt = median(PPT*52/100),
            min_ppt = min (PPT*52/100),
            max_ppt = max(PPT*52/100),
            median_ppt = median(PPT*52/100)
  )

naming_raw_data<-data %>% group_by(participant) %>% summarise(mean(naming_total))
describe(naming_raw_data)

ppt_raw_data <- data %>% group_by(participant) %>% summarise(PPT = mean(PPT*52/100))
describe(ppt_raw_data)

(table3 <- naming_ppt %>% gt(naming_ppt) %>%
    tab_spanner(
      label = "Pyramids and Palm Trees Test",
      columns = c(mean_ppt, sd_ppt, median_ppt, min_ppt, max_ppt)
    ) %>%
    cols_merge(
      columns = c(mean_ppt, sd_ppt),
      hide_columns = c(sd_ppt),
      pattern = "{1} ({2})"
    ) %>%
    cols_merge(
      columns = c(min_ppt, max_ppt),
      hide_columns = c(max_ppt),
      pattern = "{1} - {2}"
    ) %>%
    cols_label(
      mean_ppt = "Mean (SD)",
      min_ppt = "Range",
      median_ppt = "Median"
    ) %>%
    tab_spanner(
      label = "Philadelphia Naming Test",
      columns = c(mean_naming, sd_naming, median_naming, min_naming, max_naming)
    ) %>%
    cols_merge(
      columns = c(mean_naming, sd_naming),
      hide_columns = c(sd_naming),
      pattern = "{1} ({2})"
    ) %>%
    cols_merge(
      columns = c(min_naming, max_naming),
      hide_columns = c(max_naming),
      pattern = "{1} - {2}"
    ) %>%
    cols_label(
      mean_naming = "Mean (SD)",
      min_naming = "Range",
      median_naming = "Median"
    ) %>%
    fmt_number(
      columns = c(mean_ppt, sd_ppt, median_ppt, min_ppt, max_ppt, mean_naming, 
                  sd_naming, median_naming, min_naming, max_naming
      ),
      decimals = 1
    ) %>%
    tab_header(
      title = "Naming and Semantic association test scores"
    )  %>%
    tab_footnote(
      footnote = 'Test scores given in percentages',
      locations = cells_column_spanners(c("Pyramids and Palm Trees Test", "Philadelphia Naming Test"))
    ))
table3
gt::gtsave(data = table3, filename = 'table3.rtf')

## Figure 2 ####
#Distribution of naming accuracy as a function of Pyramids and Palm Trees

interaction1
(figure2 <- data %>% 
    mutate(naming_total = naming_total * 100/175) %>% 
    ggplot(aes(naming_total, PPT)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name="Naming") +
    scale_y_continuous(name="Pyramids and Palm Trees Test") +
    xlab("Philadelphia Naming Test") +
    ylab("Pyramids and Palm Trees Test") +
    ggsave("Figure2.png"))

#count number of associative errors per participant
data$associative <-  ifelse(data$Recoding == 'A', 1, 0)
associative_total<- data %>% 
  group_by (participant) %>%
  summarise(associates_total = sum(associative))

data <- merge(data,associative_total,
              by.x = c('participant'),
              by.y = c('participant'))

#calculate proportion of semantic errors subtypes per participant
individual_semantic_errors_proportions <- xtabs(formula = ~participant + Recoding, data=data)
individual_semantic_errors_proportions <- round(prop.table(individual_semantic_errors_proportions, margin=1), digits=2)
individual_semantic_errors_proportions <- data.frame(individual_semantic_errors_proportions)
head(individual_semantic_errors_proportions)

# same for subset
individual_semantic_errors_proportions <- xtabs(formula = ~participant + Recoding, data=model_data_subset)
individual_semantic_errors_proportions <- round(prop.table(individual_semantic_errors_proportions, margin=1), digits=2)
individual_semantic_errors_proportions <- data.frame(individual_semantic_errors_proportions)
head(individual_semantic_errors_proportions)

#calculate associative and coordinate ratio
associative_coordinate_ratio_data <- data %>%
  filter(Recoding == c("A", "CO")) 
associative_coordinate_ratio <- xtabs(formula = ~ participant + Recoding, data=associative_coordinate_ratio)
associative_coordinate_ratio <- round(prop.table(associative_coordinate_ratio, margin=1), digits=2)
associative_coordinate_ratio <- data.frame(associative_coordinate_ratio)



## Table 2 Percentage of all semantic errors  ####

(table2<- individual_semantic_errors_proportions %>% 
   group_by(Recoding) %>% 
   summarise(Proportion = mean(100*Freq))%>%
   gt()%>% 
   fmt_number(c(Proportion),decimals = 1))

gt::gtsave(data = table2, filename = 'table2.rtf')


(associative_proportions<-individual_semantic_errors_proportions %>% 
    filter(Recoding == "A") %>% 
    group_by (participant) %>%
    summarise(associates_prop = sum(100*Freq)))


(shared_feature_proportions <- individual_semantic_errors_proportions %>%
    filter(Recoding == c("SP", "CO")) %>% 
    group_by(participant) %>%
    summarise(shared_feature_prop = sum(100*Freq)))


(coordinate_proportions <- individual_semantic_errors_proportions %>%
    filter(Recoding == "CO") %>% 
    group_by(participant) %>%
    summarise(coordinate_prop = sum(100*Freq)))

### THE END 
