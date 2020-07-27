library(tidyverse)
library(estimatr)
library(modelr)
library(ipumsr)
library(RStata)
library(rio)
library(janitor)
library(tidylog) 
library(RColorBrewer)
library(srvyr)


#https://www.nber.org/papers/w27494.pdf

psid_family_raw <- import("~/Dropbox/Potential-projects/hh_size_covid/psid/fam2017er/psid_family_file_17.dta")
psid_ind_raw <- import("~/Dropbox/Potential-projects/hh_size_covid/psid/fam2017er/psid_individual_file_17.dta")


#The FU is defined as a group of people living together as a family. 
#They are almost always related by blood, marriage, or adoption. 
#And they must all be living in the same HU . 

#Adding in weights: ER71571 or ER71570 depending on whether I'm including the immigration sample

## Weights for PSID
# The Sampling Error Stratum variable (ER31996) may be specified as the "Stratum variable" in the design specification 
#Sampling Error Cluster variable (ER31997) may be specified as the "Cluster Variable".  (Don't think I have this in the data?)



psid_family <- psid_family_raw %>% mutate(number_in_family_unit=ER66016,
                number_non_family_in_house=ER66023,
                number_rooms=ER66029,
                senior_housing=ER66027, #require at least one resident age 55+
                weight_family_sample=ER71571,
                immigrant_sample=ER70979,
                race_rp=ER70882,
                hispanic_rp=ER70881,
                race_spouse=ER70744,
                hispanic_spouse=ER70743, 
                family_id=ER66002,
                age_rp=ER66017,
                heart_attack_rp=ER68433, ## all 1 yes 5 no 8 dk 9 na/refused
                heart_diesese_rp=ER68439,
                high_blood_pressure_rp=ER68444,
                asthma_rp=ER68449,
                lung_disease_rp=ER68454,
                diabetes_rp=ER68459,
                treatment_for_cancer_rp=ER68481, #1 yes, everything else no
                smoker_rp=ER68555,
                other_condition_rp=ER68498, # 3 kidney disese 7 autoimune
                weight_rp_pounds=ER68566, #top coded at 400, 998 DK 999 NA/refused, 0 answered in kilos
                weight_rp_kilos=ER68567,
                height_feet_rp=ER68568,
                height_inches_rp=ER68569,#1 yes
                height_meters_rp=ER68570,
                age_spouse=ER66019,
                heart_attack_spouse=ER69560,#+1127
                heart_diesese_spouse=ER69566,
                high_blood_pressure_spouse=ER69571,
                asthma_spouse=ER69576,
                lung_disease_spouse=ER69581,
                diabetes_spouse=ER69586,
                treatment_for_cancer_spouse=ER69608, #1 yes, everything else no
                smoker_spouse=ER69682,
                other_condition_spouse=ER69625,
                weight_spouse_pounds=ER69693, #top coded at 400, 998 DK 999 NA/refused, 0 answered in kilos
                weight_spouse_kilos=ER69694,
                height_feet_spouse=ER69695,
                height_inches_spouse=ER69696,#1 yes
                height_meters_spouse=ER69697) %>% 
  select(number_in_family_unit:height_meters_spouse)

#Making health variables
psid_family <- psid_family %>% 
  mutate(number_rooms=ifelse(number_rooms>20,NA,number_rooms),
         number_rooms=number_rooms+1, #0 for family unit shares a room (from codebook)
         total_in_hh=number_in_family_unit+number_non_family_in_house,
         no_spouse=ifelse(age_spouse==0,1,0)) %>% 
  mutate_at(.vars = vars(age_rp,age_spouse),
            list(~ifelse((.==999|0),NA,.))) %>%
  mutate_at(.vars = vars(hispanic_rp,hispanic_spouse),
            list(~ifelse(.>0 & .!=9,"Hispanic","Not Hispanic"))) %>% #getting hispanic wrong here
  mutate(race_rp=ifelse(hispanic_rp=="Hispanic","Hispanic",
                        ifelse(race_rp==1,"White",
                               ifelse(race_rp==2,"Black","Other")))) %>% 
  mutate(race_spouse=ifelse(hispanic_spouse=="Hispanic","Hispanic",
                        ifelse(race_spouse==1,"White",
                               ifelse(race_spouse==2,"Black","Other")))) 


psid_family <- psid_family %>% mutate_at(.vars = vars(heart_attack_rp:smoker_rp), #This might fuck up my kidney and autoimmune
              list(~ifelse(.==1,1,0))) %>% 
  mutate_at(.vars = vars(heart_attack_spouse:smoker_spouse), #Can I do multiple lists?
            list(~ifelse(.==1,1,0))) %>% 
  mutate_at(.vars = vars(weight_rp_kilos,weight_spouse_kilos), #Converting kilos to points
            list(~.*2.20462)) %>% 
  mutate(weight_rp_pounds=ifelse(weight_rp_pounds>400,NA, #Cleaning up pounds var
                                 ifelse(weight_rp_pounds==0,weight_rp_kilos,weight_rp_pounds)),
         weight_spouse_pounds=ifelse(weight_spouse_pounds>400,NA,
                                     ifelse(weight_spouse_pounds==0,weight_spouse_kilos,weight_spouse_pounds)),
         height_feet_rp=ifelse(height_feet_rp>=8,NA,
                               ifelse(height_feet_rp==0,height_meters_rp*3.28084,height_feet_rp)),
         height_feet_spouse=ifelse(height_feet_spouse>=8,NA,
                               ifelse(height_feet_spouse==0,height_meters_spouse*3.28084,height_feet_spouse))) %>%
  mutate_at(.vars = vars(height_inches_rp,height_inches_spouse), 
            list(~ifelse(.>11,NA,.))) %>% 
  mutate(kidney_disease_rp=ifelse(other_condition_rp==3,1,0),
         kidney_disease_spouse=ifelse(other_condition_spouse==3,1,0),
         autoimmune_disorder_rp=ifelse(other_condition_rp==7,1,0),
         autoimmune_disorder_spouse=ifelse(other_condition_spouse==7,1,0)) %>% 
  mutate(height_rp=(height_feet_rp*12)+height_inches_rp,
         height_spouse=(height_feet_spouse*12)+height_inches_spouse,
         bmi_rp=weight_rp_pounds/(height_rp^2)*703,
         bmi_spouse=weight_spouse_pounds/(height_spouse^2)*703,
         obese_rp=ifelse(bmi_rp>40,1,0), #cdc says severe obesity bmi>40
         obese_spouse=ifelse(bmi_spouse>40,1,0))


#Setting up individual file
psid_ind <- psid_ind_raw %>% 
  as_tibble() %>% 
  mutate(family_id=ER34501, age=ER34504, ind_weight=ER34651,
         relation_to_rp=ER34503, work_status=ER34516, seq_number=ER34502,
         sex=ER32000) %>% #Sex is new
  select(family_id:sex) %>% 
  mutate(female=ifelse(sex==2,1,0),
         age=ifelse(age==999,NA,age),
         age=ifelse(age==0,NA,age),
         currently_working=ifelse(work_status==1,1,0),
         reference_person=ifelse(relation_to_rp==10,1,0),
         spouse=ifelse((relation_to_rp==20|relation_to_rp==22),1,0)) %>% 
  filter(seq_number!=0) # Select all individuals whose Sequence
# Number for the desired year is non-zero  and match the family Interview Number for that year from the individual file with the family Interview
# Number from the corresponding family file


#Joining individual and family files
psid_joined <- full_join(psid_ind,psid_family, by = "family_id")

#Double checking data structure seems right
psid_joined %>% as_tibble() %>% 
  arrange(family_id) %>% select(spouse,reference_person,family_id) 
psid_joined %>% as_tibble() %>% filter(number_in_family_unit>2) %>% 
  arrange(family_id) %>% select(spouse,reference_person,family_id) 

#Want to get ages in the family
psid_joined <- psid_joined  %>% 
  group_by(family_id) %>% 
  summarise(max_age=max(age), min_age=min(age), median_age=median(age)) %>% 
  left_join(psid_joined) 

#Total preexisting conditions susceptible to covid
psid_joined <- psid_joined %>% 
  mutate(above_age_65_rp=ifelse(age_rp>=65,1,0),
         above_age_65_spouse=ifelse(age_spouse>=65,1,0),
         prexisting_cond_rp_total=heart_attack_rp+heart_attack_rp+heart_diesese_rp+
           high_blood_pressure_rp+asthma_rp+diabetes_rp+treatment_for_cancer_rp+
           smoker_rp+obese_rp+above_age_65_rp,
         prexisting_cond_spouse_total=heart_attack_spouse+heart_attack_spouse+heart_diesese_spouse+
           high_blood_pressure_spouse+asthma_spouse+diabetes_spouse+
           treatment_for_cancer_spouse+smoker_spouse+obese_spouse+above_age_65_spouse,
         prexisting_cond_rp_total_health_only=heart_attack_rp+heart_attack_rp+heart_diesese_rp+
           high_blood_pressure_rp+asthma_rp+diabetes_rp+treatment_for_cancer_rp+
           smoker_rp+obese_rp,
         prexisting_cond_spouse_total_health_only=heart_attack_spouse+heart_attack_spouse+heart_diesese_spouse+
           high_blood_pressure_spouse+asthma_spouse+diabetes_spouse+
           treatment_for_cancer_spouse+smoker_spouse+obese_spouse,
         prexisting_cond_rp_binary=ifelse(prexisting_cond_rp_total>1,1,0),
         prexisting_cond_rp_binary_health_only=ifelse(prexisting_cond_rp_total_health_only>1,1,0),
         prexisting_cond_spouse_binary=ifelse(prexisting_cond_spouse_total>1,1,0))

#Change all spouse variables to 0 if NA, need to filter by no spouse if I want to restrict the data
psid_joined <- psid_joined %>% mutate_at(vars(contains("_spouse")),
                          list(~ifelse(is.na(.) & no_spouse==1,0,.)))
psid_joined <- psid_joined %>%
         mutate(prexisting_cond_spouse_binary_health_only=ifelse(prexisting_cond_spouse_total_health_only>1,1,0),
         prexisting_rp_spouse_combo=prexisting_cond_rp_total+prexisting_cond_spouse_total, #What am I doing about NAs here
         prexisting_rp_spouse_combo_health_only=prexisting_cond_rp_total_health_only+
           prexisting_cond_spouse_total_health_only,
         prexisting_rp_spouse_combo_binary=ifelse(prexisting_rp_spouse_combo>1,1,0),
         prexisting_rp_spouse_combo_binary_health_only=ifelse(prexisting_rp_spouse_combo_health_only>1,1,0))

#Making the cdc conditions
psid_joined <- psid_joined %>% mutate(overall_num_conditions_rp=heart_attack_rp+heart_diesese_rp+high_blood_pressure_rp+asthma_rp+diabetes_rp+
         treatment_for_cancer_rp+smoker_rp+obese_rp+kidney_disease_rp+autoimmune_disorder_rp,
       one_condition_plus_rp=ifelse(overall_num_conditions_rp>=1,1,0),
       overall_num_conditions_spouse=heart_attack_spouse+heart_diesese_spouse+high_blood_pressure_spouse+asthma_spouse+diabetes_spouse+
                treatment_for_cancer_spouse+smoker_spouse+obese_spouse+kidney_disease_spouse+autoimmune_disorder_spouse,
              one_condition_plus_spouse=ifelse(overall_num_conditions_spouse>=1,1,0)) 




#Want to make dataset out of spouse and rp with health conditions, one long dataset rather than 
#spouse and rp as different variables in one row
#The psid_joined has that, but difference variable names for spouse and rp

psid_joined_rp <- psid_joined %>% filter(reference_person==1) %>% select(-contains("_spouse"))
psid_joined_rp <- psid_joined_rp %>% select(-age_rp) #Don't want multiple age cols
names(psid_joined_rp) <- names(psid_joined_rp) %>% str_remove("_rp")


psid_joined_spouse <- psid_joined %>% filter(spouse==1) %>% select(-contains("_rp"))
psid_joined_spouse <- psid_joined_spouse %>% select(-age_spouse) #Don't want multiple age cols
names(psid_joined_spouse) <- names(psid_joined_spouse) %>% str_remove("_spouse")

psid_joined_rp_spouse <- bind_rows(psid_joined_rp,psid_joined_spouse) 


psid_joined_rp_spouse <- psid_joined_rp_spouse %>%
  mutate(age_2=age^2,age_3=age^3) #Need to add race_rp to thus
psid_joined_race_rp <- psid_joined %>% #Just getting race_rp
  select(race_rp,family_id) %>% distinct()

psid_joined_rp_spouse <- left_join(psid_joined_rp_spouse,psid_joined_race_rp, by = "family_id") #adding race_rp

psid_joined_rp_spouse %>% arrange(family_id) %>% 
  select(spouse,reference_person,family_id) #Testing that I'm not repeating things


#Now I want to apply the predictions to the people who are not spouses or rps, 
#so I need to subset the data to them and make sure I have the same age vars
psid_joined_other_family <- psid_joined %>% filter(reference_person!=1 & spouse!=1) %>% 
  mutate(age_original=age, age=ifelse(age<18,18,age), #Since I have children of 18 year old heads of household, I'm gonna pretend they are 18 for the purposes of this
         age_2=age^2,age_3=age^3) %>% select(-contains(c("_spouse", "_rp")))
names(psid_joined_other_family) #Need to add race_rp to this
psid_joined_race_rp <- psid_joined %>% select(race_rp,family_id) %>% distinct()
psid_joined_other_family <- left_join(psid_joined_other_family,psid_joined_race_rp, by = "family_id")

#Maybe should just impute children as 0 for the conditions?


names(psid_joined_rp_spouse)

var_list <- c(names(psid_joined_rp_spouse)[23:30],names(psid_joined_rp_spouse)[38:39],names(psid_joined_rp_spouse)[42])
var_list

lbl <- function(...) {ensyms(...) %>% reduce(str_c) %>% sym}
#Could add geography here
for(i in var_list) {
  L1 <- lbl({{i}} ) #I think this works to replace original variable name..
  m1 <- lm(substitute(i ~ race_rp + age + age_2 + age*race_rp + age_2*race_rp + 
                        female + age*female + age_2*female + race_rp*female, list(i = as.name(i))), 
           data = psid_joined_rp_spouse)
  psid_joined_other_family <- psid_joined_other_family %>% add_predictions(m1) %>% 
    rename(!!L1:=pred)
}



#Now want to join with rp and spouse data to have it all, then weight
psid_joined_all <- bind_rows(psid_joined_rp_spouse,psid_joined_other_family)

#Decided to assume children have no conditions, but maybe I should do it condition by condition, getting rid of smoking?
psid_joined_all <- psid_joined_all %>% mutate(across(all_of(var_list),~ifelse(age<=18,0,.x))) 

psid_joined_all_weights <- psid_joined_all %>% as_survey_design(weights = ind_weight)


#Looking at adult population conditions accuracy
psid_joined_all_weights %>% 
  filter(age>18) %>% 
  select(var_list) %>% 
  summarise_all(survey_mean, na.rm=TRUE) %>% 
  pivot_longer(heart_attack:obese_se) %>% 
  filter(!grepl('_se', name)) 

#https://www.diabetesresearch.org/diabetes-statistics 10.2% diabetes
#Obsese is like mobid not normal obese going off CDC definition
#14% smokers #https://www.cnbc.com/2018/11/08/cdc-says-smoking-rates-fall-to-record-low-in-us.html
#Asthma is a little high, 1/13 vs 11%
psid_joined_all_weights %>% summarise(survey_mean(num_family_in_data)) #Avg family size 3.3 people

# ###
# psid_joined_all <- psid_joined_all %>% group_by(family_id) %>% count() %>% 
#   rename(num_family_in_data=n) %>% right_join(psid_joined_all) %>% 
#   mutate(num_in_family_the_same=num_family_in_data-number_in_family_unit,
#          diff_number=ifelse(num_in_family_the_same!=0,1,0)) %>% 
#   ungroup()
# 
# psid_joined_all %>% summarise(mean(num_in_family_the_same)) #Not great... But could be error in the answer to that question?
# psid_joined_all %>% summarise(mean(diff_number)) #Not great... But could be error in the answer to that question?
# 

#Family level sums
family_level_total_conds <- psid_joined_all %>% group_by(family_id,race_rp) %>% #Race_rp is constant within family so just keeping extra var for later
  summarise(across(all_of(var_list), 
         sum, .names = "sum_{col}")) 
family_level_total_conds


#age 65+ first
above_age_65_family <- psid_joined_all %>% mutate(above_age_65=ifelse(age>=65,1,0)) %>% 
  group_by(family_id) %>% 
  summarise(sum_above_age_65=sum(above_age_65)) %>% 
  mutate(above_age_65=ifelse(sum_above_age_65>=1,1,0)) 

#Adding age 65+ into the dataframe
family_level_total_conds <- family_level_total_conds %>% left_join(above_age_65_family)

#Want binary if conditaion is in family or not
family_level_total_conds <- family_level_total_conds %>% mutate(across(starts_with("sum"), ~ifelse(.x>=1,1,.x)))


#At least 1 condition
family_level_total_conds <- family_level_total_conds %>% mutate(total_conditions=sum_heart_attack+sum_heart_diesese+
                                                                  sum_high_blood_pressure+sum_asthma+sum_diabetes+sum_treatment_for_cancer+
                                                                  sum_smoker+sum_obese+sum_above_age_65,
                                                                sum_one_condition_plus=ifelse(total_conditions>=1,1,0))


#Want to go back to individual level

ind_level_total_condtions <- psid_ind %>% left_join(family_level_total_conds, by = "family_id") 
names(ind_level_total_condtions)


ind_level_total_condtions <- ind_level_total_condtions %>% select(-total_conditions)

ind_level_total_condtions <- ind_level_total_condtions %>% as_survey_design(weights = ind_weight)

ind_level_fam_total_race <- ind_level_total_condtions %>% group_by(race_rp) %>% 
  select(sum_heart_attack:sum_one_condition_plus) %>% 
  summarise_all(survey_mean, na.rm=TRUE) 



ind_level_fam_total_race <- ind_level_total_condtions %>% 
  select(sum_heart_attack:sum_one_condition_plus) %>% 
  summarise_all(survey_mean, na.rm=TRUE) %>% 
  mutate(race_rp="Overall") %>% 
  bind_rows(ind_level_fam_total_race) %>% 
  mutate(race_rp=as.factor(race_rp),
         race_rp=fct_relevel(race_rp, "Overall", "White", "Black", "Hispanic")) %>% 
  relocate(race_rp)
names(ind_level_fam_total_race)
ind_level_fam_total_race_long <- pivot_longer(ind_level_fam_total_race, cols = sum_heart_attack:sum_one_condition_plus_se)

ind_level_fam_total_race_long_ses <- ind_level_fam_total_race_long %>% filter(grepl('_se', name)) %>% mutate(name=str_remove(name, "_se")) %>% 
  rename(se=value) 

ind_level_fam_total_race_long <- ind_level_fam_total_race_long %>% 
  filter(!grepl('_se', name)) %>% 
  left_join(ind_level_fam_total_race_long_ses) %>% 
  mutate(clean_name=str_replace_all(name, "_"," "),
         clean_name=str_replace(clean_name, "sum ",""),
         clean_name=str_squish(clean_name),
         clean_name=str_to_title(clean_name),
         clean_name=str_replace_all(clean_name, " ","\n"))

#Not sure why this isn't working
#Need a thing for everything else e.g. when I don't change it
ind_level_fam_total_race_long <- ind_level_fam_total_race_long %>% 
  mutate(clean_name=case_when(clean_name=="One\nCondition\nPlus" ~ "At\nLeast\nOne Condition",
                              clean_name=="Treatment for Cancer" ~ "Treatment for Cancer",
                              clean_name=="Autoimmune\nDisorder" ~ "Autoimmune\nDisorder",
                              clean_name=="High\nBlood\nPressure" ~ "High\nBlood\nPressure",
                              TRUE ~ as.character(clean_name)),
         clean_name=fct_relevel(clean_name, "At\nLeast\nOne Condition", after = Inf))




ind_level_hh_risk <- ggplot(data=ind_level_fam_total_race_long %>% filter(race_rp!="Other"), aes(x=clean_name, y=value, fill = race_rp,
                                                                  ymin=value-(se*1.96), ymax=value+(se*1.96))) +
  theme_jack() +
  geom_bar( stat="identity", position = "dodge", width=0.5) +
  geom_errorbar(width = .08, position = position_dodge(0.5), color = "gray") +
  scale_y_continuous(limits = c(0,.8), n.breaks = 8, labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Prevalence of conditions vulnerable to Covid-19\namoung household members\n", x = "", 
       y = "Percetage\nof people\nwho live in a\nhousehold\nwith\nat least\none\nhousehold\nmember\nhas\ngiven\ncondition", fill = "",
       caption = "Data from PSID\nFor non heads of household or spouses,\nhealth conditions are imputed based on demographics\nOVERESTIMATES, see text for explanation") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=12),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_fill_brewer(palette = "Set2")
ind_level_hh_risk
png(filename = "/Users/jacklandry/Documents/GitHub/covid-blog/figures/ind_level_hh_risk.png", 
    width=11, height=5, units="in", res=1000)
ind_level_hh_risk
dev.off()



#Also above age 65 I can do manually...


conds_var_list <- names(family_level_total_conds)[2:20]


psid_joined_all <- psid_joined_all %>% 
  as_survey_design(weights = ind_weight)

psid_joined_all <- psid_joined_all %>% 
  as_survey_design(weights = ind_weight,
                   strata = strata)





everyone_summary_race <- psid_joined_all %>% filter(age>=18) %>% 
  mutate(race_rp=as.factor(race_rp)) %>% group_by(race_rp) %>% 
  select(var_list) %>% 
  summarise_all(survey_mean, na.rm=TRUE) %>% 
  mutate(race_rp=as.character(race_rp))



everyone_summary_overall <- psid_joined_all %>% 
  select(var_list) %>% 
  summarise_all(survey_mean, na.rm=TRUE) %>% mutate(race_rp="Overall") 

overall_health <- bind_rows(everyone_summary_overall,everyone_summary_race) %>% 
  mutate(race_rp=fct_relevel(race_rp, "Overall", "White", "Black", "Hispanic"))

#Make a graph out of this?
#Not doing it by race

overall_health_long <- overall_health %>% filter(race_rp=="Overall") %>% select(heart_attack:smoker_se) %>% 
  pivot_longer(cols = heart_attack:smoker_se)
overall_health_ses <- overall_health_long %>% filter(grepl('_se', name)) %>% mutate(name=str_remove(name, "_se")) %>% 
  rename(se=value) 

overall_health_long <- overall_health_long %>% 
  filter(!grepl('_se', name)) %>% 
  left_join(overall_health_ses) %>% 
  mutate(clean_name=str_replace_all(name, "_"," "),
         clean_name=str_replace(clean_name, "sum ",""),
         clean_name=str_squish(clean_name),
         clean_name=str_to_title(clean_name)) 

#Not sure why this isn't working
#Need a thing for everything else e.g. when I don't change it
overall_health_long <- overall_health_long %>% 
  mutate(clean_name=case_when(clean_name=="One Condition Plus" ~ "At Least\nOne Condition",
                              clean_name=="Treatment for Cancer" ~ "Treatment for Cancer",
                              clean_name=="Autoimmune Disorder" ~ "Autoimmune Disorder",
                              clean_name=="High Blood Pressure" ~ "High Blood\nPressure",
                              TRUE ~ as.character(clean_name)),
         clean_name=fct_relevel(clean_name, "At Least\nOne Condition", after = Inf))

ind_level_risk <- ggplot(data=overall_health_long, aes(x=clean_name, y=value, ymin=value-(se*1.96), ymax=value+(se*1.96))) +
  theme_jack() +
  geom_bar( stat="identity", position = "dodge", width=0.5) +
  geom_errorbar(width = .08, position = position_dodge(0.5), color = "gray") +
  scale_y_continuous(limits = c(0,.5), n.breaks = 5, labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Estimated Condition Prevelence\nFor Adult Population\n", x = "", 
       y = "Percentage\nwith\ncondition", fill = "",
       caption = "Data from PSID\nFor non heads of household or spouses,\nhealth conditions are imputed based on demographics") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=12),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_fill_brewer(palette = "Set2")
ind_level_risk
png(filename = "/Users/jacklandry/Documents/GitHub/covid-blog/figures/ind_level_risk.png", 
    width=6, height=4, units="in", res=1000)
ind_level_risk
dev.off()





#https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-at-higher-risk.html





  