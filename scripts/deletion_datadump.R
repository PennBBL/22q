

# Load Libraries 
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse) 
library(janitor)
library(knitr)
library(readxl)

monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }


#Set working directory and Import Data Sets 

# USE THIS WHEN WORKING ON DESKTOP
#setwd("/Users/mruganks/bblrepo1/bit_dwh")
setwd("/data/secure/bit_dwh")


#chop_data_raw <- read.csv("/Users/mruganks/bblrepo1/bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv") %>% clean_names() %>% rename_with(~ paste0("chop_", .x))
chop_data_raw <- read.csv("/data/secure/bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv") %>% clean_names() %>% rename_with(~ paste0("chop_", .x))

study_enroll_raw <- read.csv("oracle/subject_studyenrol.csv") %>%
  clean_names()%>%
  filter(study_status != "not enrolled")
study_enroll_raw$doenroll <- as.Date(as.character(study_enroll_raw$doenroll), format = "%m/%d/%Y")
  

subject_demos <- read.csv("oracle/subject.csv") %>%
  clean_names()
subject_demos$dointake <- as.Date(as.character(subject_demos$dointake), format = "%m/%d/%Y")
subject_demos$dobirth <- as.Date(as.character(subject_demos$dobirth), format = "%m/%d/%Y")


chop_data_clean <- chop_data_raw %>%
  select(bblid = chop_bbl_id, chop_vcfs_id, chop_chop_mrn, chop_deceased, chop_q_type, chop_q_brkpt,	chop_q_brkpt_oth, chop_inherited_denovo, contains("race"), contains("sex"), chop_ethnicity, chop_dob) %>%
  filter(chop_q_type == 1)



subject_22q<-subject_demos[subject_demos$bblid %in% chop_data_clean$bblid, ]%>%
  select(bblid, guid, sex, race, ethnic, dobirth)%>%
  left_join(chop_data_clean%>%select(bblid, chop_vcfs_id, chop_mrn = chop_chop_mrn, chop_q_brkpt))



enroll_22q<- study_enroll_raw[study_enroll_raw$bblid %in% subject_22q$bblid, ]

write.csv(enroll_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_all_enrolls.csv", row.names = F)




enroll_22q_grouped <- enroll_22q %>%
  group_by(bblid) %>%
  summarise(total_enrols = n_distinct(protocol, na.rm = TRUE))

subject_demos_22q<-left_join(subject_22q,enroll_22q_grouped, by = "bblid")




#write.csv(subject_demos_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_demographics_phi.csv", row.names = F)


subject_demos_22q_no_phi<-subject_demos_22q%>%
  mutate(current_age = floor(interval(dobirth, today()) / years(1)))%>%
  select(bblid,sex, race, ethnic, chop_q_brkpt, total_enrols )


write.csv(subject_demos_22q_no_phi,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_demographics_no_phi.csv", row.names = F)




# cnb


cnb<-read.csv("/data/secure/bit_dwh/cnb/cnb_accuracy_speed_commonsmryscores_allbbl_longform.csv")

cnb<-cnb%>%
  mutate(test_sessions.bblid = ifelse(is.na(test_sessions.bblid), test_sessions.subid, test_sessions.bblid))

cnb_22q<-cnb[cnb$test_sessions.bblid %in% subject_demos_22q$bblid, ]




#write.csv(cnb_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_cnb_phi.csv", row.names = F)



cnb_22q_non_phi<-cnb_22q%>%
  select(-test_sessions_v.dob)%>%
  mutate(test_sessions_v.dotest = format(
    as.Date((test_sessions_v.dotest)),
    "%Y-%m"
  ))



write.csv(cnb_22q_non_phi,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_cnb_no_phi.csv", row.names = F)



# sips

sips <- read.csv("oracle/sips.csv") %>%
  clean_names()
sips$dosips <- as.Date(as.character(sips$dosips), format = "%m/%d/%Y")

sips_22q<- sips[sips$bblid %in% subject_demos_22q$bblid, ]%>%
  group_by(bblid) %>%
  filter(
    if ("Combined" %in% type) {
      type == "Combined"
    } else if ("Proband" %in% type) {
      type == "Proband"
    } else {
      type == "Collateral"
    }
  ) %>%
  ungroup()

#write.csv(sips_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_sips_22q_phi.csv", row.names = F)

sips_22q_non_phi<-sips_22q%>%
  mutate(dosips = format(
    as.Date((dosips)),
    "%Y-%m"
  ))%>%
group_by(bblid) %>%
  filter(
    if ("Combined" %in% type) {
      type == "Combined"
    } else if ("Proband" %in% type) {
      type == "Proband"
    } else {
      type == "Collateral"
    }
  ) %>%
  ungroup()


write.csv(sips_22q_non_phi,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_sips_22q_no_phi.csv", row.names = F)





#meds



medication <- read.csv("oracle/medcollect.csv") %>%
  clean_names()
medication$docollect <- as.Date(as.character(medication$docollect), format = "%m/%d/%Y")

medication_22q<- medication[medication$bblid %in% subject_demos_22q$bblid, ]

#write.csv(medication_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_medication_22q_phi.csv", row.names = F)

medication_22q_non_phi<-medication_22q%>%
  mutate(docollect = format(
    as.Date((docollect)),
    "%Y-%m"
  ))

write.csv(medication_22q_non_phi,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_medication_22q_no_phi.csv", row.names = F)


#imaging



imglook <- read.csv("oracle/imglook.csv") %>%
  clean_names()%>%
  filter(scanstat != "IS5")

imglook$doscan <- as.Date(as.character(imglook$doscan), format = "%m/%d/%Y")




imglook_22q<- imglook[imglook$bblid %in% subject_demos_22q$bblid, ]

#write.csv(imglook_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_imaging_22q_phi.csv", row.names = F)

imglook_22q_non_phi<-imglook_22q%>%
  mutate(doscan = format(
    as.Date((doscan)),
    "%Y-%m"
  ))

write.csv(imglook_22q_non_phi,"/data/secure/bit_repo/projects_repo/22q/22q_all/22qdel_imaging_22q_no_phi.csv", row.names = F)




