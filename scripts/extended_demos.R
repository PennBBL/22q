# ==============================================================================
# Load Libraries
# ==============================================================================

library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse)
library(janitor)
library(knitr)
library(readxl)


# ==============================================================================
# Helper Functions
# ==============================================================================

monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  lt$year * 12 + lt$mon
}

mondf <- function(d1, d2) {
  monnb(d2) - monnb(d1)
}


# ==============================================================================
# Set Working Directory
# ==============================================================================

# USE THIS WHEN WORKING ON DESKTOP
#setwd("/Users/mruganks/bblrepo1/bit_dwh")

setwd("/data/secure/bit_dwh")


# ==============================================================================
# Import CHOP 22q Data
# ==============================================================================

# Desktop
# chop_data_raw <- read.csv(
#   "/Users/mruganks/bblrepo1/bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv"
# ) %>%
#   clean_names() %>%
#   rename_with(~ paste0("chop_", .x))

# Server
chop_data_raw <- read.csv(
  "/data/secure/bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv"
) %>%
  clean_names() %>%
  rename_with(~ paste0("chop_", .x))


# Keep relevant CHOP variables and subjects with q_type == 1
chop_data_clean <- chop_data_raw %>%
  select(
    bblid = chop_bbl_id,
    chop_vcfs_id,
    chop_chop_mrn,
    chop_deceased,
    chop_q_type,
    chop_q_brkpt,
    chop_q_brkpt_oth,
    chop_inherited_denovo,
    contains("race"),
    contains("sex"),
    chop_ethnicity,
    chop_dob
  ) %>%
  filter(chop_q_type == 1)


# ==============================================================================
# Import Study Enrollment Data
# ==============================================================================

study_enroll_raw <- read.csv("oracle/subject_studyenrol.csv") %>%
  clean_names() %>%
  filter(study_status != "not enrolled") %>%
  mutate(
    doenroll = as.Date(as.character(doenroll), format = "%m/%d/%Y")
  )


# ==============================================================================
# Import Subject Demographics and Correct Family Relationships
# ==============================================================================

subject_demos_raw <- read.csv("oracle/subject.csv") %>%
  clean_names() %>%
  mutate(
    dobirth = mdy(dobirth),
    dointake = mdy(dointake)
  )


# Keep all members of families that contain a CHOP 22q subject
q22_subjects_with_family <- subject_demos_raw %>%
  
  filter(
    any(bblid %in% chop_data_clean$bblid),
    .by = famid
  ) %>%
  
  # Add CHOP information where available
  left_join(
    chop_data_clean %>%
      select(
        bblid,
        chop_q_type,
        chop_q_brkpt,
        chop_inherited_denovo
      ),
    by = "bblid"
  ) %>%
  
  # Sort earliest-seen subjects first within each family
  arrange(famid, dointake, dobirth, bblid) %>%
  
  mutate(
    
    # Identify children with 22q
    is_candidate = rel_to_proband %in% c("Son", "Daughter") &
      chop_q_type == 1 &
      !is.na(dointake),
    
    # Identify the first eligible child seen in each family
    first_child = is_candidate & cumsum(is_candidate) == 1,
    
    # Count the number of subjects currently identified as Self
    n_self = sum(rel_to_proband == "Self"),
    
    # Get sex of the current Self
    self_sex = sex[rel_to_proband == "Self"][1],
    
    # Calculate age difference between Self and first eligible child
    gap_years = as.numeric(
      dobirth[first_child][1] -
        dobirth[rel_to_proband == "Self"][1]
    ) / 365.25,
    
    # If Self is at least 15 years older than the child,
    # assume the parent was incorrectly entered as Self
    swap = n_self == 1 &
      coalesce(gap_years >= 15, FALSE) &
      self_sex %in% 1:2,
    
    # Promote the first child to Self if a swap is needed
    # or if the family currently has no Self
    promote = swap | n_self == 0,
    
    # If the only family member is listed as Son/Daughter,
    # treat that subject as Self
    only_child = n() == 1 &
      rel_to_proband %in% c("Son", "Daughter"),
    
    .by = famid
  ) %>%
  
  # Correct relationship to proband
  mutate(
    rel_to_proband_original = rel_to_proband,
    
    rel_to_proband = case_when(
      swap & rel_to_proband == "Self" & sex == 2 ~ "Mother",
      swap & rel_to_proband == "Self" & sex == 1 ~ "Father",
      promote & first_child ~ "Self",
      only_child ~ "Self",
      .default = rel_to_proband
    )
  ) %>%
  
  # Remove temporary variables
  select(
    -c(
      is_candidate,
      first_child,
      n_self,
      self_sex,
      gap_years,
      swap,
      promote,
      only_child
    )
  )


# ==============================================================================
# Create Subject Demographics Dataset
# ==============================================================================

subject_demos <- q22_subjects_with_family %>%
  
  # Calculate age at intake
  mutate(
    age_at_intake = interval(dobirth, dointake) / years(1)
  ) %>%
  
  # Keep the most recent intake for each subject
  group_by(bblid) %>%
  slice_max(dointake, n = 1, with_ties = FALSE) %>%
  ungroup()


# ==============================================================================
# Import Extended Demographics
# ==============================================================================

subject_ext_demos <- read.csv("oracle/subject_extdemos.csv") %>%
  clean_names() %>%
  mutate(
    docollect = as.Date(as.character(docollect), format = "%m/%d/%Y")
  ) %>%
  
  # Keep the most recent extended demographics record
  group_by(bblid) %>%
  slice_max(docollect, n = 1, with_ties = FALSE) %>%
  ungroup() 


#==============================================================================
#Combine Subject Demographics
#==============================================================================

subject_full_demo <- left_join(
  subject_demos,
  subject_ext_demos,
  by = "bblid"
)


# ==============================================================================
# Create Final 22q Subject Dataset
# ==============================================================================

# Keep subjects identified as Self after correcting family relationships
subject_22q <- subject_full_demo %>%
  filter(
    rel_to_proband == "Self",
    bblid %in% chop_data_clean$bblid
  ) 


h_w_b <- subject_full_demo %>%
  select(bblid, famid, docollect, rel_to_proband, height, weight,bmi )

education<- subject_full_demo %>%
  select(bblid, famid, docollect, rel_to_proband, contains("edu"))

ses_oracle<- subject_full_demo %>%
  select(bblid, famid, docollect, rel_to_proband, contains("ses"))


write.csv(h_w_b,"/data/secure/bit_repo/projects_repo/22q/22q_all/height_weight_bmi_family.csv", row.names = F)

write.csv(education,"/data/secure/bit_repo/projects_repo/22q/22q_all/education_family.csv", row.names = F)

write.csv(ses_oracle,"/data/secure/bit_repo/projects_repo/22q/22q_all/ses_oracle_family.csv", row.names = F)