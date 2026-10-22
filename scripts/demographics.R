# ==============================================================================
# Load Libraries
# ==============================================================================

library(data.table)
library(knitr)
library(readxl)
library(janitor)
library(tidyverse)

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
# Paths
# ==============================================================================
# The server path is the default. To run on a desktop, add this line to your
# ~/.Renviron (once) instead of editing this script:
#   BBLREPO_ROOT_DIR=/Users/<you>/bblrepo1
# (usethis::edit_r_environ() in R console to open Renviron file)

data_root <- Sys.getenv("BBLREPO_ROOT_DIR", unset = "/data/secure")

paths <- list(
  dwh      = file.path(data_root, "bit_dwh"),
  chop_22q = file.path(data_root, "bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv"),
  study_enroll = file.path(data_root, "bit_dwh/oracle/subject_studyenrol.csv")
)

# ==============================================================================
# Import CHOP 22q Data
# ==============================================================================

# Desktop
# chop_data_raw <- read.csv(
#   "/Users/mruganks/bblrepo1/bit_repo/projects_repo/22q/22q_all/chop_penn_data_pull.csv"
# ) %>%
#   clean_names() %>%
#   rename_with(~ paste0("chop_", .x))

chop_data_raw <- read_csv(paths$chop_22q, show_col_types = FALSE) |>
  clean_names() |>
  rename_with(~ paste0("chop_", .x))

# Keep relevant CHOP variables and subjects with q_type == 1
# Note: Does not remove any variables from raw data (renames bblid)
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

study_enroll_raw <- read_csv(paths$study_enroll, show_col_types = FALSE) |>
  clean_names() |>
  filter(study_status != "not enrolled") |>
  mutate(doenroll = mdy(doenroll))


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

write.csv(subject_demos,"/data/secure/bit_repo/projects_repo/22q/22q_all/demographics_22q_family.csv", row.names = F)

# ==============================================================================
# Create Final 22q Subject Dataset
# ==============================================================================

# Keep subjects identified as Self after correcting family relationships
subject_22q <- subject_demos %>%
  filter(
    rel_to_proband == "Self",
    bblid %in% chop_data_clean$bblid
  ) 


write.csv(subject_22q,"/data/secure/bit_repo/projects_repo/22q/22q_all/demographics_22q.csv", row.names = F)

