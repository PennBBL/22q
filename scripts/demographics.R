# ==============================================================================
# Load Libraries
# ==============================================================================

library(data.table)
library(knitr)
library(readxl)
library(janitor)
library(tidyverse)

# ==============================================================================
# Paths
# ==============================================================================
# The server path is the default. To run on a desktop, add this line to your
# ~/.Renviron (once) instead of editing this script:
#   BBLREPO_ROOT_DIR=/Users/<you>/bblrepo1
# (usethis::edit_r_environ() in R console to open Renviron file)

data_root  <- Sys.getenv("BBLREPO_ROOT_DIR", unset = "/data/secure")
oracle_dir <- file.path(data_root, "bit_dwh/oracle")
q22_dir    <- file.path(data_root, "bit_repo/projects_repo/22q/22q_all")

input_paths <- list(
  chop_pull    = file.path(q22_dir,    "chop_penn_data_pull.csv"),
  subject      = file.path(oracle_dir, "subject.csv"),
  study_enroll = file.path(oracle_dir, "subject_studyenrol.csv")
)

output_paths <- list(
  family_demos = file.path(q22_dir, "demographics_22q_family.csv"),
  q22_demos    = file.path(q22_dir, "demographics_22q.csv")
)

# ==============================================================================
# Import CHOP 22q Data
# ==============================================================================


chop_data_raw <- read_csv(input_paths$chop_pull, show_col_types = FALSE) |>
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

study_enroll_raw <- read_csv(input_paths$study_enroll, show_col_types = FALSE) |>
  clean_names() |>
  filter(study_status != "not enrolled") |>
  mutate(doenroll = mdy(doenroll))


# ==============================================================================
# Import Subject Demographics and Correct Family Relationships
# ==============================================================================

subject_demos_raw <- read_csv(input_paths$subject, show_col_types = FALSE) |>
  clean_names() |>
  mutate(
    dobirth  = mdy(dobirth),
    dointake = mdy(dointake)
  )

# ==============================================================================
# Proband Correction Settings
# ==============================================================================

child_labels         <- c("Son", "Daughter")
min_parent_gap_years <- 15  # A Self this much older than the 22q child is assumed to be a parent
# sex coding: 1 = male, 2 = female

# ==============================================================================
# Families With a CHOP 22q Subject
# ==============================================================================
# Keep every member of any family that includes a CHOP 22q subject,
# add CHOP info where available, and sort earliest-seen first within family

q22_families <- subject_demos_raw |>
  filter(any(bblid %in% chop_data_clean$bblid), .by = famid) |>
  left_join(
    chop_data_clean |>
      select(bblid, chop_q_type, chop_q_brkpt, chop_inherited_denovo),
    by = "bblid"
  ) |>
  arrange(famid, dointake, dobirth, bblid) |> 
  distinct()

# ==============================================================================
# Flag Families Where the Proband (Self) Looks Wrong
# ==============================================================================

q22_family_flags <- q22_families |>
  mutate(
    is_self  = rel_to_proband %in% "Self",
    is_child = rel_to_proband %in% child_labels,

    # A CHOP-confirmed 22q child who has an intake date
    is_candidate = is_child & coalesce(chop_q_type == 1, FALSE) & !is.na(dointake),

    # The earliest-seen candidate in each family
    first_child = is_candidate & cumsum(is_candidate) == 1,

    # Family-level facts about the current Self and the first child
    n_self    = sum(is_self),
    self_sex  = first(sex[is_self]),
    self_dob  = first(dobirth[is_self]),
    child_dob = first(dobirth[first_child]),
    gap_years = time_length(interval(self_dob, child_dob), "years"),

    # Self is old enough to be the child's parent, so Self was mislabeled
    swap = n_self == 1 &
      coalesce(gap_years >= min_parent_gap_years, FALSE) &
      self_sex %in% c(1, 2),

    # Make the first child Self if a swap is needed or there's no Self at all
    promote = swap | n_self == 0,

    # A family with one person and no Self: that person is Self
    lone_member = n_distinct(bblid) == 1 & n_self == 0,

    .by = famid
  )


# ==============================================================================
# Correct Relationship to Proband
# ==============================================================================

q22_subjects_with_family <- q22_family_flags |>
  mutate(
    rel_to_proband_original = rel_to_proband,
    rel_to_proband = case_when(
      swap & is_self & sex == 2 ~ "Mother",
      swap & is_self & sex == 1 ~ "Father",
      promote & first_child     ~ "Self",
      lone_member               ~ "Self",
      .default = rel_to_proband
    )
  ) |>
  # Drop helper flags: keep the original columns plus the audit column
  select(all_of(names(q22_families)), rel_to_proband_original) |> 
  select(-rel_to_proband_original)

# View edited records
# q22_subjects_with_family |> filter(rel_to_proband != rel_to_proband_original)


# ==============================================================================
# Final Subject Demographics
# ==============================================================================
# One row per subject: the most recent intake, with age at that intake

subject_demos <- q22_subjects_with_family |>
  mutate(age_at_intake = time_length(interval(dobirth, dointake), "years")) |>
  slice_max(dointake, n = 1, with_ties = FALSE, by = bblid)


# ==============================================================================
# Write Output
# ==============================================================================

write_csv(subject_demos, output_paths$family_demos)
message("Wrote ", nrow(subject_demos), " subjects to ", output_paths$family_demos)

# ==============================================================================
# Create Final 22q Subject Dataset
# ==============================================================================

# Keep subjects identified as Self after correcting family relationships
subject_22q <- subject_demos |>
  filter(
    rel_to_proband == "Self",
    bblid %in% chop_data_clean$bblid
  )

write_csv(subject_22q, output_paths$q22_demos)
message("Wrote ", nrow(subject_22q), " subjects to ", output_paths$q22_demos)
