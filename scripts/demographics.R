# ==============================================================================
# Load Libraries
# ==============================================================================

library(data.table)
library(knitr)
library(readxl)
library(janitor)
library(tidyverse)

# ==============================================================================
# Paths ----
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
  study_enroll = file.path(oracle_dir, "subject_studyenrol.csv"),
  subject_ext_demos = file.path(oracle_dir, "subject_extdemos.csv")
)

output_paths <- list(
  family_demos = file.path(q22_dir, "demographics_22q_and_family.csv"),
  height_weight_bmi = file.path(q22_dir, "height_weight_bmi_22q_and_family.csv"),
  education_family = file.path(q22_dir, "education_22q_and_family.csv"),
  ses_oracle_family = file.path(q22_dir, "ses_oracle_22q_and_family.csv")
  # q22_demos    = file.path(q22_dir, "demographics_22q.csv")
)

# ==============================================================================
# Import CHOP 22q Data ----
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
# Import Study Enrollment Data ----
# ==============================================================================

study_enroll_raw <- read_csv(input_paths$study_enroll, show_col_types = FALSE) |>
  clean_names() |>
  filter(study_status != "not enrolled") |>
  mutate(doenroll = mdy(doenroll))


# ==============================================================================
# Import Subject Demographics and Correct Family Relationships ----
# ==============================================================================

subject_demos_raw <- read_csv(input_paths$subject, show_col_types = FALSE) |>
  clean_names() |>
  mutate(
    dobirth  = mdy(dobirth),
    dointake = mdy(dointake)
  )

# ==============================================================================
# Proband Correction Settings ----
# ==============================================================================

child_labels         <- c("Son", "Daughter")
min_parent_gap_years <- 15  # A Self this much older than the 22q child is assumed to be a parent
# sex coding: 1 = male, 2 = female

# ==============================================================================
# Families With a CHOP 22q Subject ----
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
# Flag Families Where the Proband (Self) Looks Wrong ----
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
    # ([1] returns NA when a family has no Self or no candidate)
    n_self    = n_distinct(bblid[is_self]),
    self_sex  = sex[is_self][1],
    self_dob  = dobirth[is_self][1],
    child_dob = dobirth[first_child][1],
    gap_years = time_length(interval(self_dob, child_dob), "years"),

    # Which side of the family the old Self is on (for grandparents, aunts, uncles)
    self_side = case_when(self_sex == 2 ~ "Maternal", self_sex == 1 ~ "Paternal"),

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
# Correct Relationship to Proband ----
# ==============================================================================

q22_subjects_with_family <- q22_family_flags |>
  mutate(
    rel_to_proband_original = rel_to_proband,
    rel_to_proband = case_when(
      # Old Self becomes the child's parent
      swap & is_self & sex == 2 ~ "Mother",
      swap & is_self & sex == 1 ~ "Father",

      # New Self
      promote & first_child ~ "Self",
      lone_member           ~ "Self",

      # Rest of the family, relabeled relative to the new Self
      # (Brother/Sister are intentionally left unchanged)
      swap & rel_to_proband == "Son"                       ~ "Brother",
      swap & rel_to_proband == "Daughter"                  ~ "Sister",
      swap & rel_to_proband == "Spouse/Partner" & sex == 2 ~ "Mother",
      swap & rel_to_proband == "Spouse/Partner" & sex == 1 ~ "Father",
      swap & rel_to_proband == "Mother"                    ~ paste(self_side, "Grandmother"),
      swap & rel_to_proband == "Father"                    ~ paste(self_side, "Grandfather"),

      .default = rel_to_proband
    )
  ) |>
  # Drop helper flags: keep the original columns plus the audit column
  select(all_of(names(q22_families)), rel_to_proband_original)

# View edited records
# q22_subjects_with_family |> filter(rel_to_proband != rel_to_proband_original) |> select(bblid, famid, rel_to_proband, rel_to_proband_original, chop_q_type) |> View()

# ==============================================================================
# Create Proband Variable ----
# ==============================================================================
# Proband = TRUE for 22q proband who most recently participated (e.g. 22q long)

# Relationships that can be the proband (must also be CHOP 22q)
proband_labels <- c("Self", "Brother", "Sister", "Maternal half-brother", "Son", "Daughter")

# Enrollments that don't count as study participation
excluded_protocols <- c("000000 - Nostudy")

# Latest enrollment
latest_enrollment <- study_enroll_raw |>
  filter(!protocol %in% excluded_protocols) |>
  slice_max(doenroll, n = 1, with_ties = FALSE, by = bblid) |>
  select(
    bblid,
    last_enroll          = doenroll,
    last_enroll_protocol = protocol,
    last_enroll_status   = study_status
  )

# ==============================================================================
# Final Subject Demographics ----
# ==============================================================================
# One row per subject: the most recent intake, with age at that intake

family_demos <- q22_subjects_with_family |>
  mutate(age_at_intake = time_length(interval(dobirth, dointake), "years")) |>
  slice_max(dointake, n = 1, with_ties = FALSE, by = bblid) |>
  left_join(latest_enrollment, by = "bblid", relationship = "one-to-one") |>
  mutate(
    is_proband_eligible = rel_to_proband %in% proband_labels &
      coalesce(chop_q_type == 1, FALSE)
  ) |>
  # Eligible first, then most recent enrollment, then Self on ties
  arrange(famid, desc(is_proband_eligible), desc(last_enroll), rel_to_proband != "Self") |>
  mutate(
    proband = is_proband_eligible & row_number() == 1,
    .by = famid
  ) |>
  # Drop name columns, and all NA columns
  select(
    -c(
      is_proband_eligible
    )) |>
  arrange(famid, bblid)

# ==============================================================================
# Sanity Checks: Sex vs. Relationship ----
# ==============================================================================

# # Expected sex for gendered relationships (1 = male, 2 = female)
# sex_by_relationship <- tribble(
#   ~rel_to_proband, ~expected_sex,
#   "Mother",        2,
#   "Father",        1,
#   "Sister",        2,
#   "Brother",       1
# )

# sex_rel_mismatches <- subject_demos |>
#   inner_join(sex_by_relationship, by = "rel_to_proband") |>
#   filter(sex != expected_sex) |>
#   mutate(
#     # Sort in the same order as the lookup table
#     rel_to_proband = factor(rel_to_proband, levels = sex_by_relationship$rel_to_proband)
#   ) |>
#   select(
#     rel_to_proband, sex, expected_sex, famid, bblid,
#     rel_to_proband_original, dobirth, proband
#   ) |>
#   arrange(rel_to_proband, famid, bblid)

# # How many of each mismatch
# count(sex_rel_mismatches, rel_to_proband)

# sex_rel_mismatches |> print(n = Inf)

# ==============================================================================
# Manual fixes ----
# Based on sex_rel_mismatches and names. 
# Two families with 1+ Self
# ==============================================================================
sex_fixes = tribble(
  ~bblid, ~rel_to_proband, ~sex,
  24280, "Mother", 2, # Sex 3 -> 2
  24109, "Mother", 2, # Sex 1 -> 2, fname=Stacey? (ambig.)
  20459, "Mother", 2, # Sex 1 -> 2, fname=Jennifer (no dob)
  20478, "Mother", 2, # Sex 1 -> 2, fname=Maria (no dob)
  25577, "Mother", 2, # Sex 1 -> 2, fname=Christina
  23879, "Mother", 2, # Sex 1 -> 2, fname=Michele (ambig?)
  25800, "Father", 1, # Sex 2 -> 1, fname=Zachary
  24091, "Father", 1, # Sex 2 -> 1, fname=Kevin
  23092, "Father", 1, # Sex 2 -> 1, fname=Eric
  25578, "Father", 1, # Sex 2 -> 1, fname=Alfred
  21549, "Father", 1, # Sex 2 -> 1, fname=David
  23921, "Brother", 1, # Sex 2 -> 1, fname=Elias
  21494, "Mother", 2, # Sex 1 -> 2 fname=Chris (ambig.)
  26470, "Father", 1, # Rel Mother -> Father (fname=Franklin)
  20136, "Mother", 2, # Rel Father -> Mother, fname=Amy
  20106, "Mother", 2 # Rel Father -> Mother, fname=Samantha
)

# Two Self: 19537, 19538 both del
# Two Self: 21623-no del, 24309-22q
rel_fixes = tribble(
  ~bblid, ~rel_to_proband,
  19537, "Sister", # Keeping first enrolled as Self
  21623, "Sister" # No deletion
)

# Update inherited/denovo for the two siblings with deletion
denovo_fixes = tribble(
  ~bblid, ~chop_inherited_denovo,
  19538, 1
)


family_demos_fixed = family_demos |> 
  rows_update(sex_fixes, by="bblid") |> 
  rows_update(rel_fixes, by="bblid") |> 
  rows_update(denovo_fixes, by="bblid")

# ==============================================================================
# Sanity Checks: Family Structure ----
# ==============================================================================

# family_checks <- family_demos_fixed |>
#   summarise(
#     n_members   = n_distinct(bblid),
#     n_self      = n_distinct(bblid[rel_to_proband %in% "Self"]),
#     n_proband   = n_distinct(bblid[proband]),
#     n_mother    = n_distinct(bblid[rel_to_proband %in% "Mother"]),
#     n_father    = n_distinct(bblid[rel_to_proband %in% "Father"]),
#     .by = famid
#   ) |>
#   mutate(
#     no_self       = n_self == 0,
#     multi_self    = n_self > 1,
#     no_proband    = n_proband == 0,
#     multi_proband = n_proband > 1,
#     multi_mother  = n_mother > 1,
#     multi_father  = n_father > 1
#   )

# # Families that fail any check
# family_problems <- family_checks |>
#   filter(if_any(no_self:multi_father))

# # How many families fail each check
# family_checks |>
#   summarise(across(no_self:multi_father, sum))

# family_problems |> print(n = Inf)

# Note, families with more than 1 Mother, Father, or Self, all seem valid.
# With the Proband var it won't matter as much.

# ==============================================================================
# Merge Duplicate Participant IDs ----
# ==============================================================================
# Each row merges drop_bblid into keep_bblid. Values from keep_bblid win;
# drop_bblid only fills in where keep_bblid is NA.

id_merges <- tribble(
  ~keep_bblid, ~drop_bblid,
  21447,       24786,   
  20000,       20869,
  24090,       20001,
  23393,       20459,
  24096,       24179,
  21400,       21398, 
  21466,       20444, 
  24341,       20805, 
  22093,       20040,
  19693,       21240
)

# The records being merged away, relabeled with the favored ID
merge_fallback <- family_demos_fixed |>
  inner_join(id_merges, join_by(bblid == drop_bblid)) |>
  mutate(bblid = keep_bblid) |>
  select(-keep_bblid)

family_demos_clean <- family_demos_fixed |>
  filter(!bblid %in% id_merges$drop_bblid) |>
  rows_patch(merge_fallback, by = "bblid") |> 
  select(-c( 
      contains("name"),
      suffix,
      sex_hx,
      otherid,
      rel_to_proband_original
    ))

# ==============================================================================
# Create Final 22q Subject Dataset (Not generated) ----
# ==============================================================================

# Keep subjects identified as Self after correcting family relationships
# subject_demos_clean <- family_demos_clean |>
#   filter(
#     !rel_to_proband %in% c("Mother", "Father"),
#     bblid %in% chop_data_clean$bblid
#   )

# ==============================================================================
# Import Extended Demographics ----
# ==============================================================================

subject_ext_demos <- read_csv(input_paths$subject_ext_demos, show_col_types=FALSE) %>%
  clean_names() %>%
  filter(bblid %in% family_demos_clean$bblid) |> 
  mutate(
    docollect = as.Date(as.character(docollect), format = "%m/%d/%Y")
  ) %>%
  
  # Keep the most recent extended demographics record
  group_by(bblid) %>%
  slice_max(docollect, n = 1, with_ties = FALSE) %>%
  ungroup() 

height_weight_bmi = family_demos_clean |> 
  select(bblid, famid, rel_to_proband, proband) |> 
  right_join(
    subject_ext_demos |> select(bblid, height, weight, bmi)
  )

education_family = family_demos_clean |> 
  select(bblid, famid, rel_to_proband, proband) |> 
  right_join(
    subject_ext_demos |> select(bblid, contains("edu"))
  )

ses_oracle_family = family_demos_clean |> 
  select(bblid, famid, rel_to_proband, proband) |> 
  right_join(
    subject_ext_demos |> select(bblid, contains("ses"))
  )

# ==============================================================================
# Write Output ----
# ==============================================================================

write_csv(family_demos_clean, output_paths$family_demos)
message("Wrote ", nrow(family_demos_clean), " subjects to ", output_paths$family_demos)

write_csv(height_weight_bmi, output_paths$height_weight_bmi)
message("Wrote ", nrow(height_weight_bmi), " subjects to ", output_paths$height_weight_bmi)

write_csv(education_family, output_paths$education_family)
message("Wrote ", nrow(education_family), " subjects to ", output_paths$education_family)

write_csv(ses_oracle_family, output_paths$ses_oracle_family)
message("Wrote ", nrow(ses_oracle_family), " subjects to ", output_paths$ses_oracle_family)

# Only outputting family file, can be filtered to Proband, or Self or deleted
# write_csv(subject_demos_clean, output_paths$q22_demos)
# message("Wrote ", nrow(subject_22q), " subjects to ", output_paths$q22_demos)
