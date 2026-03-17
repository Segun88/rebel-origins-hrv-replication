

# ============================================================
# GROUP-YEAR LEVEL ANALYSIS SCRIPT (FULLY REVISED)
# Improved GDP & V-Dem Coverage + Clean Variable Creation
# ============================================================

# ---- 1. Load Required Libraries ----
library(haven)
library(dplyr)
library(tidyverse)
library(janitor)
library(WDI)
library(ggplot2)
library(lmtest)
library(modelsummary)
library(sandwich)
library(MASS)
library(lubridate)
library(countrycode)
library(marginaleffects)
library(car)





# ---- 2. Load Data ----
humanrv   <- read_dta("data/raw/rhr (fresh).dta") |> as_tibble()
forge     <- read_dta("data/raw/newforge_v1.0_public.dta") |> as_tibble()
nsa_data <- read_tsv("data/raw/nsa_.asc") |> clean_names()
wdi_raw   <- readRDS("data/raw/wdi_gdp_data_1989_2019.rds")
vdem_raw  <- read_dta("data/raw/V-Dem-CY-Core-v15.dta")
cat("✅ Data loaded successfully\n")



# NSA: Expand to Group-Year and Collapse
# The NSA data arrives at the dyad level with start and end dates. 
# Each dyad record is expanded into one row per year of activity, and then
# collapsed so that each sideb x year combination has a single row.
# =================================================================
# PART A: NSA (expand to group-year, collapse)
# ============================================================
nsa_data <- nsa_data %>%
  dplyr::rename(dyadid1 = dyadid, sideb = side_b)
nsa_years <- nsa_data %>%
  dplyr::mutate(
    start_year = lubridate::year(lubridate::ymd(startdate)),
    end_year   = lubridate::year(lubridate::ymd(enddate))
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(year = list(seq.int(start_year, end_year))) %>%
  tidyr::unnest(year) %>%
  dplyr::ungroup()
nsa_year_clean <- nsa_years %>%
  dplyr::mutate(
    start_year = lubridate::year(lubridate::ymd(startdate)),
    conflict_duration = year - start_year
  ) %>%
  dplyr::select(
    dyadid1, sideb, year, startdate, enddate, conflict_duration,
    rebestimate, rebstrength, centcontrol, fightcap, terrcont,
    rebextpart, govextpart, rebel_support
  ) %>%
  dplyr::mutate(year = as.integer(year))
order_rebstrength <- c("much stronger","stronger","parity","weaker","much weaker")
order_fightcap    <- c("high","moderate","low")
order_yesno       <- c("yes","present","1","no","0")
nsa_year_collapsed <- nsa_year_clean %>%
  dplyr::mutate(
    rebstrength = factor(rebstrength, levels = order_rebstrength, ordered = TRUE),
    fightcap    = factor(fightcap,    levels = order_fightcap,    ordered = TRUE),
    terrcont    = factor(terrcont,    levels = order_yesno,       ordered = TRUE),
    centcontrol = factor(centcontrol, levels = order_yesno,       ordered = TRUE)
  ) %>%
  dplyr::group_by(dyadid1, sideb, year) %>%
  dplyr::summarise(
    rebstrength       = suppressWarnings(max(rebstrength, na.rm = TRUE)),
    fightcap          = suppressWarnings(max(fightcap,    na.rm = TRUE)),
    terrcont          = suppressWarnings(max(terrcont,    na.rm = TRUE)),
    centcontrol       = suppressWarnings(max(centcontrol, na.rm = TRUE)),
    rebel_support     = dplyr::first(stats::na.omit(rebel_support)),
    conflict_duration = suppressWarnings(max(conflict_duration, na.rm = TRUE)),
    rebestimate       = suppressWarnings(max(rebestimate, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(rebstrength, fightcap, terrcont, centcontrol),
                  ~na_if(as.character(.), "-Inf")),
    year = as.integer(year)
  )
cat("✅ NSA yearly data ready:", nrow(nsa_year_collapsed), "rows\n")
glimpse(nsa_year_collapsed)
head(nsa_year_collapsed, 5)
nsa_year_clean |> count(sideb, year) |> filter(n > 1)






## FORGE: Select Key Variables

# FORGE provides the organizational origin data for each rebel group. 
# Here, the dyad identifier is harmonized and the relevant parent-organization 
# indicators needed for my research are only selected.


# ============================================================
# PART B: FORGE (select key vars)
# ============================================================

forge <- forge %>% dplyr::rename(dyadid1 = NSAdyadid)

forge_vars <- c("dyadid1", "sideb", "gname", "ccode", "cname", "conflict_id",
                "foundyear", "ideology", "preorg", "preorgno", "preorgreb",
                "preorgter", "preorgpar", "preorgmvt", "preorgyou", "preorglab",
                "preorgmil", "preorggov", "preorgfmr", "preorgrel", "preorgfor",
                "preorgref", "preorgeth", "preorgoth", "preorgname", "merger", "splinter")

forge_clean <- forge %>% dplyr::select(dplyr::any_of(forge_vars))
cat("✅ FORGE data prepared:", nrow(forge_clean), "groups\n")


glimpse(forge_clean)
head(forge_clean, 5)



## HRV: Collapse to Group-Year

# The RHRV data contains event-level violation records. 
# I aggregate this to group-year level by retaining the first non-missing 
# identifier per group-year and the maximum reported severity for each violation type.

# ============================================================
# PART C: HRV (collapse to group-year)
# ============================================================

humanrv_clean <- humanrv %>%
  janitor::clean_names() %>%
  dplyr::rename(dyadid1 = dyadid)

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0) x[1] else NA
}

hrv_cy <- humanrv_clean %>%
  dplyr::group_by(dyadid1, sideb, year) %>%
  dplyr::summarize(
    intensity   = first_non_missing(intensity),
    osv         = first_non_missing(osv),
    osv_dummy   = first_non_missing(osv_dummy),
    conflictid  = first_non_missing(conflictid),
    sidebid     = first_non_missing(sidebid),
    location    = first_non_missing(location),
    sidea       = first_non_missing(sidea),
    violation_s = suppressWarnings(max(violation_s,   na.rm = TRUE)),
    rkillings_s = suppressWarnings(max(rkillings_s,   na.rm = TRUE)),
    rtorture_s  = suppressWarnings(max(rtorture_s,    na.rm = TRUE)),
    rdetention_s= suppressWarnings(max(rdetention_s,  na.rm = TRUE)),
    rproperty_s = suppressWarnings(max(rproperty_s,   na.rm = TRUE)),
    rrecruitment_s = suppressWarnings(max(rrecruitment_s, na.rm = TRUE)),
    rsexual_s   = suppressWarnings(max(rsexual_s,     na.rm = TRUE)),
    rdisplace_s = suppressWarnings(max(rdisplace_s,   na.rm = TRUE)),
    rrestrict_s = suppressWarnings(max(rrestrict_s,   na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(violation_s, rkillings_s, rtorture_s, rdetention_s,
                    rproperty_s, rrecruitment_s, rsexual_s, rdisplace_s, rrestrict_s),
                  ~ ifelse(is.infinite(.), NA_real_, as.integer(.))),
    year = as.integer(year)
  )

cat("✅ HRV group-year data ready:", nrow(hrv_cy), "rows\n")

hrv_cy %>% count(sideb, year) %>% filter(n>1)


glimpse(hrv_cy)
head(hrv_cy, 5)




# In this section, I continued the wrangling by continuing the merging of 
# datasets. I incorporated the GDP and V-dem datasets while merging 
# with other datasets.

## Merge HRV + FORGE + NSA into a Unique Group-Year Panel

# ============================================================
# PART D: Merge HRV + FORGE + NSA (unique group-year) ALL DUPLICATES REMOVED
# ============================================================

# Step 1: HRV left-join FORGE (static origin data onto group-year violations)
panel_step1 <- hrv_cy %>%
  dplyr::left_join(forge_clean, by = "sideb")

panel_step1_unique <- panel_step1 %>%
  dplyr::group_by(sideb, year) %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::first(stats::na.omit(.))),
                   .groups = "drop")

# Step 2: Collapse NSA to unique sideb-year
nsa_year_unique <- nsa_year_collapsed %>%
  dplyr::group_by(sideb, year) %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::first(stats::na.omit(.))),
                   .groups = "drop")

# Step 3: Left-join NSA capability data
newpanel_final <- panel_step1_unique %>%
  dplyr::left_join(nsa_year_unique, by = c("sideb", "year")) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  tibble::as_tibble()

print(paste("After merging HRV + FORGE + NSA:", nrow(newpanel_final), "rows"))


glimpse(newpanel_final)
head(newpanel_final, 5)


# Quick dup check
dup_keys <- newpanel_final %>%
  dplyr::count(sideb, year) %>% dplyr::filter(n > 1)
if (nrow(dup_keys) > 0) message("⚠️ Duplicates remain in (sideb,year).")





## GDP (WDI): Country-Code Harmonization, Merge, and Interpolation

# Merging GDP data requires translating COW country codes 
#(used in the conflict data) to ISO3C codes (used by the World Bank). 
# After merging, linear interpolation fills within-country gaps in GDP coverage.

# ============================================================
# PART E: GDP (WDI) — build ISO3C, merge, interpolate
# ============================================================
# Build ISO3C from COW codes with fallback from country names
custom_match <- c(
  "DR Congo (Zaire)" = "COD", "DR Congo" = "COD",
  "Congo (Kinshasa)" = "COD", "Yemen (North Yemen)" = "YEM",
  "Serbia (Yugoslavia)" = "SRB", "Russia (Soviet Union)" = "RUS",
  "Macedonia, FYR" = "MKD", "Bosnia-Herzegovina" = "BIH",
  "Myanmar (Burma)" = "MMR", "South Sudan" = "SSD"
)

newpanel_final <- newpanel_final %>%
  dplyr::mutate(
    ccode      = suppressWarnings(as.integer(ccode)),
    iso3c_ccode = countrycode(ccode, "cown", "iso3c", warn = FALSE),
    sidea_clean = stringr::str_squish(sidea),
    iso3c_name  = countrycode(sidea_clean, "country.name", "iso3c",
                              warn = FALSE, custom_match = custom_match),
    iso3c       = dplyr::coalesce(iso3c_ccode, iso3c_name)
  )

# Clean and merge WDI GDP data
wdi_clean <- wdi_raw %>%
  dplyr::transmute(iso3c = iso3c, year = as.integer(year), gdppc_const2015) %>%
  dplyr::filter(!is.na(iso3c), !is.na(year), !is.na(gdppc_const2015))

newpanel_final <- newpanel_final %>%
  dplyr::select(-dplyr::any_of(c("gdp_pc","log_gdp_pc","gdppc_const2015"))) %>%
  dplyr::left_join(wdi_clean, by = c("iso3c","year"))

# Linear interpolation within country to fill GDP gaps
wdi_filled <- newpanel_final %>%
  dplyr::distinct(iso3c, year, gdppc_const2015) %>%
  dplyr::arrange(iso3c, year) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::mutate(
    gdppc_filled = {
      y <- gdppc_const2015; x <- year
      if (sum(!is.na(y)) >= 2) {
        approx(x = x[!is.na(y)], y = y[!is.na(y)], xout = x,
               method = "linear", rule = 1)$y
      } else y
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3c, year, gdppc_filled)

newpanel_final <- newpanel_final %>%
  dplyr::select(-gdppc_const2015) %>%
  dplyr::left_join(wdi_filled, by = c("iso3c","year")) %>%
  dplyr::rename(gdp_pc = gdppc_filled) %>%
  dplyr::mutate(log_gdp_pc = log(gdp_pc))





# V-Dem polyarchy scores are matched to the panel via ISO3C 
#country codes and year.

# ============================================================
# PART F: V-Dem (democracy) — ISO3C match and merge
# ============================================================

vdem_clean <- vdem_raw %>%
  dplyr::select(year, COWcode, v2x_polyarchy) %>%
  dplyr::mutate(
    year = as.integer(year),
    cown = suppressWarnings(as.integer(COWcode)),
    iso3c = countrycode(cown, "cown", "iso3c", warn = FALSE),
    democracy = as.numeric(v2x_polyarchy)
  ) %>%
  dplyr::filter(!is.na(iso3c), !is.na(year), !is.na(democracy),
                year >= 1989, year <= 2019) %>%
  dplyr::select(iso3c, year, democracy)

newpanel_final <- newpanel_final %>%
  dplyr::left_join(vdem_clean, by = c("iso3c","year")) %>%
  dplyr::mutate(
    democracy_z = as.numeric(scale(democracy)),
    democracy_high = as.integer(!is.na(democracy) & democracy >= 0.5)
  )






# ============================================================
# PART G: Final coverage summary
# ============================================================

newpanel_final %>%
  dplyr::summarise(
    total_rows = dplyr::n(),
    gdp_coverage = round(mean(!is.na(gdp_pc)) * 100, 1),
    democracy_coverage = round(mean(!is.na(democracy)) * 100, 1),
    both_coverage = round(mean(!is.na(gdp_pc) & !is.na(democracy)) * 100, 1)
  ) %>%
  print()





# ============================================================
# PART H: CREATE VIOLATION COUNT VARIABLE
# ============================================================

# ---- 9. Create Violation Count ----
viol_vars <- c("rkillings_s", "rtorture_s", "rdetention_s", "rproperty_s",
               "rrecruitment_s", "rsexual_s", "rdisplace_s", "rrestrict_s")

newpanel_final <- newpanel_final %>%
  mutate(
    violation_count = rowSums(across(all_of(viol_vars), ~ as.integer(. >= 1)), na.rm = TRUE)
  )




# ============================================================
# PART I: FILTER TO COMPLETE CASES AND CREATE MODEL BASE
# ============================================================


# Observations with missing key rebel capability measures are 
# however dropped to form the analytic base.

# ---- 10. Filter to Complete Cases ----
model_base <- newpanel_final %>%
  filter(!is.na(rebstrength) & !is.na(terrcont))

print(paste("After filtering NSA variables:", nrow(model_base), "rows"))

glimpse(model_base)
head(model_base, 5)




## Create Origin Variables

### Broad Parent Organization Categories

# Each FORGE parent-organization indicator is recoded to 0/1, then grouped into 
# four broad origin types: civil society, violent/military parent, political party, 
# and no parent organization.


# ============================================================
# PART J: CREATE ORIGIN VARIABLES
# ============================================================

# ---- 11. Initialize Parent Organization Variables ----
model_base <- model_base %>%
  mutate(
    across(
      c(preorgmvt, preorgyou, preorglab, preorgrel, 
        preorgeth, preorgref, preorgno, preorgoth,
        preorgmil, preorgfmr, preorgfor, preorgter, 
        preorggov, preorgreb, preorgpar),
      ~ replace_na(as.integer(.), 0L)
    )
  )



# ---- 12. Create Broad Parent Organization Categories ----
model_base <- model_base %>%
  mutate(
    # Civil society origins
    civil_society = as.integer(
      preorgmvt == 1L | preorgyou == 1L | preorglab == 1L | preorgrel == 1L
    ),
    
    # No formal parent organization
    no_parorg = as.integer(
      preorgeth == 1L | preorgref == 1L | preorgno == 1L | preorgoth == 1L
    ),
    
    # Violent/armed parent
    violent_parent = as.integer(
      preorgmil == 1L | preorgfmr == 1L | preorgfor == 1L | 
        preorgter == 1L | preorgreb == 1L
    ),
    
    # Political party origin
    political_party = as.integer(preorgpar == 1L),
    
    # Government faction origin
    govt_faction = as.integer(preorggov == 1L),
    
    # Detailed violent origin subcategories
    military_origin = as.integer(preorgmil == 1 | preorgfmr == 1),
    armed_nonstate_origin = as.integer(preorgreb == 1 | preorgter == 1),
    foreign_armed_origin = as.integer(preorgfor == 1)
  )



# ---- 13. Create Mutually Exclusive Origin Categories ----
model_base <- model_base %>%
  mutate(
    # Exclusive categories (no overlap)
    civil_only = as.integer(
      civil_society == 1 & violent_parent == 0 & 
        no_parorg == 0 & political_party == 0 & govt_faction == 0
    ),
    
    violent_only = as.integer(
      violent_parent == 1 & civil_society == 0 & 
        no_parorg == 0 & political_party == 0 & govt_faction == 0
    ),
    
    no_parent_only = as.integer(
      no_parorg == 1 & civil_society == 0 & 
        violent_parent == 0 & political_party == 0 & govt_faction == 0
    ),
    
    political_party_only = as.integer(
      political_party == 1 & civil_society == 0 & 
        violent_parent == 0 & no_parorg == 0 & govt_faction == 0
    ),
    
    govt_faction_only = as.integer(
      govt_faction == 1 & civil_society == 0 & 
        violent_parent == 0 & no_parorg == 0 & political_party == 0
    ),
    
    # Mixed origins (for tracking, not analysis)
    mixed_origin = as.integer(
      (civil_society + violent_parent + no_parorg + political_party + govt_faction) > 1
    )
  )





# ---- 14. Create Mutually Exclusive Detailed Violent Subcategories ----
model_base <- model_base %>%
  mutate(
    military_only = as.integer(
      violent_only == 1 & military_origin == 1 & 
        armed_nonstate_origin == 0 & foreign_armed_origin == 0
    ),
    
    armed_nonstate_only = as.integer(
      violent_only == 1 & armed_nonstate_origin == 1 & 
        military_origin == 0 & foreign_armed_origin == 0
    ),
    
    foreign_armed_only = as.integer(
      violent_only == 1 & foreign_armed_origin == 1 & 
        military_origin == 0 & armed_nonstate_origin == 0
    ),
    
    mixed_violent = as.integer(
      violent_only == 1 & 
        (military_origin + armed_nonstate_origin + foreign_armed_origin) > 1
    )
  )



## Create Dependent Variables: Discriminatory vs. Indiscriminate Violence

#Violation types are classified into two conceptual groups. 
# Binary indicators, variety counts, severity scores, 
# and compositional shares are all constructed.


# ============================================================
# PART K: CREATE DISCRIMINATORY/INDISCRIMINATE VIOLENCE VARIABLES
# ============================================================
# ---- 15. Create Discriminatory and Indiscriminate DVs ----
model_base <- model_base %>%
  mutate(
    # Convert ordinal to binary (ever violated)
    killings_binary = as.integer(rkillings_s >= 1),
    detention_binary = as.integer(rdetention_s >= 1),
    torture_binary = as.integer(rtorture_s >= 1),
    recruitment_binary = as.integer(rrecruitment_s >= 1),
    sexual_binary = as.integer(rsexual_s >= 1),
    displace_binary = as.integer(rdisplace_s >= 1),
    restrict_binary = as.integer(rrestrict_s >= 1),
    property_binary = as.integer(rproperty_s >= 1),
    
    # Composite binary measures
    discriminatory_binary = as.integer(
      killings_binary == 1 | detention_binary == 1 | torture_binary == 1
    ),
    indiscriminate_binary = as.integer(
      recruitment_binary == 1 | sexual_binary == 1 | displace_binary == 1 | 
        restrict_binary == 1 | property_binary == 1
    ),
    
    # Variety counts (0-3 for discriminatory, 0-5 for indiscriminate)
    discriminatory_variety = killings_binary + detention_binary + torture_binary,
    indiscriminate_variety = recruitment_binary + sexual_binary + displace_binary + 
      restrict_binary + property_binary,
    
    # H2: LEVEL OF VIOLATIONS (Total variety and severity)
    total_variety = discriminatory_variety + indiscriminate_variety,  # 0-8 scale
    total_severity = (rkillings_s + rdetention_s + rtorture_s) + 
      (rrecruitment_s + rsexual_s + rdisplace_s + 
         rrestrict_s + rproperty_s),  # 0-16 scale
    any_violation = as.integer(total_variety > 0),  # Binary: 0/1
    
    # H1: PATTERN OF VIOLATIONS (Discriminatory share)
    violence_share_disc = if_else(
      total_variety > 0,
      discriminatory_variety / total_variety,
      NA_real_
    ),
    
    # ALTERNATIVE MEASURES
    # Violence ratio
    
    violence_ratio_smooth = (discriminatory_variety + 1) / (indiscriminate_variety + 1),
    
    
    # Violence difference (normalized for different scales)
    violence_diff = discriminatory_variety - (5/3) * indiscriminate_variety,
    
    # Individual severity scores (for reference)
    discriminatory_severity = rkillings_s + rdetention_s + rtorture_s,
    indiscriminate_severity = rrecruitment_s + rsexual_s + rdisplace_s + 
      rrestrict_s + rproperty_s
  )






# ============================================================
# PART N: CHECK DV DISTRIBUTIONS 
# ============================================================

# Violence share (H1)
summary(model_base$violence_share_disc)
table(cut(model_base$violence_share_disc, 
          breaks = c(0, 0.25, 0.5, 0.75, 1.0),
          include.lowest = TRUE,
          labels = c("Mostly Indiscrim (0-25%)", "Mixed-Indiscrim (25-50%)", 
                     "Mixed-Discrim (50-75%)", "Mostly Discrim (75-100%)")))

# Total variety (H2)
summary(model_base$total_variety)
table(model_base$total_variety)

# Total severity
summary(model_base$total_severity)

# Binary outcome
table(model_base$any_violation)

# Alternative measures
summary(model_base$violence_ratio)
summary(model_base$violence_diff)

# Missing data
sum(is.na(model_base$violence_share_disc))
sum(!is.na(model_base$violence_share_disc))







# ============================================================
# PART L: CREATE CONTROL VARIABLES (ORDINAL VERSIONS)
# ============================================================

# Character-coded NSA variables are converted to ordered numeric scales 
# for use in regression models.


# Ensure ordered factors exist first
order_rebstrength <- c("much weaker", "weaker", "parity", "stronger", "much stronger")
order_fightcap <- c("low", "moderate", "high")
order_terrcont <- c("no", "0", "yes", "present", "1")
order_centcontrol <- c("no", "0", "yes", "present", "1")

model_base <- model_base %>%
  mutate(
    # Create ordered factors from character versions
    rebstrength_ordered = factor(rebstrength, levels = order_rebstrength, ordered = TRUE),
    fightcap_ordered = factor(fightcap, levels = order_fightcap, ordered = TRUE),
    terrcont_ordered = factor(terrcont, levels = order_terrcont, ordered = TRUE),
    centcontrol_ordered = factor(centcontrol, levels = order_centcontrol, ordered = TRUE),
    
    # Convert to numeric (preserves ordinality)
    rebstrength_num = as.numeric(rebstrength_ordered),      # 1-5
    fightcap_num = as.numeric(fightcap_ordered),            # 1-3
    terrcont_num = as.numeric(terrcont_ordered),            # Ordinal
    centcontrol_num = as.numeric(centcontrol_ordered),      # Ordinal
    
    # Rebel support ordinal
    rebel_support_num = case_when(
      rebel_support == "explicit" ~ 2L,
      rebel_support == "alleged" ~ 1L,
      rebel_support == "no" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Verify
summary(model_base$rebstrength_num)
summary(model_base$fightcap_num)
summary(model_base$terrcont_num)
summary(model_base$rebel_support_num)






# ============================================================
# PART M: FILTER TO ANALYSIS YEARS
# ============================================================


# Step 1 — Filter to analysis years first
model_base <- model_base %>%
  filter(year >= 1990, year <= 2018)




# Build Analytic Samples

# Two mutually exclusive analytic samples 
# are constructed for my proposed regression models.


# ============================================================
# SWITCHES (define these BEFORE running samples)
# ============================================================
DROP_MERGERS <- TRUE                 # TRUE = drop merger groups; FALSE = keep
COMMUNITY_INCLUDES_PARTY <- TRUE     # TRUE = civil_society OR political_party; FALSE = civil_society only





# ============================================================
# SAFETY: make sure key indicators are 0/1 and non-missing
# ============================================================
req_vars <- c(
  "merger",
  "civil_society","political_party","violent_parent",
  "preorgno","preorgeth","preorgref","preorgoth",
  "civil_only","violent_only","no_parent_only","political_party_only"
)

missing_vars <- setdiff(req_vars, names(model_base))
if (length(missing_vars) > 0) stop("Missing variables in model_base: ", paste(missing_vars, collapse = ", "))

# This safety check verifies that `model_base` contains all 12 
# required origin variables before running the analysis. 
# If any variables are missing, the script immediately stops with an error message listing what's missing.






# ============================================================
# Build origin_clean in model_base (4-category label)
# ============================================================
model_base <- model_base %>%
  mutate(
    origin_clean = case_when(
      civil_only            == 1L ~ "Civil society origin",
      violent_only          == 1L ~ "Violent/military origin",
      political_party_only  == 1L ~ "Political party origin",
      no_parent_only        == 1L ~ "No parent organization",
      TRUE ~ NA_character_
    ),
    origin_clean = factor(
      origin_clean,
      levels = c("Violent/military origin",
                 "Civil society origin",
                 "Political party origin",
                 "No parent organization")
    )
  )





# ============================================================
# Sample A: origin_3cat (violent vs civil vs noparent) — EXCLUSIVE
# ============================================================

model_data_all <- model_base %>%
  { if (DROP_MERGERS) filter(., merger == 0L) else . } %>%
  filter(civil_only + violent_only + no_parent_only == 1L) %>%
  mutate(
    origin_3cat = case_when(
      violent_only   == 1L ~ "violent",
      civil_only     == 1L ~ "civil",
      no_parent_only == 1L ~ "noparent",
      TRUE ~ NA_character_
    ),
    origin_3cat = factor(origin_3cat, levels = c("violent","civil","noparent"))
  ) %>%
  filter(!is.na(origin_3cat))




# ============================================================
# Sample B: origin_comm (violent vs community vs noparent) — EXCLUSIVE
# ============================================================

model_data_comm_broad <- model_base %>%
  { if (DROP_MERGERS) filter(., merger == 0L) else . } %>%
  mutate(
    community_ties = if (COMMUNITY_INCLUDES_PARTY) {
      as.integer(civil_society == 1L | political_party == 1L)
    } else {
      as.integer(civil_society == 1L)
    },
    no_parorg_broad = as.integer(preorgno == 1L | preorgeth == 1L | preorgref == 1L | preorgoth == 1L)
  ) %>%
  filter(community_ties + violent_parent + no_parorg_broad == 1L) %>%
  mutate(
    origin_comm = case_when(
      violent_parent  == 1L ~ "violent",
      community_ties  == 1L ~ "community",
      no_parorg_broad == 1L ~ "noparent",
      TRUE ~ NA_character_
    ),
    origin_comm = factor(origin_comm, levels = c("violent","community","noparent"))
  ) %>%
  filter(!is.na(origin_comm))





# ============================================================
# QUICK CHECKS 
# ============================================================
table(model_data_all$origin_3cat)
table(model_data_comm_broad$origin_comm)

model_data_comm_broad %>%
  mutate(sum_check = community_ties + violent_parent + no_parorg_broad) %>%
  count(sum_check)




# ============================================================
# REBEL GROUP ORIGINS AND HUMAN RIGHTS VIOLATIONS
# Organized Analysis: No-Parent, Civil Society, Community Ties
# ============================================================




# Helper function for cluster-robust standard errors
tidy_cluster <- function(model, cluster_var, conf = 0.95) {
  V  <- vcovCL(model, cluster = cluster_var)
  ct <- coeftest(model, vcov = V)
  
  is_lm <- inherits(model, "lm") && !inherits(model, "glm")
  if (is_lm) {
    crit <- qt(1 - (1 - conf) / 2, df = model$df.residual)
  } else {
    crit <- qnorm(1 - (1 - conf) / 2)
  }
  
  tidy <- data.frame(
    term      = rownames(ct),
    estimate  = ct[, 1],
    std.error = ct[, 2],
    statistic = ct[, 3],
    p.value   = ct[, 4],
    stringsAsFactors = FALSE
  ) |>
    mutate(
      conf.low  = estimate - crit * std.error,
      conf.high = estimate + crit * std.error
    )
  
  list(ct = ct, vcov = V, tidy = tidy)
}




# ============================================================
# SECTION 1: PARENTLESS ORIGIN
# Reference = All other origin types combined
# Sample: model_data_noparent (full model_base)
# ============================================================

# --- Fractional Logit: Violence Share ---
m1_nopar_share <- glm(
  violence_share_disc ~ no_parent + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_noparent %>% filter(!is.na(violence_share_disc)),
  family = quasibinomial("logit")
)
r1_nopar_share <- tidy_cluster(m1_nopar_share, ~sideb)
r1_nopar_share$ct
nobs(m1_nopar_share)

# --- Binomial Composition: cbind (violators only) ---
df_nopar_pattern <- model_data_noparent %>%
  filter(total_variety > 0) %>%
  mutate(
    disc_count   = as.integer(discriminatory_variety),
    indisc_count = as.integer(indiscriminate_variety)
  )

m1_nopar_pattern <- glm(
  cbind(disc_count, indisc_count) ~ no_parent +
    log_gdp_pc + democracy_z + ideology +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  family = binomial("logit"),
  data   = df_nopar_pattern
)
r1_nopar_pattern <- tidy_cluster(m1_nopar_pattern, ~sideb)
r1_nopar_pattern$ct
nobs(m1_nopar_pattern)

# --- Binary: Any Violation ---
m1_nopar_anyviol <- glm(
  violation_s ~ no_parent + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_noparent,
  family = binomial("logit")
)
r1_nopar_anyviol <- tidy_cluster(m1_nopar_anyviol, ~sideb)
r1_nopar_anyviol$ct
nobs(m1_nopar_anyviol)

# --- Binary: Discriminatory ---
m1_nopar_disc_bin <- glm(
  discriminatory_binary ~ no_parent + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_noparent,
  family = binomial("logit")
)
r1_nopar_disc_bin <- tidy_cluster(m1_nopar_disc_bin, ~sideb)
r1_nopar_disc_bin$ct
nobs(m1_nopar_disc_bin)

# --- Binary: Indiscriminate ---
m1_nopar_indisc_bin <- glm(
  indiscriminate_binary ~ no_parent + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_noparent,
  family = binomial("logit")
)
r1_nopar_indisc_bin <- tidy_cluster(m1_nopar_indisc_bin, ~sideb)
r1_nopar_indisc_bin$ct
nobs(m1_nopar_indisc_bin)

# --- Count: Total Variety ---
m1_nopar_total <- glm.nb(
  total_variety ~ no_parent + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent
)
r1_nopar_total <- tidy_cluster(m1_nopar_total, ~sideb)
r1_nopar_total$ct
nobs(m1_nopar_total)

# --- Count: Discriminatory Variety ---
m1_nopar_disc_var <- glm.nb(
  discriminatory_variety ~ no_parent + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent
)
r1_nopar_disc_var <- tidy_cluster(m1_nopar_disc_var, ~sideb)
r1_nopar_disc_var$ct
nobs(m1_nopar_disc_var)

# --- Count: Indiscriminate Variety ---
m1_nopar_indisc_var <- glm.nb(
  indiscriminate_variety ~ no_parent + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent
)
r1_nopar_indisc_var <- tidy_cluster(m1_nopar_indisc_var, ~sideb)
r1_nopar_indisc_var$ct
nobs(m1_nopar_indisc_var)

# --- Count: Discriminatory Severity ---
m1_nopar_disc_sev <- glm.nb(
  discriminatory_severity ~ no_parent + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent
)
r1_nopar_disc_sev <- tidy_cluster(m1_nopar_disc_sev, ~sideb)
r1_nopar_disc_sev$ct
nobs(m1_nopar_disc_sev)

# --- Count: Indiscriminate Severity ---
m1_nopar_indisc_sev <- glm.nb(
  indiscriminate_severity ~ no_parent + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent
)
r1_nopar_indisc_sev <- tidy_cluster(m1_nopar_indisc_sev, ~sideb)
r1_nopar_indisc_sev$ct
nobs(m1_nopar_indisc_sev)

# --- OLS: Violence Diff ---
m1_nopar_diff <- lm(
  violence_diff ~ no_parent + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_noparent %>% filter(!is.na(violence_share_disc))
)
r1_nopar_diff <- tidy_cluster(m1_nopar_diff, ~sideb)
r1_nopar_diff$ct
nobs(m1_nopar_diff)



# ============================================================
# SECTION 2: CIVIL SOCIETY (3-category)
# Categories: Violent vs Civil vs Parentless
# Reference = Violent origin
# Sample: model_data_all (N=294)
# ============================================================

# --- Fractional Logit: Discriminatory Share ---
m2_civil_share <- glm(
  violence_share_disc ~ origin_3cat + log_gdp_pc  + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_all %>% filter(!is.na(violence_share_disc)),
  family = quasibinomial("logit")
)
r2_civil_share <- tidy_cluster(m2_civil_share, ~sideb)
r2_civil_share$ct
nobs(m2_civil_share)


# --- Fractional Logit: Indiscriminate Share ---
m2_civil_indisc <- glm(
  violence_share_indisc ~ origin_3cat + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_all %>% filter(!is.na(violence_share_indisc)),
  family = quasibinomial("logit")
)
r2_civil_indisc <- tidy_cluster(m2_civil_indisc, ~sideb)
r2_civil_indisc$ct
nobs(m2_civil_indisc)

# --- Binomial Composition: cbind (violators only) ---
df_civil_pattern <- model_data_all %>%
  filter(total_variety > 0) %>%
  mutate(
    disc_count   = as.integer(discriminatory_variety),
    indisc_count = as.integer(indiscriminate_variety)
  )

m2_civil_pattern <- glm(
  cbind(disc_count, indisc_count) ~ origin_3cat +
    log_gdp_pc + democracy_z + ideology +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  family = binomial("logit"),
  data   = df_civil_pattern
)
r2_civil_pattern <- tidy_cluster(m2_civil_pattern, ~sideb)
r2_civil_pattern$ct
nobs(m2_civil_pattern)

# --- Binary: Any Violation ---
m2_civil_anyviol <- glm(
  violation_s ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_all,
  family = binomial("logit")
)
r2_civil_anyviol <- tidy_cluster(m2_civil_anyviol, ~sideb)
r2_civil_anyviol$ct
nobs(m2_civil_anyviol)

# --- Binary: Discriminatory ---
m2_civil_disc_bin <- glm(
  discriminatory_binary ~ origin_3cat + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_all,
  family = binomial("logit")
)
r2_civil_disc_bin <- tidy_cluster(m2_civil_disc_bin, ~sideb)
r2_civil_disc_bin$ct
nobs(m2_civil_disc_bin)

# --- Binary: Indiscriminate ---
m2_civil_indisc_bin <- glm(
  indiscriminate_binary ~ origin_3cat + ideology + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_all,
  family = binomial("logit")
)
r2_civil_indisc_bin <- tidy_cluster(m2_civil_indisc_bin, ~sideb)
r2_civil_indisc_bin$ct
nobs(m2_civil_indisc_bin)

# --- Count: Total Variety ---
m2_civil_total <- glm.nb(
  total_variety ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all
)
r2_civil_total <- tidy_cluster(m2_civil_total, ~sideb)
r2_civil_total$ct
nobs(m2_civil_total)

# --- Count: Discriminatory Variety ---
m2_civil_disc_var <- glm.nb(
  discriminatory_variety ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all
)
r2_civil_disc_var <- tidy_cluster(m2_civil_disc_var, ~sideb)
r2_civil_disc_var$ct
nobs(m2_civil_disc_var)

# --- Count: Indiscriminate Variety ---
m2_civil_indisc_var <- glm.nb(
  indiscriminate_variety ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all
)
r2_civil_indisc_var <- tidy_cluster(m2_civil_indisc_var, ~sideb)
r2_civil_indisc_var$ct
nobs(m2_civil_indisc_var)

# --- Count: Discriminatory Severity ---
m2_civil_disc_sev <- glm.nb(
  discriminatory_severity ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all
)
r2_civil_disc_sev <- tidy_cluster(m2_civil_disc_sev, ~sideb)
r2_civil_disc_sev$ct
nobs(m2_civil_disc_sev)

# --- Count: Indiscriminate Severity ---
m2_civil_indisc_sev <- glm.nb(
  indiscriminate_severity ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all
)
r2_civil_indisc_sev <- tidy_cluster(m2_civil_indisc_sev, ~sideb)
r2_civil_indisc_sev$ct
nobs(m2_civil_indisc_sev)

# --- OLS: Violence Diff ---
m2_civil_diff <- lm(
  violence_diff ~ origin_3cat + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_all %>% filter(!is.na(violence_share_disc))
)
r2_civil_diff <- tidy_cluster(m2_civil_diff, ~sideb)
r2_civil_diff$ct
nobs(m2_civil_diff)

# --- Violence Ratio ---
m2_civil_ratio <- glm(
  violence_ratio_smooth ~ origin_3cat + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  family = gaussian("log"),
  data   = model_data_all %>% filter(!is.na(violence_ratio_smooth))
)
tidy_cluster(m2_civil_ratio, ~sideb)$ct
nobs(m2_civil_ratio)


# ============================================================
# SECTION 3: COMMUNITY TIES (3-category)
# Categories: Violent vs Community (civil+party) vs Parentless
# Reference = Violent origin
# Sample: model_data_comm_broad (N=378)
# ============================================================

# --- Fractional Logit: Discriminatory Share ---
m3_comm_share <- glm(
  violence_share_disc ~ origin_comm + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_comm_broad %>% filter(!is.na(violence_share_disc)),
  family = quasibinomial("logit")
)
r3_comm_share <- tidy_cluster(m3_comm_share, ~sideb)
r3_comm_share$ct
nobs(m3_comm_share)

# --- Fractional Logit: Indiscriminate Share ---
m3_comm_indisc <- glm(
  violence_share_indisc ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_comm_broad %>% filter(!is.na(violence_share_indisc)),
  family = quasibinomial("logit")
)
r3_comm_indisc <- tidy_cluster(m3_comm_indisc, ~sideb)
r3_comm_indisc$ct
nobs(m3_comm_indisc)

# --- Binomial Composition: cbind (violators only) ---
df_comm_pattern <- model_data_comm_broad %>%
  filter(total_variety > 0) %>%
  mutate(
    disc_count   = as.integer(discriminatory_variety),
    indisc_count = as.integer(indiscriminate_variety)
  )

m3_comm_pattern <- glm(
  cbind(disc_count, indisc_count) ~ origin_comm +
    log_gdp_pc + democracy_z + ideology +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  family = binomial("logit"),
  data   = df_comm_pattern
)
r3_comm_pattern <- tidy_cluster(m3_comm_pattern, ~sideb)
r3_comm_pattern$ct
nobs(m3_comm_pattern)

# --- Binary: Any Violation ---
m3_comm_anyviol <- glm(
  violation_s ~ origin_comm + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_comm_broad,
  family = binomial("logit")
)
r3_comm_anyviol <- tidy_cluster(m3_comm_anyviol, ~sideb)
r3_comm_anyviol$ct
nobs(m3_comm_anyviol)

# --- Binary: Discriminatory ---
m3_comm_disc_bin <- glm(
  discriminatory_binary ~ origin_comm + log_gdp_pc + democracy_z + ideology +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_comm_broad,
  family = binomial("logit")
)
r3_comm_disc_bin <- tidy_cluster(m3_comm_disc_bin, ~sideb)
r3_comm_disc_bin$ct
nobs(m3_comm_disc_bin)

# --- Binary: Indiscriminate ---
m3_comm_indisc_bin <- glm(
  indiscriminate_binary ~ origin_comm + log_gdp_pc + democracy_z + ideology +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data   = model_data_comm_broad,
  family = binomial("logit")
)
r3_comm_indisc_bin <- tidy_cluster(m3_comm_indisc_bin, ~sideb)
r3_comm_indisc_bin$ct
nobs(m3_comm_indisc_bin)

# --- Count: Total Variety ---
m3_comm_total <- glm.nb(
  total_variety ~ origin_comm + log_gdp_pc + ideology + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad
)
r3_comm_total <- tidy_cluster(m3_comm_total, ~sideb)
r3_comm_total$ct
nobs(m3_comm_total)

# --- Count: Discriminatory Variety ---
m3_comm_disc_var <- glm.nb(
  discriminatory_variety ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad
)
r3_comm_disc_var <- tidy_cluster(m3_comm_disc_var, ~sideb)
r3_comm_disc_var$ct
nobs(m3_comm_disc_var)

# --- Count: Indiscriminate Variety ---
m3_comm_indisc_var <- glm.nb(
  indiscriminate_variety ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad
)
r3_comm_indisc_var <- tidy_cluster(m3_comm_indisc_var, ~sideb)
r3_comm_indisc_var$ct
nobs(m3_comm_indisc_var)

# --- Count: Discriminatory Severity ---
m3_comm_disc_sev <- glm.nb(
  discriminatory_severity ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad
)
r3_comm_disc_sev <- tidy_cluster(m3_comm_disc_sev, ~sideb)
r3_comm_disc_sev$ct
nobs(m3_comm_disc_sev)

# --- Count: Indiscriminate Severity ---
m3_comm_indisc_sev <- glm.nb(
  indiscriminate_severity ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad
)
r3_comm_indisc_sev <- tidy_cluster(m3_comm_indisc_sev, ~sideb)
r3_comm_indisc_sev$ct
nobs(m3_comm_indisc_sev)

# --- OLS: Violence Diff ---
m3_comm_diff <- lm(
  violence_diff ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  data = model_data_comm_broad %>% filter(!is.na(violence_share_disc))
)
r3_comm_diff <- tidy_cluster(m3_comm_diff, ~sideb)
r3_comm_diff$ct
nobs(m3_comm_diff)

# --- Violence Ratio ---
m3_comm_ratio <- glm(
  violence_ratio_smooth ~ origin_comm + log_gdp_pc + democracy_z +
    rebstrength_num + terrcont_num + rebel_support_num +
    intensity + conflict_duration,
  family = gaussian("log"),
  data   = model_data_comm_broad %>% filter(!is.na(violence_ratio_smooth))
)
tidy_cluster(m3_comm_ratio, ~sideb)$ct
nobs(m3_comm_ratio)


# ============================================================
# SECTION 4: DESCRIPTIVE CHECK
# ============================================================

civil_groups <- model_data_all %>%
  filter(origin_3cat == "civil") %>%
  count(sideb) %>%
  arrange(sideb)
print(civil_groups)


# ============================================================
# SECTION 5: PLOTS
# ============================================================

# --- Color and shape aesthetics ---
origin_colors <- c(
  "Civil Society"  = "#E07B6A",
  "Community Ties" = "#4BAEA0",
  "Parentless"     = "#F0A500",
  "Violent Origin" = "#6A5ACD"
)

origin_shapes <- c(
  "Civil Society Specification"  = 16,
  "Community Ties Specification" = 17
)

# --- Build predicted discriminatory share data ---
pred_disc_civil <- predictions(
  m2_civil_share,
  newdata = datagrid(
    origin_3cat       = c("violent", "civil", "noparent"),
    log_gdp_pc        = mean(model_data_all$log_gdp_pc, na.rm = TRUE),
    democracy_z       = mean(model_data_all$democracy_z, na.rm = TRUE),
    ideology          = mean(model_data_all$ideology, na.rm = TRUE),
    rebstrength_num   = mean(model_data_all$rebstrength_num, na.rm = TRUE),
    terrcont_num      = mean(model_data_all$terrcont_num, na.rm = TRUE),
    rebel_support_num = mean(model_data_all$rebel_support_num, na.rm = TRUE),
    intensity         = mean(model_data_all$intensity, na.rm = TRUE),
    conflict_duration = mean(model_data_all$conflict_duration, na.rm = TRUE)
  )
) %>%
  mutate(
    Origin = case_when(
      origin_3cat == "civil"    ~ "Civil Society",
      origin_3cat == "violent"  ~ "Violent Origin",
      origin_3cat == "noparent" ~ "Parentless"
    ),
    Model = "Civil Society Specification"
  )

pred_disc_comm <- predictions(
  m3_comm_share,
  newdata = datagrid(
    origin_comm       = c("violent", "community", "noparent"),
    log_gdp_pc        = mean(model_data_comm_broad$log_gdp_pc, na.rm = TRUE),
    democracy_z       = mean(model_data_comm_broad$democracy_z, na.rm = TRUE),
    ideology          = mean(model_data_comm_broad$ideology, na.rm = TRUE),
    rebstrength_num   = mean(model_data_comm_broad$rebstrength_num, na.rm = TRUE),
    terrcont_num      = mean(model_data_comm_broad$terrcont_num, na.rm = TRUE),
    rebel_support_num = mean(model_data_comm_broad$rebel_support_num, na.rm = TRUE),
    intensity         = mean(model_data_comm_broad$intensity, na.rm = TRUE),
    conflict_duration = mean(model_data_comm_broad$conflict_duration, na.rm = TRUE)
  )
) %>%
  mutate(
    Origin = case_when(
      origin_comm == "community" ~ "Community Ties",
      origin_comm == "violent"   ~ "Violent Origin",
      origin_comm == "noparent"  ~ "Parentless"
    ),
    Model = "Community Ties Specification"
  )

pred_disc <- bind_rows(pred_disc_civil, pred_disc_comm) %>%
  mutate(Origin = factor(Origin, levels = c("Civil Society", "Community Ties",
                                            "Parentless", "Violent Origin")))

# --- Build predicted indiscriminate share data ---
pred_indisc_civil <- predictions(
  m2_civil_indisc,
  newdata = datagrid(
    origin_3cat       = c("violent", "civil", "noparent"),
    log_gdp_pc        = mean(model_data_all$log_gdp_pc, na.rm = TRUE),
    democracy_z       = mean(model_data_all$democracy_z, na.rm = TRUE),
    rebstrength_num   = mean(model_data_all$rebstrength_num, na.rm = TRUE),
    terrcont_num      = mean(model_data_all$terrcont_num, na.rm = TRUE),
    rebel_support_num = mean(model_data_all$rebel_support_num, na.rm = TRUE),
    intensity         = mean(model_data_all$intensity, na.rm = TRUE),
    conflict_duration = mean(model_data_all$conflict_duration, na.rm = TRUE)
  )
) %>%
  mutate(
    Origin = case_when(
      origin_3cat == "civil"    ~ "Civil Society",
      origin_3cat == "violent"  ~ "Violent Origin",
      origin_3cat == "noparent" ~ "Parentless"
    ),
    Model = "Civil Society Specification"
  )

pred_indisc_comm <- predictions(
  m3_comm_indisc,
  newdata = datagrid(
    origin_comm       = c("violent", "community", "noparent"),
    log_gdp_pc        = mean(model_data_comm_broad$log_gdp_pc, na.rm = TRUE),
    democracy_z       = mean(model_data_comm_broad$democracy_z, na.rm = TRUE),
    rebstrength_num   = mean(model_data_comm_broad$rebstrength_num, na.rm = TRUE),
    terrcont_num      = mean(model_data_comm_broad$terrcont_num, na.rm = TRUE),
    rebel_support_num = mean(model_data_comm_broad$rebel_support_num, na.rm = TRUE),
    intensity         = mean(model_data_comm_broad$intensity, na.rm = TRUE),
    conflict_duration = mean(model_data_comm_broad$conflict_duration, na.rm = TRUE)
  )
) %>%
  mutate(
    Origin = case_when(
      origin_comm == "community" ~ "Community Ties",
      origin_comm == "violent"   ~ "Violent Origin",
      origin_comm == "noparent"  ~ "Parentless"
    ),
    Model = "Community Ties Specification"
  )

pred_indisc <- bind_rows(pred_indisc_civil, pred_indisc_comm) %>%
  mutate(Origin = factor(Origin, levels = c("Civil Society", "Community Ties",
                                            "Parentless", "Violent Origin")))

# --- Plot 1: Discriminatory Share ---
p_disc_single <- ggplot(pred_disc,
                        aes(x = Origin, y = estimate,
                            ymin = conf.low, ymax = conf.high,
                            color = Origin, shape = Model)) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.7, linewidth = 0.8) +
  scale_color_manual(values = origin_colors) +
  scale_shape_manual(values = origin_shapes) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format()) +
  labs(
    title   = "Predicted Discriminatory Share by Organizational Origin",
    x       = "Organizational Origin",
    y       = "Predicted Discriminatory Share",
    color   = "Origin Category",
    shape   = "Model Specification",
    caption = "Note: Predictions computed holding all controls at sample means.\nCircle = Civil Society Specification; Triangle = Community Ties Specification."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 8, face = "bold"),
    legend.key.size  = unit(0.7, "lines"),
    plot.title       = element_text(face = "bold", size = 10, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 9),
    plot.caption     = element_text(size = 7, hjust = 0.5),
    plot.margin      = margin(10, 10, 10, 10)
  )

ggsave("predicted_disc_share.png", p_disc_single,
       width = 8, height = 7, dpi = 300)
print(p_disc_single)

# --- Plot 2: Indiscriminate Share ---
p_indisc_single <- ggplot(pred_indisc,
                          aes(x = Origin, y = estimate,
                              ymin = conf.low, ymax = conf.high,
                              color = Origin, shape = Model)) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.7, linewidth = 0.8) +
  scale_color_manual(values = origin_colors) +
  scale_shape_manual(values = origin_shapes) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format()) +
  labs(
    title   = "Predicted Indiscriminate Share by Organizational Origin",
    x       = "Organizational Origin",
    y       = "Predicted Indiscriminate Share",
    color   = "Origin Category",
    shape   = "Model Specification",
    caption = "Note: Predictions computed holding all controls at sample means.\nCircle = Civil Society Specification; Triangle = Community Ties Specification."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 8, face = "bold"),
    legend.key.size  = unit(0.7, "lines"),
    plot.title       = element_text(face = "bold", size = 10, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 9),
    plot.caption     = element_text(size = 7, hjust = 0.5),
    plot.margin      = margin(10, 10, 10, 10)
  )

ggsave("predicted_indisc_share.png", p_indisc_single,
       width = 8, height = 7, dpi = 300)
print(p_indisc_single)

# --- Plot 3: Density Plot — Raw Discriminatory Share ---
df_density <- model_data_comm_broad %>%
  filter(!is.na(violence_share_disc) & !is.na(origin_comm)) %>%
  mutate(Origin = case_when(
    origin_comm == "community" & civil_society == 1 ~ "Civil Society",
    origin_comm == "community" & civil_society == 0 ~ "Political Party",
    origin_comm == "violent"                        ~ "Violent Origin",
    origin_comm == "noparent"                       ~ "Parentless"
  ))

p_density <- ggplot(df_density,
                    aes(x = violence_share_disc,
                        fill = Origin, color = Origin)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 1),
                     labels = scales::percent_format()) +
  labs(
    title   = "Distribution of Discriminatory Violation Share by Organizational Origin",
    x       = "Discriminatory Share of Violation Repertoire",
    y       = "Density",
    fill    = "Origin Category",
    color   = "Origin Category"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 10, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("density_disc_share.png", p_density,
       width = 8, height = 6, dpi = 300)
print(p_density)

# ============================================================
# END OF ANALYSIS
# ============================================================
