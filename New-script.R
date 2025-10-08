# ---- 1. Load Required Libraries ----

library(haven)
library(dplyr)
library(tidyverse)
library(janitor)
library(WDI)
library(ggplot2)
library(broom)
library(lmtest)
library(modelsummary)
library(kableExtra)
library(ggeffects)
library(stargazer)
library(sandwich)
library(AER)
library(clubSandwich)
library(MASS)
library(lubridate)
library(stringr)
library(tibble)
library(countrycode)
library(pscl)
library(marginaleffects)


humanrv <- read_dta("rhr (realfresh).dta") |> as_tibble()
nonstate <- read_dta("nsa_v3.4_21 copy.dta") |> as_tibble()
forge <- read_dta("newforge_v1.0_public.dta") |> as_tibble()
rebleader <- read_dta("ROLE 1.1 Final.dta") |> as_tibble()


nonstate <- nonstate %>% clean_names()
forge <- forge %>% clean_names()


nonstate <- nonstate %>%
  rename(
    dyadid1 = dyadid,
    sideb = side_b # <- use side_b here
  )

forge <- forge %>%
  rename(
    dyadid1 = ns_adyadid
  )




forge_vars <- c(
  "dyadid1",
  "sideb",
  "gname",
  "ccode",
  "cname",
  "conflict_id",
  "foundyear",
  "ideology",
  "preorg",
  "preorgno",
  "preorgreb",
  "preorgter",
  "preorgpar",
  "preorgmvt",
  "preorgyou",
  "preorglab",
  "preorgmil",
  "preorggov",
  "preorgfmr",
  "preorgrel",
  "preorgfor",
  "preorgref",
  "preorgeth",
  "preorgoth",
  "preorgname",
  "merger",
  "splinter"
)

forge_clean <- forge[, forge_vars]
forge_clean <- forge %>% dplyr::select(dplyr::any_of(forge_vars))


#########################################################################




nonstate_keep <- c(
  "dyadid1",
  "sidea",
  "sideb",
  "startdate",
  "enddate",
  "rebestimate",
  "rebestlow",
  "rebesthigh",
  "rebstrength",
  "centcontrol",
  "strengthcent",
  "mobcap",
  "armsproc",
  "fightcap",
  "terrcont",
  "effterrcont",
  "transconstsupp",
  "rebextpart",
  "rebelsupport",
  "govsupport",
  "govextpart",
  "typeoftermination",
  "victoryside"
)


nonstate_clean <- nonstate %>% dplyr::select(dplyr::any_of(nonstate_keep))
nonstate_sel <- nonstate %>%
  dplyr::select(dplyr::any_of(nonstate_keep))


# Rename startdate/enddate in nonstate_sel to avoid conflicts


nonstate_sel <- nonstate_sel %>%
  rename(
    start_ns = startdate, # group-level start date
    end_ns = enddate # group-level end date
  )


# Quick check that rename worked
names(nonstate_sel)[names(nonstate_sel) %in% c("start_ns", "end_ns")]



forge_nonstate <- left_join(
  forge_clean,
  nonstate_sel,
  by = c("sideb", "dyadid1")
) |>
  as_tibble()
for_nonstate <- inner_join(
  forge_clean,
  nonstate_sel,
  by = c("sideb", "dyadid1")
) |>
  as_tibble()


safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
na_str_to_na <- function(x) {
  x <- as.character(x)
  x[x %in% c("NA", "na", "Na", "nA")] <- NA_character_
  x
}
norm_txt <- function(x) str_squish(str_to_lower(x))


#######################################################################



humanrv_clean <- humanrv %>%
  janitor::clean_names() %>%
  dplyr::rename(dyadid1 = dyadid)


hr_final <- left_join(humanrv_clean, forge_nonstate, by = c("sideb")) |>
  as_tibble()



# See the year range

range(hr_final$year, na.rm = TRUE)

# Count observations per conflict
hr_final |>
  count(sideb, sidea.x, name = "years_observed") |>
  arrange(desc(years_observed))

# See which conflicts have the most years of data
hr_final |>
  group_by(sideb, sidea.x) |>
  summarise(
    years = n(),
    year_range = paste(min(year), "-", max(year)),
    .groups = "drop"
  ) |>
  arrange(desc(years))



hr_final_clean <- hr_final |>
  dplyr::select(-sidea.y) |>
  rename(sidea = sidea.x)


# Verifying if it worked

names(hr_final_clean)[grepl("sidea", names(hr_final_clean))]



# Quick overview of the cleaned dataset
glimpse(hr_final_clean)

# Or check the structure
str(hr_final_clean)

# Verify your key variables are intact
hr_final_clean |>
  count(sideb, sidea, name = "years_observed") |>
  arrange(desc(years_observed))




hr_final_clean <- hr_final_clean %>%
  mutate(dyadid1 = dplyr::coalesce(dyadid1.x, dyadid1.y)) %>%
  dplyr::select(-dplyr::any_of(c("dyadid1.x", "dyadid1.y")))


hr_final_clean <- hr_final_clean %>%
  mutate(
    start_ns = as.Date(start_ns),
    end_ns = as.Date(end_ns)
  )




group_start <- hr_final_clean %>%
  group_by(dyadid1, sidea, sideb) %>%
  summarise(
    min_start_ns = suppressWarnings(min(start_ns, na.rm = TRUE)),
    first_year = suppressWarnings(min(year, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Vectorized clean-up: if min_start_ns is Inf (all missing), set to NA
  mutate(
    min_start_ns = as.Date(ifelse(
      is.finite(as.numeric(min_start_ns)),
      min_start_ns,
      NA
    ))
  ) %>%
  mutate(
    # Use earliest non-missing start_ns; otherwise fallback to Jan 1 of first observed year
    start_group = coalesce(
      min_start_ns,
      as.Date(sprintf("%d-01-01", first_year))
    )
  ) %>%
  dplyr::select(dyadid1, sidea, sideb, start_group)



hr_final_clean <- hr_final_clean %>%
  left_join(group_start, by = c("dyadid1", "sidea", "sideb")) %>%
  mutate(
    obs_end_of_year = as.Date(sprintf("%d-12-31", year)),

    # ongoing duration = years since group started (measured at end of this observation year)
    duration_years_ongoing = as.numeric(
      pmax(obs_end_of_year, start_group) - start_group
    ) /
      365.25,

    # total duration only if we know the end date
    conflict_duration_days = as.numeric(ifelse(
      !is.na(end_ns),
      end_ns - start_group,
      NA
    )),
    conflict_duration_years = conflict_duration_days / 365.25
  )



analysis_df <- hr_final_clean %>% filter(!is.na(duration_years_ongoing))




###########################################################################



humanrv_aug <- hr_final_clean %>%
  mutate(
    sidea_std = str_squish(sidea),
    iso3c = countrycode(sidea_std, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3c = case_when(
      sidea_std %in%
        c(
          "Congo (Kinshasa)",
          "DR Congo",
          "Democratic Republic of the Congo",
          "Congo, Dem. Rep.",
          "Congo, Democratic Republic of"
        ) ~
        "COD",
      sidea_std %in%
        c("Congo (Brazzaville)", "Republic of the Congo", "Congo") ~
        "COG",
      sidea_std %in%
        c("Ivory Coast", "Cote d'Ivoire", "Côte d’Ivoire", "Côte d'Ivoire") ~
        "CIV",
      sidea_std %in% c("Eswatini", "Swaziland") ~ "SWZ",
      sidea_std %in% c("Burma", "Myanmar") ~ "MMR",
      sidea_std %in% c("Russia", "Russian Federation") ~ "RUS",
      sidea_std %in% c("Iran", "Iran (Islamic Republic of)") ~ "IRN",
      sidea_std %in% c("Syria", "Syrian Arab Republic") ~ "SYR",
      sidea_std %in% c("Laos", "Lao PDR", "Lao People's Democratic Republic") ~
        "LAO",
      sidea_std %in%
        c("Yemen (North Yemen)", "North Yemen", "Yemen Arab Republic") ~
        "YEM",
      TRUE ~ iso3c
    )
  )


# sanity check: must have ISO3 for all rows
if (anyNA(humanrv_aug$iso3c)) {
  stop(
    "Some ISO3C codes are missing. Inspect with: humanrv_aug %>% filter(is.na(iso3c)) %>% distinct(sidea_std)"
  )
}


## 2) WDI GDP per capita (constant 2015 US$) --------------------------------
wdi_raw <- WDI(
  country = "all",
  indicator = c(gdppc_const2015 = "NY.GDP.PCAP.KD"),
  start = 1990,
  end = 2023,
  extra = TRUE
)


wdi_clean <- wdi_raw %>%
  transmute(iso3c = iso3c, year = year, gdppc_const2015) %>%
  filter(!is.na(iso3c), !is.na(year))

# merge GDP → panel
hr_with_gdp <- humanrv_aug %>%
  left_join(wdi_clean, by = c("iso3c", "year"))

message(sprintf(
  "WDI GDP coverage before fill: %.1f%%",
  mean(!is.na(hr_with_gdp$gdppc_const2015)) * 100
))

## 3) Fill short GDP gaps (linear interpolation, no warnings)
wdi_series <- hr_with_gdp %>%
  distinct(iso3c, year, gdppc_const2015) %>%
  arrange(iso3c, year)

wdi_filled <- wdi_series %>%
  group_by(iso3c) %>%
  mutate(
    gdppc_const2015_filled = {
      y <- gdppc_const2015
      x <- year
      if (sum(!is.na(y)) >= 2) {
        approx(
          x = x[!is.na(y)],
          y = y[!is.na(y)],
          xout = x,
          method = "linear",
          rule = 2
        )$y
      } else {
        y
      }
    }
  ) %>%
  ungroup()



hr_with_gdp_filled <- hr_with_gdp %>%
  left_join(
    wdi_filled %>% dplyr::select(iso3c, year, gdppc_const2015_filled),
    by = c("iso3c", "year")
  )

# Coverage check
hr_with_gdp_filled %>%
  summarise(
    total_rows = n(),
    coverage = mean(!is.na(gdppc_const2015_filled)) * 100
  )





## 4) Read V-Dem (Stata .dta), convert COW → ISO3C --------------------------


vdem_raw <- read_dta("V-Dem-CY-Core-v15.dta")


vdem_keep <- vdem_raw %>%
  dplyr::select(year, COWcode, v2x_polyarchy) %>%
  mutate(
    year = as.integer(year),
    cown = suppressWarnings(as.integer(COWcode)),
    iso3c = countrycode(cown, "cown", "iso3c"),
    v2x_polyarchy = as.numeric(v2x_polyarchy)
  ) %>%
  filter(!is.na(iso3c), !is.na(year)) %>%
  dplyr::select(year, iso3c, v2x_polyarchy)

## 5) Merge democracy + create modeling vars --------------------------------
hr_model_ready <- hr_with_gdp_filled %>%
  left_join(vdem_keep, by = c("iso3c", "year")) %>%
  mutate(
    # log GDP per capita (filled)
    log_rgdppc = log(gdppc_const2015_filled),

    # democracy (primary): V-Dem Electoral Democracy Index
    vdem_edi = v2x_polyarchy,

    # standardized democracy (z-score)
    vdem_edi_z = as.numeric(scale(vdem_edi)),

    # optional democracy dummy for robustness (threshold 0.5)
    dem_dummy_vdem = if_else(!is.na(vdem_edi) & vdem_edi >= 0.5, 1L, 0L)
  ) %>%
  relocate(
    sidea,
    sideb,
    year,
    iso3c,
    log_rgdppc,
    vdem_edi,
    vdem_edi_z,
    dem_dummy_vdem
  )



## 6) Diagnostics ------------------------------------------------------------
print(summary(hr_model_ready[, c(
  "gdppc_const2015_filled",
  "log_rgdppc",
  "vdem_edi",
  "vdem_edi_z"
)]))

diag <- hr_model_ready %>%
  summarise(
    rows = n(),
    gdp_nonmissing = sum(!is.na(gdppc_const2015_filled)),
    vdem_nonmiss = sum(!is.na(vdem_edi)),
    gdp_cover_pct = mean(!is.na(gdppc_const2015_filled)) * 100,
    vdem_cover_pct = mean(!is.na(vdem_edi)) * 100
  )
print(diag)






# From the combined dataset
full_model <- hr_model_ready

# drop the duplicate ".y" cols if present
full_model <- full_model %>%
  dplyr::select(-tidyselect::any_of(c("dyadid1.y", "startdate.y", "sidea.y")))

# conditionally rename ".x" → clean names (only if those columns exist)
if ("dyadid1.x" %in% names(full_model)) {
  full_model <- dplyr::rename(full_model, dyadid1 = dyadid1.x)
}
if ("startdate.x" %in% names(full_model)) {
  full_model <- dplyr::rename(full_model, startdate = startdate.x)
}
if ("sidea.x" %in% names(full_model)) {
  full_model <- dplyr::rename(full_model, sidea = sidea.x)
}



# safety: required keys must exist now
stopifnot(all(c("sidea", "sideb", "year", "dyadid1") %in% names(full_model)))

# rows & duplicate-key check
message(sprintf("Rows in full_model (after cleanup): %s", nrow(full_model)))

dupe_key <- full_model %>%
  dplyr::count(sidea, sideb, year, name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(dupe_key) > 0) {
  message(
    "WARNING: Duplicate (sidea, sideb, year) rows detected. Showing first 10:"
  )
  print(head(dupe_key, 10))
} else {
  message("OK: No duplicate (sidea, sideb, year) rows.")
}

# coverage diagnostics
diag <- full_model %>%
  dplyr::summarise(
    rows = dplyr::n(),
    forge_match_pct = mean(!is.na(gname)) * 100,
    gdp_cover_pct = mean(!is.na(gdppc_const2015_filled)) * 100,
    vdem_cover_pct = mean(!is.na(v2x_polyarchy)) * 100
  )
print(diag)





unmatched <- full_model %>% filter(is.na(gname))
matched <- full_model %>% filter(!is.na(gname))



full_model <- full_model %>%
  mutate(
    # Add the violation scores first
    count_violations = rowSums(
      across(c(
        rkillings_s,
        rtorture_s,
        rdetention_s,
        rproperty_s,
        rrecruitment_s,
        rsexual_s,
        rdisplace_s,
        rrestrict_s
      )),
      na.rm = TRUE
    ),

    discriminatory_score = rowSums(
      across(c(
        rkillings_s,
        rdetention_s,
        rtorture_s,
        rrecruitment_s,
        rsexual_s
      )),
      na.rm = TRUE
    ),

    indiscriminate_score = rowSums(
      across(c(
        rproperty_s,
        rdisplace_s,
        rrestrict_s
      )),
      na.rm = TRUE
    ),

    # Organizational categories
    civil_society = as.integer(
      preorgmvt == 1 | preorgyou == 1 | preorglab == 1 | preorgrel == 1
    ),
    no_parorg = as.integer(
      preorgeth == 1 | preorgref == 1 | preorgno == 1 | preorgoth == 1
    ),
    violent_parent = as.integer(
      preorgmil == 1 |
        preorgfmr == 1 |
        preorgfor == 1 |
        preorgter == 1 |
        preorggov == 1 |
        preorgreb == 1
    ),
  )



normalize_chr <- function(x) tolower(trimws(as.character(haven::as_factor(x))))

full_model <- full_model %>%
  mutate(
    rebelsupport_chr = normalize_chr(rebelsupport),
    rebstrength_chr = normalize_chr(rebstrength),
    fightcap_chr = normalize_chr(fightcap),
    terrcont_chr = normalize_chr(terrcont),
    centcontrol_chr = normalize_chr(centcontrol)
  ) %>%
  mutate(
    rebelsupport_binary = ifelse(grepl("explicit", rebelsupport_chr), 1L, 0L),

    rebstrength_binary = ifelse(
      rebstrength_chr %in% c("stronger", "much stronger"),
      1L,
      ifelse(
        rebstrength_chr %in% c("weaker", "much weaker", "parity"),
        0L,
        NA_integer_
      )
    ),

    fightcap_binary = ifelse(
      fightcap_chr %in% c("moderate", "high"),
      1L,
      ifelse(fightcap_chr %in% c("low"), 0L, NA_integer_)
    ),

    terrcont_binary = ifelse(
      terrcont_chr %in% c("yes", "present", "1"),
      1L,
      ifelse(terrcont_chr %in% c("no", "0"), 0L, NA_integer_)
    ),

    centcontrol_binary = ifelse(
      centcontrol_chr %in% c("yes", "present", "1"),
      1L,
      ifelse(centcontrol_chr %in% c("no", "0"), 0L, NA_integer_)
    )
  )


lapply(
  c(
    "rebelsupport_binary",
    "rebstrength_binary",
    "fightcap_binary",
    "terrcont_binary",
    "centcontrol_binary"
  ),
  \(v) table(full_model[[v]], useNA = "ifany")
)


full_model <- full_model %>% mutate(violation_s = as.integer(violation_s))


##########################################################################

# Filter to complete cases for all variables

hr_model_corrected <- full_model %>%
  filter(if_all(
    c(
      conflictid,
      violation_s,
      no_parorg,
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc
    ),
    ~ !is.na(.)
  ))

# Check sample size
nrow(hr_model_corrected)



# Load required libraries

# Descriptive statistics table
descriptive_stats <- hr_model_corrected %>%
  summarise(
    # Sum of violations (count_violations)
    n_count_viol = sum(!is.na(count_violations)),
    mean_count_viol = round(mean(count_violations, na.rm = TRUE), 3),
    median_count_viol = median(count_violations, na.rm = TRUE),
    sd_count_viol = round(sd(count_violations, na.rm = TRUE), 3),
    
    # Violation binary (violation_a - assuming this is your binary violation measure)
    n_viol_bin = sum(!is.na(violation_a)),
    mean_viol_bin = round(mean(violation_a, na.rm = TRUE), 3),
    median_viol_bin = median(violation_a, na.rm = TRUE),
    sd_viol_bin = round(sd(violation_a, na.rm = TRUE), 3),
    
    # No Parent Organization
    n_no_parorg = sum(!is.na(no_parorg)),
    mean_no_parorg = round(mean(no_parorg, na.rm = TRUE), 3),
    median_no_parorg = median(no_parorg, na.rm = TRUE),
    sd_no_parorg = round(sd(no_parorg, na.rm = TRUE), 3),
    
    # Civil Society
    n_civil_soc = sum(!is.na(civil_society)),
    mean_civil_soc = round(mean(civil_society, na.rm = TRUE), 3),
    median_civil_soc = median(civil_society, na.rm = TRUE),
    sd_civil_soc = round(sd(civil_society, na.rm = TRUE), 3),
    
    # Rebel Support Binary
    n_rebel_supp = sum(!is.na(rebelsupport_binary)),
    mean_rebel_supp = round(mean(rebelsupport_binary, na.rm = TRUE), 3),
    median_rebel_supp = median(rebelsupport_binary, na.rm = TRUE),
    sd_rebel_supp = round(sd(rebelsupport_binary, na.rm = TRUE), 3),
    
    # Territorial Control
    n_terr_cont = sum(!is.na(terrcont_binary)),
    mean_terr_cont = round(mean(terrcont_binary, na.rm = TRUE), 3),
    median_terr_cont = median(terrcont_binary, na.rm = TRUE),
    sd_terr_cont = round(sd(terrcont_binary, na.rm = TRUE), 3),
    
    # Fighting Capacity
    n_fight_cap = sum(!is.na(fightcap_binary)),
    mean_fight_cap = round(mean(fightcap_binary, na.rm = TRUE), 3),
    median_fight_cap = median(fightcap_binary, na.rm = TRUE),
    sd_fight_cap = round(sd(fightcap_binary, na.rm = TRUE), 3),
    
    # Rebel Strength
    n_reb_str = sum(!is.na(rebstrength_binary)),
    mean_reb_str = round(mean(rebstrength_binary, na.rm = TRUE), 3),
    median_reb_str = median(rebstrength_binary, na.rm = TRUE),
    sd_reb_str = round(sd(rebstrength_binary, na.rm = TRUE), 3),
    
    # Ideology
    n_ideology = sum(!is.na(ideology)),
    mean_ideology = round(mean(ideology, na.rm = TRUE), 3),
    median_ideology = median(ideology, na.rm = TRUE),
    sd_ideology = round(sd(ideology, na.rm = TRUE), 3),
    
    # Merger
    n_merger = sum(!is.na(merger)),
    mean_merger = round(mean(merger, na.rm = TRUE), 3),
    median_merger = median(merger, na.rm = TRUE),
    sd_merger = round(sd(merger, na.rm = TRUE), 3),
    
    # Intensity
    n_intensity = sum(!is.na(intensity)),
    mean_intensity = round(mean(intensity, na.rm = TRUE), 3),
    median_intensity = median(intensity, na.rm = TRUE),
    sd_intensity = round(sd(intensity, na.rm = TRUE), 3),
    
    # Conflict Duration (years)
    n_duration = sum(!is.na(conflict_duration_years)),
    mean_duration = round(mean(conflict_duration_years, na.rm = TRUE), 3),
    median_duration = median(conflict_duration_years, na.rm = TRUE),
    sd_duration = round(sd(conflict_duration_years, na.rm = TRUE), 3),
    
    # Polity (v2x_polyarchy)
    n_polity = sum(!is.na(v2x_polyarchy)),
    mean_polity = round(mean(v2x_polyarchy, na.rm = TRUE), 3),
    median_polity = median(v2x_polyarchy, na.rm = TRUE),
    sd_polity = round(sd(v2x_polyarchy, na.rm = TRUE), 3),
    
    # Log GDPPC
    n_gdp = sum(!is.na(log_rgdppc)),
    mean_gdp = round(mean(log_rgdppc, na.rm = TRUE), 3),
    median_gdp = median(log_rgdppc, na.rm = TRUE),
    sd_gdp = round(sd(log_rgdppc, na.rm = TRUE), 3)
  )

# Create a formatted table
variables <- c("Sum of Violations", "Violation (Binary)", "No Parent Org.", "Civil Society",
               "Rebel Support Binary", "Territorial Control", "Fighting Capacity", 
               "Rebel Strength", "Ideology", "Merger", "Intensity", 
               "Conflict Duration (years)", "Polity", "Log GDPPC")

n_values <- c(descriptive_stats$n_count_viol, descriptive_stats$n_viol_bin, 
              descriptive_stats$n_no_parorg, descriptive_stats$n_civil_soc,
              descriptive_stats$n_rebel_supp, descriptive_stats$n_terr_cont,
              descriptive_stats$n_fight_cap, descriptive_stats$n_reb_str,
              descriptive_stats$n_ideology, descriptive_stats$n_merger,
              descriptive_stats$n_intensity, descriptive_stats$n_duration,
              descriptive_stats$n_polity, descriptive_stats$n_gdp)

means <- c(descriptive_stats$mean_count_viol, descriptive_stats$mean_viol_bin,
           descriptive_stats$mean_no_parorg, descriptive_stats$mean_civil_soc,
           descriptive_stats$mean_rebel_supp, descriptive_stats$mean_terr_cont,
           descriptive_stats$mean_fight_cap, descriptive_stats$mean_reb_str,
           descriptive_stats$mean_ideology, descriptive_stats$mean_merger,
           descriptive_stats$mean_intensity, descriptive_stats$mean_duration,
           descriptive_stats$mean_polity, descriptive_stats$mean_gdp)

medians <- c(descriptive_stats$median_count_viol, descriptive_stats$median_viol_bin,
             descriptive_stats$median_no_parorg, descriptive_stats$median_civil_soc,
             descriptive_stats$median_rebel_supp, descriptive_stats$median_terr_cont,
             descriptive_stats$median_fight_cap, descriptive_stats$median_reb_str,
             descriptive_stats$median_ideology, descriptive_stats$median_merger,
             descriptive_stats$median_intensity, descriptive_stats$median_duration,
             descriptive_stats$median_polity, descriptive_stats$median_gdp)

sds <- c(descriptive_stats$sd_count_viol, descriptive_stats$sd_viol_bin,
         descriptive_stats$sd_no_parorg, descriptive_stats$sd_civil_soc,
         descriptive_stats$sd_rebel_supp, descriptive_stats$sd_terr_cont,
         descriptive_stats$sd_fight_cap, descriptive_stats$sd_reb_str,
         descriptive_stats$sd_ideology, descriptive_stats$sd_merger,
         descriptive_stats$sd_intensity, descriptive_stats$sd_duration,
         descriptive_stats$sd_polity, descriptive_stats$sd_gdp)

# Create final table
descriptive_table <- data.frame(
  Variable = variables,
  N = n_values,
  Mean = means,
  Median = medians,
  SD = sds
)

print(descriptive_table)



# Fit logit model
logit_model_corrected <- glm(
  violation_s ~
    no_parorg +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc,
  data = hr_model_corrected,
  family = binomial()
)

# Clustered standard errors by conflictid
cluster_se_corrected <- vcovCL(
  logit_model_corrected,
  cluster = hr_model_corrected$conflictid
)

# Results with clustered SEs
coeftest(logit_model_corrected, vcov = cluster_se_corrected)


### Negative Binomial Model ###
hr_model_nreg <- full_model %>% # Fixed: removed "name" and used correct dataset
  filter(if_all(
    c(
      conflictid,
      count_violations,
      no_parorg, # Fixed: conflictid instead of conflict_id
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc # Fixed: consistent variables
    ),
    ~ !is.na(.) # Fixed: proper handling for all data types
  ))


nreg_model <- glm.nb(
  count_violations ~
    no_parorg +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc, # Fixed: consistent variables
  data = hr_model_nreg,
  control = glm.control(maxit = 500)
)


cluster_se_nreg <- vcovCL(nreg_model, cluster = hr_model_nreg$conflictid) # Fixed: conflictid
coeftest(nreg_model, vcov = cluster_se_nreg)

# Check sample size
nrow(hr_model_nreg)


### Negative Binomial Model ###
hr_model_nreg <- full_model %>% # Fixed: removed "name" and used correct dataset
  filter(if_all(
    c(
      conflictid,
      count_violations,
      no_parorg, # Fixed: conflictid instead of conflict_id
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc # Fixed: consistent variables
    ),
    ~ !is.na(.) # Fixed: proper handling for all data types
  ))


nreg_model <- glm.nb(
  count_violations ~
    no_parorg +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc, # Fixed: consistent variables
  data = hr_model_nreg,
  control = glm.control(maxit = 500)
)


cluster_se_nreg <- vcovCL(nreg_model, cluster = hr_model_nreg$conflictid) # Fixed: conflictid
coeftest(nreg_model, vcov = cluster_se_nreg)

# Check sample size
nrow(hr_model_nreg)


# Calculating Predicted Counts


# Step 1: Run the model with clustered standard errors
nreg_model_no_parorg <- glm.nb(count_violations ~ no_parorg + rebelsupport_binary + 
                                 terrcont_binary + fightcap_binary + rebstrength_binary + 
                                 ideology + merger + intensity + duration_years_ongoing + 
                                 vdem_edi + log_rgdppc, 
                               data = hr_model_corrected)

# Calculate clustered standard errors
clustered_se <- coeftest(nreg_model_no_parorg, 
                         vcov = vcovCL(nreg_model_no_parorg, 
                                       cluster = hr_model_corrected$conflictid))

# Step 2: PREDICTED COUNTS
hr_model_corrected$predicted_counts <- predict(nreg_model_no_parorg, type = "response")

# Calculate average predicted counts by group
predicted_summary <- hr_model_corrected %>%
  group_by(no_parorg) %>%
  summarise(
    Mean_Predicted_Count = mean(predicted_counts, na.rm = TRUE),
    Count = n()
  )

# Plot predicted counts
plot_predicted <- ggplot(predicted_summary, aes(x = factor(no_parorg), y = Mean_Predicted_Count)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = round(Mean_Predicted_Count, 2)), vjust = -0.5, size = 4) +
  scale_x_discrete(labels = c("Has Parent Org", "No Parent Org")) +
  labs(title = "Predicted Count of Violations by Parent Organization Status",
       x = "Parent Organization Status",
       y = "Expected Count of Violations") +
  theme_minimal(base_size = 14)

print(plot_predicted)

# Step 3: MARGINAL EFFECTS
marginal_effects <- margins(nreg_model_no_parorg, variables = "no_parorg")
marginal_summary <- summary(marginal_effects)

# Extract values for marginal effects plot
me_value <- marginal_summary$AME
lower_ci <- marginal_summary$lower
upper_ci <- marginal_summary$upper

# Plot marginal effects with confidence intervals
plot_marginal <- ggplot(data.frame(
  variable = "No Parent Organization",
  effect = me_value,
  lower = lower_ci,
  upper = upper_ci
), aes(x = variable, y = effect)) +
  geom_point(size = 4, color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Marginal Effect of No Parent Organization",
       subtitle = "on Expected Count of Human Rights Violations",
       x = "", 
       y = "Change in Expected Count",
       caption = "Error bars show 95% confidence intervals") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )

print(plot_marginal)

# Print summaries
cat("=== PREDICTED COUNTS SUMMARY ===\n")
print(predicted_summary)

cat("\n=== MARGINAL EFFECTS SUMMARY ===\n")
print(marginal_summary)

cat("\nInterpretation:\n")
cat("- Groups with parent org expect", round(predicted_summary$Mean_Predicted_Count[1], 2), "violations\n")
cat("- Groups without parent org expect", round(predicted_summary$Mean_Predicted_Count[2], 2), "violations\n")
cat("- Marginal effect:", round(me_value, 3), "change in expected violations\n")




























###### ZINB model (count and zero-inflation) #####
hr_model_zinb <- full_model %>% # Fixed: use correct dataset name
  filter(if_all(
    c(
      conflictid,
      count_violations,
      no_parorg, # Fixed: added conflictid, consistent clustering
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc # Fixed: consistent variables
    ),
    ~ !is.na(.) # Fixed: handle all data types
  ))


zinb_model <- zeroinfl(
  count_violations ~
    no_parorg +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc | # Fixed: consistent variables

      no_parorg +
        rebelsupport_binary +
        terrcont_binary +
        fightcap_binary +
        rebstrength_binary +
        ideology +
        merger +
        intensity +
        duration_years_ongoing +
        vdem_edi +
        log_rgdppc, # Fixed: consistent variables

  data = hr_model_zinb,
  dist = "negbin"
)

#### Step-by-step: Cluster-robust SE for a ZINB model
summary(zinb_model)


# Cluster variable (must be the same length as data)
cluster_var <- hr_model_zinb$conflictid # Fixed: consistent clustering variable

# Compute clustered variance-covariance matrix
cluster_vcov <- sandwich::vcovCL(zinb_model, cluster = cluster_var)

# Use coeftest to display results with clustered SEs
coeftest(zinb_model, vcov = cluster_vcov)

# Check sample size
nrow(hr_model_zinb)




#### Civil Society Models #####



hr_model_cs_logit <- full_model %>%
  filter(if_all(
    c(
      conflictid,
      violation_s,
      civil_society,
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc
    ),
    ~ !is.na(.)
  ))


# Fit logit model
logit_model_cs <- glm(
  violation_s ~
    civil_society +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc,
  data = hr_model_cs_logit,
  family = binomial()
)


cluster_se_cs_logit <- vcovCL(
  logit_model_cs,
  cluster = hr_model_cs_logit$conflictid
)
coeftest(logit_model_cs, vcov = cluster_se_cs_logit)


# Check sample size
nrow(hr_model_cs_logit)




#  2. Negative Binomial Model:
# Filter to complete cases



hr_model_cs_nreg <- full_model %>%
  filter(if_all(
    c(
      conflictid,
      count_violations,
      civil_society,
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc
    ),
    ~ !is.na(.)
  ))


nreg_model_cs <- glm.nb(
  count_violations ~
    civil_society +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc,
  data = hr_model_cs_nreg,
  control = glm.control(maxit = 500)
)


cluster_se_cs_nreg <- vcovCL(
  nreg_model_cs,
  cluster = hr_model_cs_nreg$conflictid
)
coeftest(nreg_model_cs, vcov = cluster_se_cs_nreg)

# Check sample size
nrow(hr_model_cs_nreg)





# Predicted expected count of violations (μ_i) for each observation

# Use consistent dataset
hr_model_cs_nreg$predicted_counts <- predict(nreg_model_cs, type = "response")
hr_model_cs_nreg %>%
  group_by(civil_society) %>%
  summarise(
    Mean_Predicted_Count = mean(predicted_counts, na.rm = TRUE),
    Count = n()
  )


cs_nb_pred <- ggpredict(nreg_model_cs, terms = "civil_society [0,1]")


p_nb <- plot(cs_nb_pred) +
  labs(
    title = "Predicted Number of Violations by Civil Society Origin",
    x = "Civil Society",
    y = "Expected Count of Violations"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12)  # Adjust title font size here
  )

# Show the plot
print(p_nb)

# Save the plot
ggsave("predicted_counts_nbreg_plot1.png", plot = p_nb, width = 6, height = 4, dpi = 300)




# Marginal Effect

# Calculate marginal effect with confidence intervals
civil_society_marginal <- margins(nreg_model_cs, variables = "civil_society")
civil_society_summary <- summary(civil_society_marginal)

# Extract values for plotting
me_value <- civil_society_summary$AME
lower_ci <- civil_society_summary$lower
upper_ci <- civil_society_summary$upper



# Create a more informative plot with confidence intervals

ggplot(data.frame(
  variable = "Civil Society",
  effect = me_value,
  lower = lower_ci,
  upper = upper_ci
), aes(x = variable, y = effect)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Marginal Effect of Civil Society Background",
    subtitle = "on Expected Count of Human Rights Violations",
    x = "",
    y = "Change in Expected Count",
    caption = "Error bars show 95% confidence intervals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10)
  ) +
  ylim(min(lower_ci) - 0.05, max(upper_ci) + 0.05)



ggsave("civ_effect_nbreg_plot1.png", plot = p_nb, width = 6, height = 4, dpi = 300)

print(civil_society_summary)



# Step 1: Get predicted counts by civil society group
predicted_summary <- hr_model_cs_nreg %>%
  group_by(civil_society) %>%
  summarise(
    Mean_Predicted_Count = mean(predicted_counts, na.rm = TRUE),
    Count = n()
  )

print(predicted_summary)

# Step 2: Extract the values for calculation
non_civil_society_count <- predicted_summary$Mean_Predicted_Count[predicted_summary$civil_society == 0]
civil_society_count <- predicted_summary$Mean_Predicted_Count[predicted_summary$civil_society == 1]

# Step 3: Calculate percentage reduction
percentage_reduction <- ((non_civil_society_count - civil_society_count) / non_civil_society_count) * 100

# Step 4: Print results
cat("=== PERCENTAGE REDUCTION CALCULATION ===\n")
cat("Non-civil society groups:", round(non_civil_society_count, 2), "expected violations\n")
cat("Civil society groups:", round(civil_society_count, 2), "expected violations\n")
cat("Absolute difference (marginal effect):", round(non_civil_society_count - civil_society_count, 2), "\n")
cat("Percentage reduction:", round(percentage_reduction, 1), "%\n")

# Step 5: Verify this matches your marginal effect
cat("Marginal effect from margins package:", round(me_value, 2), "\n")
cat("Manual calculation difference:", round(non_civil_society_count - civil_society_count, 2), "\n")



# Calculate marginal effect with confidence intervals
civil_society_marginal <- margins(nreg_model_cs, variables = "civil_society")
civil_society_summary <- summary(civil_society_marginal)

# Extract values for plotting
me_value <- civil_society_summary$AME
lower_ci <- civil_society_summary$lower
upper_ci <- civil_society_summary$upper

# Create the marginal effects plot
marginal_plot <- ggplot(data.frame(
  variable = "Civil Society",
  effect = me_value,
  lower = lower_ci,
  upper = upper_ci
), aes(x = variable, y = effect)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Marginal Effect of Civil Society Background",
    subtitle = "on Expected Count of Human Rights Violations",
    x = "",
    y = "Change in Expected Count",
    caption = "Error bars show 95% confidence intervals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10)
  ) +
  ylim(min(lower_ci) - 0.05, max(upper_ci) + 0.05)

# Display the plot
print(marginal_plot)

# Save the plot
ggsave("marginal_effect_civil_society.png", plot = marginal_plot, 
       width = 8, height = 6, dpi = 300)











## 3. Zero-Inflated Negative Binomial Model:

# Filter to complete cases
hr_model_cs_zinb <- full_model %>%
  filter(if_all(
    c(
      conflictid,
      count_violations,
      civil_society,
      rebelsupport_binary,
      terrcont_binary,
      fightcap_binary,
      rebstrength_binary,
      ideology,
      merger,
      intensity,
      duration_years_ongoing,
      vdem_edi,
      log_rgdppc
    ),
    ~ !is.na(.)
  ))




zinb_model_cs <- zeroinfl(
  count_violations ~
    civil_society +
      rebelsupport_binary +
      terrcont_binary +
      fightcap_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc |

      civil_society +
        rebelsupport_binary +
        terrcont_binary +
        fightcap_binary +
        rebstrength_binary +
        ideology +
        merger +
        intensity +
        duration_years_ongoing +
        vdem_edi +
        log_rgdppc,

  data = hr_model_cs_zinb,
  dist = "negbin"
)

summary(zinb_model_cs)


cluster_var_cs <- hr_model_cs_zinb$conflictid
cluster_vcov_cs <- sandwich::vcovCL(zinb_model_cs, cluster = cluster_var_cs)
coeftest(zinb_model_cs, vcov = cluster_vcov_cs)

# Check sample size
nrow(hr_model_cs_zinb)





# Negative binomial with interaction
nreg_model_interact_cs <- glm.nb(
  count_violations ~
    civil_society *
      fightcap_binary +
      rebelsupport_binary +
      terrcont_binary +
      rebstrength_binary +
      ideology +
      merger +
      intensity +
      duration_years_ongoing +
      vdem_edi +
      log_rgdppc,
  data = hr_model_cs_nreg, # Use the correctly filtered dataset
  control = glm.control(maxit = 500)
)

# Clustered SEs
cluster_se_nreg_interact <- vcovCL(
  nreg_model_interact_cs,
  cluster = hr_model_cs_nreg$conflictid
)
nreg_interact_results <- coeftest(
  nreg_model_interact_cs,
  vcov = cluster_se_nreg_interact
)
print(nreg_interact_results)

# Marginal effects of civil_society at each level of fightcap_binary
library(margins)
mfx_interact <- margins(
  nreg_model_interact_cs,
  variables = "civil_society",
  at = list(fightcap_binary = c(0, 1)),
  vcov = cluster_se_nreg_interact
)

# Format for plotting
mfx_df <- summary(mfx_interact)
print(mfx_df)
str(mfx_df)


mfx_plot_data <- data.frame(
  FightCap = factor(
    c(0, 1),
    levels = c(0, 1),
    labels = c("No Fight Capacity", "Has Fight Capacity")
  ),
  AME = mfx_df$AME,
  SE = mfx_df$SE,
  lower = mfx_df$lower,
  upper = mfx_df$upper
)
ggplot(mfx_plot_data, aes(x = FightCap, y = AME)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Marginal Effect of Civil Society by Fight Capacity",
    x = "Rebel Fight Capacity",
    y = "Average Marginal Effect"
  ) +
  theme_minimal(base_size = 14)


# Check sample size
nrow(hr_model_cs_nreg)


#Plots

# Calculate violation percentages by organizational type
violation_summary <- full_model %>%
  filter(!is.na(count_violations), !is.na(no_parorg), !is.na(civil_society)) %>%
  summarise(
    # Total violations by no_parorg groups
    violations_no_parorg = sum(count_violations[no_parorg == 1], na.rm = TRUE),
    violations_has_parorg = sum(count_violations[no_parorg == 0], na.rm = TRUE),

    # Total violations by civil_society groups
    violations_civil_society = sum(
      count_violations[civil_society == 1],
      na.rm = TRUE
    ),
    violations_not_civil_society = sum(
      count_violations[civil_society == 0],
      na.rm = TRUE
    ),

    # Total violations overall
    total_violations = sum(count_violations, na.rm = TRUE)
  ) %>%
  mutate(
    # Calculate percentages
    pct_no_parorg = (violations_no_parorg / total_violations) * 100,
    pct_has_parorg = (violations_has_parorg / total_violations) * 100,
    pct_civil_society = (violations_civil_society / total_violations) * 100,
    pct_not_civil_society = (violations_not_civil_society / total_violations) *
      100
  )

# Create data for plotting
plot_data <- data.frame(
  Group_Type = c(
    "No Parent Org",
    "Has Parent Org",
    "Civil Society",
    "Not Civil Society"
  ),
  Percentage = c(
    violation_summary$pct_no_parorg,
    violation_summary$pct_has_parorg,
    violation_summary$pct_civil_society,
    violation_summary$pct_not_civil_society
  ),
  Category = c("Parent Org", "Parent Org", "Civil Society", "Civil Society")
)

# Create bar chart
ggplot(plot_data, aes(x = Group_Type, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Percentage of Total Violations by Organizational Type",
    x = "Group Type",
    y = "Percentage of Total Violations",
    fill = "Classification"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    values = c("Parent Org" = "steelblue", "Civil Society" = "darkorange")
  )

# Print the actual percentages
print(violation_summary)


# Create bar chart with percentage labels for both civil society and No parents
library(ggplot2)
ggplot(plot_data, aes(x = Group_Type, y = Percentage, fill = Group_Type)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) + # Add percentage labels
  labs(
    title = "Percentage of Total Violations by Group Type",
    x = "Group Type",
    y = "Percentage of Total Violations"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(
    values = c("Civil Society" = "darkorange", "No Parent Org" = "steelblue")
  ) +
  theme(legend.position = "none") +
  ylim(0, max(plot_data$Percentage) * 1.1) # Add some space above bars for labels


# Count observations for each group type
obs_counts <- full_model %>%
  filter(!is.na(civil_society), !is.na(no_parorg)) %>%
  summarise(
    civil_society_obs = sum(civil_society == 1, na.rm = TRUE),
    no_parorg_obs = sum(no_parorg == 1, na.rm = TRUE)
  )

# Create data for plotting
obs_plot_data <- data.frame(
  Group_Type = c("Civil Society", "No Parent Org"),
  N_Observations = c(obs_counts$civil_society_obs, obs_counts$no_parorg_obs)
)

# Create bar chart with observation counts
library(ggplot2)
ggplot(
  obs_plot_data,
  aes(x = Group_Type, y = N_Observations, fill = Group_Type)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = N_Observations),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) + # Add count labels
  labs(
    title = "Number of Observations by Group Type",
    x = "Group Type",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(
    values = c("Civil Society" = "darkorange", "No Parent Org" = "steelblue")
  ) +
  theme(legend.position = "none") +
  ylim(0, max(obs_plot_data$N_Observations) * 1.1) # Add space for labels

# Print the actual counts
print(obs_plot_data)
