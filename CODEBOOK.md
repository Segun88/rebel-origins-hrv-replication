# Codebook: Rebel Group Origins and Human Rights Violations

## Overview

This codebook documents the variables used in the analysis of rebel group organizational origins and human rights violations. The dataset is a group-year panel covering armed groups active in civil conflicts from 1990 to 2018, constructed by merging five international data sources: RHRV, FORGE, NSA, WDI, and V-Dem.

**Unit of observation:** Rebel group × year  
**Temporal coverage:** 1990–2018  
**Observations:** ~700+ group-year observations (varies by analytic sample)

---

## Identifiers

| Variable | Description |
|----------|-------------|
| `sideb` | Rebel group name (primary group identifier, from UCDP) |
| `dyadid1` | Dyad identifier linking rebel group to government opponent |
| `year` | Calendar year of observation |
| `conflictid` | UCDP conflict identifier |
| `sidea` | Government side in the conflict |
| `ccode` | COW (Correlates of War) country code |
| `iso3c` | ISO 3166-1 alpha-3 country code (harmonized from COW codes) |
| `location` | Country or territory where conflict takes place |

---

## Dependent Variables: Human Rights Violations

All violation variables are derived from the **Rebel Human Rights Violations (RHRV) Dataset** (Walsh, Conrad & Whitaker 2024). The RHRV codes violations from U.S. Department of State and Amnesty International annual reports.

### Severity Scores (Original RHRV Coding)

Each violation type is coded on a 0–2 ordinal scale:
- **0** = No reported violation
- **1** = Reported violation (isolated or limited)
- **2** = Systematic or widespread violation

| Variable | Description |
|----------|-------------|
| `rkillings_s` | Unlawful or arbitrary killings |
| `rtorture_s` | Torture |
| `rdetention_s` | Detention and disappearances |
| `rproperty_s` | Property violations and destruction |
| `rrecruitment_s` | Forcible recruitment (including child soldiers) |
| `rsexual_s` | Sexual violence |
| `rdisplace_s` | Forced displacement |
| `rrestrict_s` | Movement restriction |
| `violation_s` | Any violation reported (binary: 0/1) |

### Binary Violation Indicators (Constructed)

Each binary indicator equals 1 if the corresponding severity score is ≥ 1, and 0 otherwise.

| Variable | Description |
|----------|-------------|
| `killings_binary` | Any reported killings (0/1) |
| `torture_binary` | Any reported torture (0/1) |
| `detention_binary` | Any reported detention/disappearance (0/1) |
| `property_binary` | Any reported property violation (0/1) |
| `recruitment_binary` | Any reported forcible recruitment (0/1) |
| `sexual_binary` | Any reported sexual violence (0/1) |
| `displace_binary` | Any reported forced displacement (0/1) |
| `restrict_binary` | Any reported movement restriction (0/1) |

### Composite Dependent Variables (Constructed)

These variables operationalize the theoretical distinction between **discriminatory violence** (selective, targeted) and **indiscriminate violence** (broad, non-selective).

**Discriminatory violations:** killings, detention, torture — these tend to be selective and targeted at specific individuals or groups.

**Indiscriminate violations:** forced recruitment, sexual violence, displacement, movement restriction, property destruction — these tend to affect broader civilian populations non-selectively.

| Variable | Description | Range |
|----------|-------------|-------|
| `discriminatory_binary` | Any discriminatory violation reported (0/1) | 0–1 |
| `indiscriminate_binary` | Any indiscriminate violation reported (0/1) | 0–1 |
| `discriminatory_variety` | Count of distinct discriminatory violation types | 0–3 |
| `indiscriminate_variety` | Count of distinct indiscriminate violation types | 0–5 |
| `total_variety` | Total count of distinct violation types | 0–8 |
| `total_severity` | Sum of all severity scores | 0–16 |
| `any_violation` | Any violation of any type reported (0/1) | 0–1 |
| `violation_count` | Count of violation types with severity ≥ 1 | 0–8 |
| `violence_share_disc` | Discriminatory share of total violations (discriminatory_variety / total_variety) | 0–1 |
| `violence_ratio_smooth` | Smoothed ratio: (discriminatory + 1) / (indiscriminate + 1) | Continuous |
| `violence_diff` | Normalized difference: discriminatory - (5/3) × indiscriminate | Continuous |
| `discriminatory_severity` | Sum of discriminatory severity scores | 0–6 |
| `indiscriminate_severity` | Sum of indiscriminate severity scores | 0–10 |

---

## Independent Variables: Organizational Origins

All origin variables are derived from the **FORGE Dataset** (Braithwaite & Cunningham 2020). FORGE codes the parent organizations from which rebel groups emerged.

### Raw FORGE Parent Organization Indicators

Each indicator equals 1 if the rebel group had that type of parent organization, 0 otherwise. Groups may have multiple parent types.

| Variable | Description |
|----------|-------------|
| `preorgmvt` | Social/political movement parent |
| `preorgyou` | Youth organization parent |
| `preorglab` | Labor organization parent |
| `preorgrel` | Religious organization parent |
| `preorgeth` | Ethnic organization parent |
| `preorgref` | Refugee organization parent |
| `preorgno` | No identifiable parent organization |
| `preorgoth` | Other type of parent organization |
| `preorgmil` | Military faction parent |
| `preorgfmr` | Former military parent |
| `preorgfor` | Foreign armed group parent |
| `preorgter` | Terrorist organization parent |
| `preorgreb` | Prior rebel group parent |
| `preorgpar` | Political party parent |
| `preorggov` | Government faction parent |
| `merger` | Group formed through merger of existing organizations (0/1) |
| `splinter` | Group formed through splintering from existing organization (0/1) |

### Broad Origin Categories (Constructed)

These aggregate the raw FORGE indicators into theoretically meaningful groupings. A group may belong to multiple broad categories.

| Variable | Description |
|----------|-------------|
| `civil_society` | 1 if any civil society parent (movement, youth, labor, or religious) |
| `violent_parent` | 1 if any violent/military parent (military, former military, foreign armed, terrorist, or prior rebel) |
| `political_party` | 1 if political party parent |
| `no_parorg` | 1 if no formal parent or ethnic/refugee/other parent |
| `govt_faction` | 1 if government faction parent |

### Mutually Exclusive Origin Categories (Constructed)

These ensure each group belongs to exactly one category. Groups with overlapping origins are excluded from the relevant analytic sample.

| Variable | Description |
|----------|-------------|
| `civil_only` | 1 if civil society origin AND no other origin type |
| `violent_only` | 1 if violent/military origin AND no other origin type |
| `no_parent_only` | 1 if no parent organization AND no other origin type |
| `political_party_only` | 1 if political party origin AND no other origin type |
| `govt_faction_only` | 1 if government faction origin AND no other origin type |
| `mixed_origin` | 1 if multiple broad origin categories apply |

### Analytic Sample Origin Factors

| Variable | Sample | Categories | Reference |
|----------|--------|------------|-----------|
| `origin_3cat` | model_data_all (N≈294) | violent, civil, noparent | violent |
| `origin_comm` | model_data_comm_broad (N≈378) | violent, community, noparent | violent |
| `origin_clean` | Full model_base | 4-level factor | Violent/military origin |

**Note on `origin_comm`:** The "community" category combines civil society AND political party origins (when `COMMUNITY_INCLUDES_PARTY = TRUE`), based on the theoretical argument that both types maintain pre-conflict ties to civilian populations.

---

## Control Variables

### Rebel Group Capabilities (from NSA Dataset)

| Variable | Description | Scale |
|----------|-------------|-------|
| `rebstrength` | Rebel strength relative to government | much weaker → much stronger |
| `rebstrength_num` | Numeric version of rebel strength | 1–5 |
| `fightcap` | Fighting capacity | low, moderate, high |
| `fightcap_num` | Numeric version of fighting capacity | 1–3 |
| `terrcont` | Territorial control | no/yes |
| `terrcont_num` | Numeric version of territorial control | Ordinal |
| `centcontrol` | Centralized command structure | no/yes |
| `centcontrol_num` | Numeric version of centralized control | Ordinal |
| `rebel_support` | External support for rebels | no (0), alleged (1), explicit (2) |
| `rebel_support_num` | Numeric version of external support | 0–2 |
| `rebestimate` | Estimated rebel group size | Numeric |
| `conflict_duration` | Years since conflict onset for this dyad | Numeric |

### Conflict Characteristics (from RHRV/UCDP)

| Variable | Description | Scale |
|----------|-------------|-------|
| `intensity` | Conflict intensity level | UCDP coding |
| `osv` | One-sided violence level | UCDP coding |
| `osv_dummy` | Any one-sided violence reported (0/1) | Binary |

### Country-Level Controls

| Variable | Description | Source |
|----------|-------------|--------|
| `gdp_pc` | GDP per capita, constant 2015 USD (interpolated) | WDI |
| `log_gdp_pc` | Natural log of GDP per capita | Constructed from WDI |
| `democracy` | V-Dem polyarchy score (electoral democracy index) | V-Dem |
| `democracy_z` | Standardized (z-scored) democracy measure | Constructed from V-Dem |
| `democracy_high` | Binary: 1 if democracy ≥ 0.5 | Constructed from V-Dem |

### Rebel Group Characteristics (from FORGE)

| Variable | Description |
|----------|-------------|
| `ideology` | Rebel group ideology |
| `foundyear` | Year the rebel group was founded |

---

## Analytic Samples

| Sample | Variable | N (approx.) | Description |
|--------|----------|-------------|-------------|
| `model_base` | Full filtered panel | ~700 | All group-years with non-missing NSA variables, 1990–2018 |
| `model_data_all` | 3-category origins | ~294 | Violent vs. civil society vs. no-parent (mutually exclusive, mergers dropped) |
| `model_data_comm_broad` | Community ties | ~378 | Violent vs. community (civil + party) vs. no-parent (mutually exclusive, mergers dropped) |

**Sample construction notes:**
- Merger groups (`merger == 1`) are dropped from both analytic samples by default
- Only groups with exactly one broad origin category are included (mixed-origin groups excluded)
- The `COMMUNITY_INCLUDES_PARTY` switch controls whether political party origins are grouped with civil society in Sample B

---

## Data Harmonization Notes

### Country Code Reconciliation
COW country codes from FORGE are translated to ISO3C codes for merging with WDI and V-Dem. A custom matching table handles special cases including: DR Congo (COD), Yemen (YEM), Serbia/Yugoslavia (SRB), Russia/Soviet Union (RUS), Myanmar/Burma (MMR), South Sudan (SSD), Bosnia-Herzegovina (BIH), and North Macedonia (MKD).

### GDP Interpolation
After merging WDI GDP data via ISO3C codes, linear interpolation is applied within each country to fill gaps in GDP coverage. This increases GDP coverage from the raw merge rate to approximately 90%+ of observations.

### NSA Expansion
The NSA dataset arrives at the dyad level with start and end dates. Each record is expanded into one row per year of activity, then collapsed to unique sideb × year observations, retaining the maximum value of ordinal capability measures across any duplicates.

---

## References

- Braithwaite, Jessica Maves, and Kathleen Gallagher Cunningham. 2020. "When Organizations Rebel: Introducing the Foundations of Rebel Group Emergence (FORGE) Dataset." *International Studies Quarterly* 64(1): 183–193.
- Cunningham, David E., Kristian Skrede Gleditsch, and Idean Salehyan. 2013. "Non-state Actors in Civil Wars: A New Dataset." *Conflict Management and Peace Science* 30(5): 516–531.
- Walsh, James Igoe, Justin M. Conrad, and Beth Elise Whitaker. 2024. "Rebel Human Rights Abuses During Civil Wars: Introducing the Rebel Human Rights Violations Dataset." *Journal of Peace Research* 61(3): 477–491.
- Coppedge, Michael, et al. 2024. "V-Dem Dataset v15." Varieties of Democracy (V-Dem) Project.
- World Bank. 2024. "World Development Indicators." Washington, DC: The World Bank.
