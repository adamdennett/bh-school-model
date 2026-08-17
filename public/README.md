# Brighton & Hove school demand — the open-data model

A spatial interaction model of secondary school demand in Brighton & Hove,
built **entirely from published sources**. It covers the current city and the
four wards joining it under local government reorganisation on 1 April 2028.

Everything in this folder is self-contained and publishable. It reads no
pupil-level records and no model fitted on them, and `assert_open()` in
[R/00_open_core.R](R/00_open_core.R) enforces that at runtime — a path
resolving inside a restricted directory raises an error rather than being read.

```r
source("public/run_public.R")
```

## What this is for

The substantive question is whether Longhill High School can be sustained at
its current site and admission number. A precise answer needs pupil-level
admissions records, which are not public. This model asks a different and more
useful question: **does the answer depend on having them?**

It does not sweep one set of assumptions. It sweeps the whole plausible range
of the three parameters that restricted data would pin down —

| Parameter | Range swept | What it controls |
| --- | --- | --- |
| `beta` | 1.0 – 3.2 | how sharply demand falls with journey time |
| `W_j` | 4 specifications | how attractive each school is, net of location |
| `gamma` | 0 – 2.4 | how much catchment priority matters |

— across three entry years and two possible sites, and reports how often each
conclusion holds. Where a conclusion holds across the entire envelope, no
restricted data is needed to state it.

## Headline results

**Longhill cannot sustain a PAN of 240 into the 2030s.** Across the full
parameter envelope the school reaches 90% of a 240 admission number in 62% of
cases in 2026, 40% in 2030 and **18% in 2035**. Excluding the deliberately
extreme "all schools equal" specification, the 2035 figure falls to 13%.

**A PAN of 120–150 is defensible; 180 is marginal.** At 120 the school
recruits to its number in 79% of the envelope in 2035; at 150, 65%; at 180,
56%.

**Longhill is the worst-placed school in the city, and this needs no
assumptions.** The distance-only "Brightopia" model — every school identical,
every child identical, location the only difference — gives Longhill 85 of an
equal share of the cohort, and the Elm Grove site is better placed at *every*
distance decay tested. This conclusion uses no attainment data, no Ofsted
grade, no preference counts, no catchment rule and no faith criterion, and it
is the most robust finding in the bundle.

**Relocation increases modelled recruitment, but read the size with care.**
Across the envelope the median effect is +58 children in 2026 and +44 by 2035,
positive in 97–100% of runs. An earlier version reported no benefit; that was
an artefact of approximating journey times to the Elm Grove site, fixed by
rebuilding the `r5r` network over a merged East and West Sussex extract. The
direction is solid. The magnitude is an upper bound, because the model
over-predicts Longhill to begin with — see the validation caveat in the report.

**The 2024 catchment redraw is beyond what open data can settle.** Option Z,
in force from 2026 entry, moves Kemptown into the Longhill catchment and part
of Whitehawk & Marina out of it. Catchment headcount barely changes, and the
open model's estimate of the effect is a fraction of a child either way, with
the sign depending on how much weight catchment priority is given. This is the
single question where restricted data would most obviously help.

**The binding uncertainty is not distance decay.** A variance decomposition
across the sweep attributes 54% of the variation in the answer to the
*attractiveness* specification and only 4% to `beta`. This matters for what
would be worth releasing: precise travel-time behaviour is not the gap —
school-level revealed preference is.

## Data sources

All open, all cited, none requiring an access agreement.

| Input | Source |
| --- | --- |
| Travel times | `r5r` routing over OpenStreetMap and Brighton & Hove GTFS (May 2024). A postcode-to-school time matrix; contains no personal data. |
| Admissions, PAN, preferences | Brighton & Hove City Council published allocation factsheets, 2013–2026 |
| School characteristics | DfE [Get Information About Schools](https://get-information-schools.service.gov.uk/) |
| Attainment, Ofsted, finance | DfE performance tables, via [school_attainment_tool](https://github.com/adamdennett/school_attainment_tool) |
| Population | ONS small area population estimates, mid-2022; LSOA child projections |
| Boundaries | ONS Open Geography Portal; council catchment maps — pre-2024, and the "option Z" scheme determined March 2025 and in force from 2026 entry |
| Reorganisation | [MHCLG decision letters](https://www.gov.uk/government/publications/local-government-reorganisation-decision-letters-to-council-leaders), 16 July 2026 |

## Method

Production-constrained spatial interaction model,

$$T_{ij} = A_i O_i W_j^{\alpha} d_{ij}^{-\beta}, \qquad A_i = \frac{1}{\sum_j W_j^{\alpha} d_{ij}^{-\beta}}$$

following Dennett, *Idiots' Guide to Spatial Interaction Modelling*,
[Part 1](https://rpubs.com/adam_dennett/257231) and
[Part 2](https://rpubs.com/adam_dennett/259068). Modelled flows are then
constrained to school capacity by iterative proportional fitting, so that
demand displaced from a full school cascades to the next-best option rather
than disappearing.

Demand is projected by cohort ageing: a child aged *a* in the ONS mid-2022
estimates enters Year 7 in 2022 + (11 − *a*), so entry years to 2033 come from
children already born and counted. 2034 and 2035 are extrapolated and labelled
as such.

## What this model cannot do

- **It cannot identify `beta` from open data.** Published statistics give the
  margins of the origin-destination table — how many children live in each
  area, how many places each school fills — but not the interactions between
  them. That is precisely why this bundle sweeps `beta` rather than estimating
  it. A published origin-destination matrix, at catchment level or finer, would
  close this.
- **Attractiveness is proxied, not measured.** The four specifications bracket
  the plausible range, but published first preferences bundle a school's
  reputation together with where it happens to be.
- **Journey times to schools are known; journey times to *alternatives* are
  modelled.** The `r5r` matrix now covers the whole study area — BN1/2/3/7/9/
  10/25/41/42/45, including Peacehaven, Newhaven, Seaford and the Elm Grove
  site — so no distance approximation remains. What the model still cannot see
  is how families weigh those journeys against everything else, which is what
  the parameter sweep is for.
- **It models where children would go, not what happens to them there.**
  Attainment questions belong to
  [school_attainment_tool](https://github.com/adamdennett/school_attainment_tool).

## An invitation to Brighton & Hove City Council

The figures above are derived from published data with the uncertainty stated
rather than hidden. They are testable. The council holds the records that would
confirm or refute them, and the analysis identifies exactly which records would
help most: **an origin-destination matrix of Year 7 preferences and offers, by
area of residence and destination school.** Aggregated to catchment or ward
level, it carries no disclosure risk and would replace the widest band of
uncertainty in this model with a measurement.

Specific predictions this model makes, which the council's own data can check:

1. Longhill's natural recruitment sits between 63 and 224 children in 2035
   under current catchments, with a central estimate near 156.
2. Relocating the school to Elm Grove without redrawing catchments raises its
   intake, by a median of about 44 children a year by 2035 on this model —
   which, given the model's known bias towards Longhill, is an upper bound.
3. The four wards joining in 2028 contribute fewer than 10 additional children
   a year to Longhill under admissions arrangements that retain a catchment
   priority at Peacehaven Community School.

If any of these is wrong, the data to show it exists. We would welcome the
correction.

## Files

| File | Purpose |
| --- | --- |
| `R/00_open_core.R` | Paths, the open-data guard, school table, model functions |
| `R/01_open_inputs.R` | Builds zones, populations, costs and attractiveness from published sources |
| `R/02_sensitivity_envelope.R` | Sweeps the parameter space, both catchment regimes, and reports robustness |
| `R/03_open_scenarios.R` | Scenario suite at a central specification, with bands from the envelope |
| `R/04_brightopia.R` | The distance-only model: no attainment, no reputation, no catchment, no faith |
| `bh_school_sim_open.qmd` | The write-up → `bh_school_sim_open.html` |
| `output/` | Results, figures and CSVs |

```r
source("public/run_public.R")
quarto::quarto_render("public/bh_school_sim_open.qmd")
```

Deliberately duplicated rather than shared with the parent project, so this
folder can be lifted into a public repository intact.
