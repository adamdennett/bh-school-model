# Brighton & Hove secondary school admissions — an open-data analysis

**[Read the report →](https://adamdennett.github.io/bh-school-model/)**

A spatial interaction model of secondary school demand in Brighton & Hove,
built entirely from published sources. It covers the current city and the four
wards joining it under local government reorganisation on 1 April 2028.

The substantive question is whether Longhill High School can be sustained at
its current site and admission number, and what redrawing catchments, moving
the school, or reducing admission numbers across the city would each do. A
precise answer needs individual admissions records, which are not published.
This model asks a different question: **does the answer depend on having
them?** Rather than committing to one set of assumptions, it sweeps the full
plausible range of the parameters that restricted data would pin down, and
reports which conclusions survive the whole range and which do not.

## No pupil-level data is used here

Everything in this repository is built from published sources: ONS population
estimates and small-area age structure, the council's own published admissions
factsheets and catchment maps, the Schools Adjudicator's determination of
20 October 2025, DfE performance tables, and journey times routed over OSM and
GTFS with [`r5r`](https://ipeagit.github.io/r5r/).

This is enforced in code, not by convention. `assert_open()` in
[public/R/00_open_core.R](public/R/00_open_core.R) is called by every reader in
the bundle and raises an error if a path resolves inside a restricted
directory. The analysis cannot silently acquire a dependency on restricted
data.

The report does, in places, describe findings from a parallel analysis of
individual admissions records held under separate arrangements — always
labelled as such, and always as aggregate statistics rather than data. Those
passages exist to show what the published sources cannot establish. See
*What the council's own data would settle* in the report.

## Reproducing it

```r
source("public/run_public.R")   # rebuilds every output
quarto::quarto_render("public/bh_school_sim_open.qmd")
```

External inputs are resolved through `OPEN_*` environment variables listed at
the top of [public/R/00_open_core.R](public/R/00_open_core.R); the defaults
point at local paths and will need setting for a fresh checkout. The routed
travel matrix is included (`public/output/travel/`) so the model can be re-run
without rebuilding the r5r network, which is large and is not committed.

## Caveats worth reading before citing

The report states these where they arise, but the two that matter most:

- **The open model over-predicts Longhill.** It can see how reachable a school
  is but not how strongly families avoid one, because published data does not
  record that. Relocation gains here are an upper bound, not an estimate.
- **Some small-area deprivation figures are approximations.** Child counts are
  apportioned to LSOAs using ONS small-area age structure, which is a real
  improvement on postcode counts but still understates within-catchment
  contrasts relative to address-level records.

## Licence

Code is MIT. Text and figures are CC BY 4.0. Underlying data remains under the
licences of its original publishers, chiefly the Open Government Licence.

Adam Dennett, Professor of Urban Analytics, UCL Centre for Advanced Spatial
Analysis.
