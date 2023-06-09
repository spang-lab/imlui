# Changelog

All notable changes to this project will be documented in this file.

The format is loosely based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), i.e.:

- There should be an entry for every single version.
- The latest version comes first.
- The release date of each version is displayed.
- The same types of changes should be grouped.
- The following keywords are used to denote different types of changes:
  - `Added` for new features
  - `Changed` for changes in existing functionality
  - `Deprecated` for soon-to-be removed features
  - `Removed` for now removed features
  - `Fixed` for bug fixes
  - `Security` in case of vulnerabilities
  - `Infrastructure` for updates of files not related to the package itself,
    e.g. .github/workflows/*, README.md, etc. Infrastructure updates increase
    the patch version.

## [0.5.1] - 2023-04-09

- `Fixed`: Text of Home Page
- `Fixed`: Scaling of imges

## [0.5.0] - 2023-04-08

- `Added`: UMAP, tSNE, PCA, Heatmaps
- `Fixed`: Feature Effect Plot
- `Added`: Filtering of datasets according to model compatibility

## [0.4.3] - 2023-04-04

- `Added`: Enabled editing of database through Web Interface. Currently, values can only be changes but no new rows can be created.

## [0.4.2] - 2023-04-04

- `Fixed`: Fixed login via Github behind reverse proxy.
- `Fixed`: Add remaining tables as tabs to settings menu

## [0.4.1] - 2023-03-31

- `Fixed`: Fixed version displayed in footer.
- `Infrastructure`: Added action for deploying to `imlui@tux1404`.

## [0.4.0] - 2023-03-27

- `Added`: Major Improvements to database handling. In particular the default init values have improved.
- `Added`: Models and lots of dummy datasets.

## [0.1.4] - 2022-04-04

- `Fixed`: install-and-run-behind-shinyserver Dockerfile now also installs fresh from Github

## [0.1.3] - 2022-04-04

- `Fixed`: added no-cache to Dockerfile Build Action

## [0.1.2] - 2022-04-04

- `Fixed`: Cleared `imlui_db.sqlite`
- `Added`: Database Overview is now hidden for non-admins

## [0.1.1] - 2022-04-04

- `Infrastructure`: Added logo and badges.

## [0.1.0] - 2022-04-03

- `Changed`: Read/Store info in database instead of `config.json`
- `Changed`: Version bump to 0.1.0

## [0.0.0.9018] - 2022-03-26

- `Fixed`: DESCRIPTION Imports

## [0.0.0.9017] - 2022-03-26

- `Added`: footer
- `Fixed`: DESCRIPTION Imports

## [0.0.0.9016] - 2022-03-09

- `Added`: user authentication mechanism
- `Added`: automatic url bookmarking
- `Added`: loading animations

## [0.0.0.9015] - 2022-01-26

- `Fixed`: warnings due to multiple imports of functions with the same name, e.g. `plyr::compact` and `purrr::compact`

## [0.0.0.9014] - 2022-01-26

- `Fixed`: output of function `cli` when called with `--version` as argument

## 3.8.2021

- *v0.0.0.9011:* Added user configuration file

## 16.6.2021

- *v0.0.0.9010:* Added Feature Mapping for JCO Dataset. Improved predict.numeric to handle Intercepts.

## 15.6.2021

- *v0.0.0.9009:* Readded fixed Prediction Histograms (this time with density lines)
- *v0.0.0.9008:* Migrated FEP to also use  function `asSVG`
- *v0.0.0.9007:* Switched from PNGs to SVGs

## 27.05.2021 - 14.06.2021

- *v0.0.0.9006:* Lots of new datasets plus complete overhaul of app

## 20.05.2021 - 26.05.2021

- *v0.0.0.9005:* Simplified folder structure
- *v0.0.0.9004:* Added dropdown to choose sample of interest by "colID. batch:colname or score)
- *v0.0.0.9004:* Added colored rectangles showing when a classification would flip
- *v0.0.0.9001:* Swapped Dataset and Method chooser
- *v0.0.0.9003:* Add histogram/density plot of model scores for given datasets
- *v0.0.0.9002:* Made height of plots choosable
- *v0.0.0.9001:* Made Dataset a "multiple choice" dropdown
- *v0.0.0.9001:* Removed Plots Dropdown
