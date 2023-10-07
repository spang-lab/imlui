# IMLUI <img src="man/figures/imlui_logo.png" align="right" width="100"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/spang-lab/imlui/workflows/R-CMD-check/badge.svg)](https://github.com/spang-lab/imlui/actions)
<!-- badges: end -->

IMLUI (Interpretable Machine Learning User Interface) is an R package for

* Applying, visualizing and explaining model predictions
* Comparing models and/or datasets with each other
* Sharing models and/or datasets with other users

Some of the cool features that IMLUI provides are:

* [Responsive design](https://github.com/spang-lab/imlui/wiki/User-Interface), i.e. it works on mobile devices as well as on traditional monitors.
* Fast response times due to process wide caching of models and datasets
* Possibility to handle [Authentication](articles/Authentication.Rmd) through 3rd party Identity providers
* Fine grained [permission system](https://github.com/spang-lab/imlui/wiki/Authentication#internal-authorization) for models, datasets, pages and methods
* Possibility to [bookmark](https://github.com/spang-lab/imlui/wiki/Bookmarking) the exact state of each page
* Easy [Installation] and [Configuration] procedure

For a more detailed overview of IMLUI, have a look at [IMLUi's Wiki](https://github.com/spang-lab/imlui/wiki).

## Installation

Installation of imlui consists of the following steps:

1. Install [R](https://cran.r-project.org/)
2. Linux/MacOS only: install the dependencies listed in [Dependencies](https://github.com/spang-lab/imlui/wiki/Installation#dependencies)
3. Install imlui by entering the following commands in an R session:
   ```R
   if (!"devtools" %in% installed.packages()[, "Package"]) {
     install.packages(pkgs = "devtools", repos = c(CRAN = "https://cloud.r-project.org"))
   }
   devtools::install_github(repo = "spang-lab/imlui")
   ```

For details on how to start the IMLUI webapp after Installation see [Usage].

## Usage

The easiest way to start imlui is by entering the following command in an interactive R session:

```R
imlui::serve_web_app()
```

Congratulations! Now imlui is served on port 8080 of your local PC. That means, you can access it via URL <http://localhost:8080>. However, right now, the website can only be accessed from you local machine and as soon as you close the R session, the site won't be served any more. Read on for information on how to improve those things.

## What's next?

For more details on IMLUI, such as usage examples, configuration guidelines or internal implementation details, have a look at [IMLUi's Wiki](https://github.com/spang-lab/imlui/wiki).
