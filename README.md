# IMLUI <img src="inst/assets/png/imlui_logo.png" align="right" width="100"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/toscm/imlui/workflows/R-CMD-check/badge.svg)](https://github.com/toscm/imlui/actions)
<!-- badges: end -->

IMLUI (Interpretable Machine Learning User Interface) is an R package that makes it possible to share your datasets and/or statistical models via a nice and shiny web interface. Users of the web interface will not only be able to view and apply your models, but also to create additional visualizations and explanations to make the result of a classification more understandable.

Some of the cool features that IMLUI provides are:

* [Responsive design](Layout), i.e. it works on mobile devices as well as on traditional monitors.
* Fast response times due to process wide caching of models and datasets
* Possibility to handle [Authentication] through 3rd party Identity providers
* Fine grained [permission system](Authentication#external-user-authorization) for models, datasets, pages and methods
* Possibility to [bookmark](Bookmarking) the exact state of each page
* Easy [Installation] and [Configuration] procedure

For a more detailed overview of IMLUI, have a look at [IMLUi's Wiki](github.com/toscm/dummy/wiki).

## Installation

### Steps

Installation of imlui consists of the following steps:

1. Install [R](https://cran.r-project.org/)
2. Linux/MacOS only: install the dependencies listed in [Dependencies]([#dependencies](https://github.com/toscm/dummy/wiki/Installation#dependencies))
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

For more details on IMLUI, such as usage examples, configuration guidelines or internal implementation details, have a look at [IMLUi's Wiki](github.com/toscm/dummy/wiki).
