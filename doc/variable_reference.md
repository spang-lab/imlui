# Phase Reference



# Variable Reference

- [penv](#penv)
  - [imlui_config](#imlui_config)
  - [snr](#snr)
  - [datasets](#datasets)
  - [models](#models)
  - [db](#db)
- [ses](#ses)
  - [input](#input)
    - [dim](#dim)
    - [login_jscookie](#login_jscookie)
  - [output](#output)
  - [session](#session)
    - [clientData](#clientdata)
  - [const](#const)
  - [rv](#rv)
    - [user](#user)
    - [tbl](#tbl)
  - [r](#r)
    - [model](#model)
    - [dataset](#dataset)
    - [size](#size)

## penv

* Title: Process Environment
* Type: Environment
* Description: Used for storing variables in process scope, i.e., accessible across user sessions. Initalized during package loading in function `.onLoad()`.

### imlui_config

* Title: IMLUI Config
* Type: List
* Description: Values read from `$IMLUI_CONFIG_FILE`, e.g. `~/.config/imlui/imlui_config.yml`.

### snr

* Title: Session Numbers
* Type: Integers
* Description: Whenever a new user session is started, the current value of snr is used as identifier for that session and snr is incremented by one. Reasoning: for debug reasons it's important to understand which requests and errors come from which session, but printing the full session token is very ugly, because it has 32 characters (or something similar), so we generate an additional small identification number.

### datasets

Object of class `mcache`. Initalized during package loading in function `.onLoad()`. Can be updated only by function `getdata()` which is called only by reactives `ses$r$model$params[<model_symbol>]()` and `ses$r$dataset$df[dataset_symbol]()`.

### models

`list`.

### db

*DB object*. Used for interacting with IMLUI_DB. Initalized in `init_server_data` by calling `DB$new()`.

## ses

*environment*. Session environment. Contains all data specific for a single user session. Initalized in `server()` by calling `init_server_data()`. Updated by almost every other function during a user session. For details see below.

### input

*reactiveValues*. Populated by `shiny::runApp` based on messages from browser via websocket and passed as readonly object to `server`.

#### dim

*2 dimensional numeric vector*. Contains width and height of HTML body.

#### login_jscookie

String containing session_id stored by browser as cookie during last session. NULL if browser has not yet transmitted cookie to R process. Empty string if no session ID stored as cookie.

Written by: browser
Read by: server > 

### output

*reactiveValues object*. Populated by `server` and passed as readonly object to `shiny::runApp` where it (potentially) triggers messages via websocker to browser.

### session

*ShinySession object*. Populated by `shiny::runApp` based on messages from browser via websocket and passed as readonly object to `server`.

#### clientData

*reactiveValues object*.

### const

*list*. Values constant for a single session, like `url_hostname`, `url_port` determined at runtime after server has started.

### rv

*reactiveValues object*.

#### user

*list of strings*. Elements are: id, group_ids, display_name, github_id, avatar_url, password, gitlab_id, google_id, spanglab_gitlab_id, spanglab_auth_id.

#### tbl

*list of dataframes*. Each dataframe corresponds to a table in IMLUI_DB.

### r

*list of reactives and other lists*.

#### model

*list*.

##### ids

*reactive*. Returns a vector of all model IDs from IMLUI_DB for which the current user has access.

##### symbols

*reactive*. Returns a vector of all model symbols from IMLUI_DB for which the current user has access.

##### displaynames

*reactive*. Returns a vector of all model displaynames from IMLUI_DB for which the current user has access. The vector names are set to `ses$r$model$symbols()`.

##### pkgs

*reactive*. Returns a vector of all model pkgs from IMLUI_DB for which the current user has access. The vector names are set to `ses$r$model$symbols()`.

##### params

*list* **TODO: remove and use `pkg$datasets` for caching instead**

###### <model_symbol_1>

*reactive*.

###### <model_symbol_2>

*reactive*.

###### ...

##### features

*list* **TODO: remove and use `pkg$datasets` for caching instead**

###### <model_symbol_1>

*reactive*.

###### <model_symbol_2>

*reactive*.

###### ...

##### symbols_list

*reactive*.

##### params_list

*reactive*.

##### features_list

*reactive*.

#### dataset

*list*

##### ... **TODO**

#### size

*list*

##### ... **TODO**
