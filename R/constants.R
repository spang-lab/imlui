TABLES <- c(
  "users",
  "models",
  "datasets",
  "papers",
  "settings",
  "appstate",
  "samples",
  "modeltypes",
  "datatypes",
  "methods",
  "platforms",
  "mapping_users_models",
  "mapping_users_sessions",
  "mapping_users_groups",
  "mapping_users_resources",
  "mapping_users_datasets",
  "mapping_groups_models",
  "mapping_groups_datasets",
  "mapping_groups_resources",
  "mapping_papers_models",
  "mapping_papers_datasets",
  "mapping_datasets_features",
  "mapping_models_features"
)
SODIUM_HASHED <- TRUE
FEATURE_MAPPINGS <- list(
  norm.jco.train = c(`HLA-A` = "HLA.A", `HLA-C` = "HLA.C"),
  norm.jco.test = c(`HLA-A` = "HLA.A", `HLA-C` = "HLA.C")
)
CATEGORICALS <- c("STUDIDn", "PATSTUDID", "Individual", "PATIENTEN_ID")
PORT <- 8081
