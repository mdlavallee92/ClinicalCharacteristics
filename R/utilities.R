# Utilities ----------------------

# check if slot is in analysis settings
checkMode <- function(settings, mode) {
  obj <- settings[[mode]]
  length(obj) > 0
}

# silent version of getDbCovariateData
silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings) {
  cli::cat_bullet("Getting Covariates from database...",
                  bullet = "info", bullet_col = "blue")
  tik <- Sys.time()
  #get covariate data
  quietCov <- purrr::quietly(FeatureExtraction::getDbCovariateData)
  cov <- quietCov(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covariateSettings = covSettings,
    aggregated = TRUE
  )$result
  tok <- Sys.time()
  cli::cat_bullet("Covariates built at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Covariate build took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")
  return(cov)
}


#prints save activity to console
#TODO add logger to this
verboseSave <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()
  invisible(savePath)
}


