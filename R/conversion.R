# Breaks -------------------------
# Continuous => categorical
## Age breaks ---------------------------------

#' Make 5 yr age group
#' @description
#' Helper function for age characteristic to make 5 year age breaks
#'
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age5yrGrp <- function() {
  x <- seq(0,130, by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make 10 yr age group
#' @description
#' Helper function for age characteristic to make 10 year age breaks
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age10yrGrp <- function() {
  x <- seq(0,130, by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make age group using American Community Survey style
#' @description
#' Helper function for age characteristic to make age breaks following the break
#' style of the American Community Survey (acs)
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
acs <- function() {
  x <- c(0,5,10,15,18,20,21,22,25,30,35,40,45,50,55,
         60,62,65,67,70, 75, 80, 85, 130)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make age groups for common workforce lifespan
#' @description
#' Helper function for age characteristic to make age breaks following lifespan
#' of a working human. 0-14 is child, 15-64 is working age, 65+ is retirement
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
ageChildWorkRetire <- function() {
  x <- c(0,15,65, 130)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make 19 age groups
#' @description
#' Helper function for age characteristic to make 19 age groups where
#' every 5 years are categorized up to age 85. 85+ is a single group
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age19Grps <- function() {
  x <- c(seq(0,85, by = 5), 130)
  x[1] <- 1
  a <- dplyr::lead(x) - 1
  lab <- c("0", glue::glue("{x}-{a}"))[-20]
  x <- c(0,x)

  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make age group using 65 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 65 and at or above age 65
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age65 <- function() {
  x <- c(0,65, 130)
  a <- dplyr::lead(x) - 1
  lab <- c(">65", "<=65")
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make age group using 18 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 18 and at or above age 18
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age18 <- function() {
  x <- c(0,18, 130)
  a <- dplyr::lead(x) - 1
  lab <- c(">18", "<=18")
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

## year breaks--------------------

#' Make year group using 5 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 5 year groups
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year5yrGrp <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(2000, (this_year + 5), by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(2000:(this_year + 1)),
    grp = cut(as.numeric(2000:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}


#' Make year group using 10 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 10 year groups
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year10yrGrp <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(2000, (this_year + 10), by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(2000:(this_year + 1)),
    grp = cut(as.numeric(2000:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

#' Make year group by covid time
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' by covid time where 2000-2019 is pre-covid, 2020-2022 is covid and 2023+ is post-covid
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
yearCovid <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- c(2000,2020, 2023, (this_year + 1))
  a <- dplyr::lead(x) - 1
  lab <- c("pre-covid", "covid", "post-covid")
  ll <- tibble::tibble(
    value = as.numeric(2000:(this_year + 1)),
    grp = cut(as.numeric(2000:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}


## Custom breaks --------------------------

#' Function to make custom categorical breaks
#' @param x a sequence of values to categorize
#' @param breaks the breaks to the x sequenece..see `cut`
#' @param labels the labels of the sequence
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
customBreaks <- function(x, breaks, labels) {
  ll <- tibble::tibble(
    value = x,
    grp = cut(x, breaks = breaks, labels = labels, right = FALSE)
  )
  br <- new("breaksStrategy", breaks = ll)
  return(br)
}

# Scores -------------------------------
# Categorical => continuous


charlsonIndexScore <- function() {

  # deal with concept scores first
  idx <- seq_along(charlsonConcepts())

  weights <- c(
    # Score for:Acute MI,congestive heart failure, peripheral vascular disease,
    # cerebrovascular disease, dementia, chronic pulmonary disease,
    # rheumatologic disease, peptic ulcer, mild liver disease, controlled diabetes
    rep(1, times = 10),
    # Score for: hemiplegia or paraplegia, renal disease, malignancy localizedm leukemia, lymphoma
    rep(2, times = 5),
    # score for: AIDS and metastatic tumor
    rep(6, times = 2)
  )

  charlsonIndex <- new("scoreStrategy",
                       name = "CharlsonIndex",
                       domain = c("Condition", "Age"),
                       weights = tibble::tibble(
                         id = idx,
                         w = weights
                       )
                       )


  # next add age scores
  ageScore <- age10yrGrp()@breaks |>
    dplyr::filter(
      value >= 50
    ) |>
    dplyr::mutate(
      w = dplyr::case_when(
       dplyr::between(value, 50, 59) ~ 1,
       dplyr::between(value, 60, 69) ~ 2,
       dplyr::between(value, 70, 79) ~ 3,
       value >= 80 ~ 4
      ),
      id = value
    ) |>
    dplyr::select(
      id, w
    )

  charlsonIndex@weights <- dplyr::bind_rows(charlsonIndex@weights, ageScore)
  cli::cat_bullet(
    glue::glue("{crayon::red('Note')}: User needs to add an ageChar to clinChar object when using charlsonIndexScore()"),
    bullet = "info",
    bullet_col = "blue"
  )

  return(charlsonIndex)
}


