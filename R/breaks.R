
## Age breaks

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
childWorkRetire <- function() {
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

customAge <- function(breaks, labels) {
  ll <- list(
    'breaks' = breaks,
    'labels' = labels
  )
  return(ll)
}
