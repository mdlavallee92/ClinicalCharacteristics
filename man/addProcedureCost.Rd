% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addChar.R
\name{addProcedureCost}
\alias{addProcedureCost}
\title{Add a drug exposure cost characteristic}
\usage{
addProcedureCost(
  clinChar,
  timeWindows,
  conceptSets = NULL,
  costType = "amount_allowed",
  categorize = NULL,
  conceptType = c(32810, 32869)
)
}
\arguments{
\item{clinChar}{a clinChar object maintaining the components of the characterization}

\item{timeWindows}{a timeWindow object that specifies the boundaries relative to the target start date
on when to search for the presence of a value. use \code{makeTimeTable} function}

\item{costType}{the column in the cost table to summarize}

\item{categorize}{describes how the continuous value should be categorized.
This function takes a breaksStrategy object to describe the categories ow it is left NULL.
If the parameter is NULL then no categorization summary is done}
}
\value{
adds a costChar object of drug exposure into the clinChar extractSettings slot
}
\description{
This function adds a cost characteristic to the clinChar object for a drug exposure.
A cost characteristics summarizes the total cost of events observed during a window of time.
}
