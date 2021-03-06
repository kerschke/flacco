% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gui_SmoofImport.R
\name{SmoofImport}
\alias{SmoofImport}
\title{Shiny Server Function for BBOB Import Page Module}
\usage{
SmoofImport(input, output, session, stringsAsFactors)
}
\arguments{
\item{input}{[\code{shiny-input}]\cr
\href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} input variable for the specific UI module.}

\item{output}{[\code{shiny-output object}]\cr
\href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} output variable for the specific UI module.}

\item{session}{[\code{shiny-session object}]\cr
\href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} session variable for the specific UI module.}

\item{stringsAsFactors}{[\code{\link{logical}(1)}]\cr
How should strings be treated internally?}
}
\description{
\code{SmoofImport} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}}
server function which will control all aspects of the \code{SmoofImportPage}-UI Module.
It will be called with \code{\link[shiny]{callModule}}.
}
