% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Capitalize.r
\name{capitalize}
\alias{capitalize}
\alias{capitalize.data.frame}
\alias{capitalize.data.table}
\alias{capitalize.default}
\title{Capitalizes all Character Strings}
\usage{
capitalize(data)

\method{capitalize}{data.table}(data)

\method{capitalize}{data.frame}(data)

\method{capitalize}{default}(data)
}
\arguments{
\item{data}{\code{data.frame} containing character columns or \code{character vector}}
}
\value{
Object of input class
}
\description{
Capitalizes all letters in an object.
}
\section{Methods (by class)}{
\itemize{
\item \code{data.table}: data.table

\item \code{data.frame}: data.frame

\item \code{default}: default
}}
\section{Methods}{

Current classes supported:
\itemize{
 \item \code{data.frame}
 \item \code{data.table}
 \item \code{character vector}
}
}

