% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JoinNA.R
\name{JoinNA}
\alias{JoinNA}
\title{dplyr Join that Fills NAs}
\usage{
JoinNA(x, y, join_type, fill = 0, ...)
}
\arguments{
\item{x}{a data.frame (left in left_join)}

\item{y}{a data.frame}

\item{join_type}{dplyr join}

\item{fill}{numeric or character to fill NAs}

\item{...}{by arguement plus any additional args for join}
}
\value{
data.frame
}
\description{
\code{JoinNA} performs a selected dplyr join and fills ALL NAs
}

