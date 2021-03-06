% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_name.R
\name{process_words}
\alias{process_words}
\title{Process names into its components}
\usage{
process_words(word1, word2, word3, word4, word5)
}
\arguments{
\item{word1-word5}{\code{character string}, should be a single word from a
\code{character string} containing a name}
}
\value{
A \code{list} with name components filled. Missing components are filled with
 \code{NA_caracter}
}
\description{
takes up to five words from a \code{string} containing a name and divides them
 into there components.
}
\section{Name Components}{

\itemize{
 \item{prefix: Prefix title, e.g. "Mr", "Mrs", "Dr", "Gov", "President"}
 \item{first: First name}
 \item{middle: Middle name}
 \item{middle_initial: Single Character e.g. "Q"}
 \item{last: Last name, can be more than one word}
 \item{suffix: Suffix title, e.g. "Sr", "Jr", "ESQ"}
}
}

