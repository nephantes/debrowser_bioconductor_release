% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condSelect.R
\name{selectConditions}
\alias{selectConditions}
\title{selectConditions}
\usage{
selectConditions(Dataset = NULL, metadata = NULL, choicecounter = NULL,
  input = NULL)
}
\arguments{
\item{Dataset, }{used dataset}

\item{metadata, }{metadatatable to select from metadata}

\item{choicecounter, }{choicecounter to add multiple comparisons}

\item{input, }{input params}
}
\value{
the panel for go plots;
}
\description{
Selects user input conditions, multiple if present, to be
used in DESeq.
}
\note{
\code{selectConditions}
}
\examples{
    x<- selectConditions()

}
