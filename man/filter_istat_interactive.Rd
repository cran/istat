% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_istat_interactive.R
\name{filter_istat_interactive}
\alias{filter_istat_interactive}
\title{Filter data set interactively}
\usage{
filter_istat_interactive(dataset, lang = "ita")
}
\arguments{
\item{dataset}{as data.frame}

\item{lang}{language parameter for labels ("ita" for Italian, "eng" for English)}
}
\value{
It returns the filtered data set.
}
\description{
An interactive and more intuitive version of filter_istat function. It filters data set by column(s). Takes as input a data.frame (not only ISTAT ones) and allows you to select for which column(s) value(s) to filter the data set interactively.
}
\note{
In this first version, language parameter works only with data sets downloaded with get_i_stat (provider I.Stat).
}
\examples{
\donttest{filter_istat_interactive(iris)}
}
