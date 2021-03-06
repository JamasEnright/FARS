% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read FARS file}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A file name for the function to read in}
}
\value{
This function returns the file as a data file.
}
\description{
This is a simple function that reads in a file (called \code{filename}) and returns a data file.
The function expects the file to be in a CSV format.
}
\details{
It checks to make sure the file exists first, and generates an error if it does not.
}
\examples{

\dontrun{
fars_read("inputfile.csv")
fars_read("accident_2001.csv")
fars_read(filenamevar)
fars_read("madeupfile.ext")
}

}
