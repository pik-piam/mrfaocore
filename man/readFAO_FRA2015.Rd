% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFAO_FRA2015.R
\name{readFAO_FRA2015}
\alias{readFAO_FRA2015}
\title{Read FAO_FRA2015}
\usage{
readFAO_FRA2015(subtype)
}
\arguments{
\item{subtype}{data subtype. Either "production" or "fac" (forest area and characteristics)
or "biodiversity" or "anndat" (Annual Data)}
}
\value{
magpie object of the FRA 2015 data
}
\description{
Read-in an FRA data from 2015 (forest resource assessment)
}
\examples{
\dontrun{
a <- readSource("FAO_FRA2015", "production")
}

}
\seealso{
[readSource()]
}
\author{
Abhijeet Mishra
}
