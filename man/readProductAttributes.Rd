% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readProductAttributes.R
\name{readProductAttributes}
\alias{readProductAttributes}
\title{Read product attributes}
\usage{
readProductAttributes(subtype = "Products")
}
\arguments{
\item{subtype}{Available subtypes: "Products", MAgPIE products "AgResidues"
Aboveground crop residues and "BgResidues" Belowground crop residues}
}
\value{
magpie object with the dimension crops and attributes
}
\description{
Read-in a file containing the attributes of MAgPIE products. Currently
Covers dry matter (DM), reactive nitrogen (Nr), Phosphorus (P),
Generalizable Energy (GE) and wet matter (WM). Values are assembled from
various literature sources, and the weighting and allocation is done in the
spreadsheet crop_specifications_06_2011.ods and
livestock_specifications_2012_06_14.ods in the svn folder /tools/Nutrients .
Values standardized on DM.
}
\examples{
\dontrun{
a <- readSource("ProductAttributes")
}
}
\seealso{
[readSource()]
}
\author{
Benjamin Leon Bodrisky
}
