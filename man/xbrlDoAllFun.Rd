% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xbrlDoAllFun.R
\name{xbrlDoAllFun}
\alias{xbrlDoAllFun}
\title{A mirror function of XBRL}
\usage{
xbrlDoAllFun(
  file.inst,
  cache.dir = "xbrl.Cache",
  prefix.out = NULL,
  verbose = FALSE,
  delete.cached.inst = TRUE
)
}
\value{
A XBRL list of data.frames
}
\description{
A mirror function to package XBRL function xbrlDoAll
}
\details{
This function is almost exactly equal to the function xbrlDoAll of the
package XBRL. The reason it exist is because the above mentioned doesn´t work
with https type links, which is fixed by editing the fixFileName function
inside the XBRL function.
On linux I was able to built it but It seams I´m not able to do so on
Windows because I don´t know how to make libxml accesible to Rtools. For the
mean time, this is a workaround.
}
\keyword{XBRL}
