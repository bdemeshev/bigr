% Generated by roxygen2 (4.0.2): do not edit by hand
\name{str_utf2cp}
\alias{str_utf2cp}
\title{Change encoding of cyrillic text from "utf8" to "cp1251"}
\usage{
str_utf2cp(x)
}
\arguments{
\item{x}{the vector or data.frame of cyrillic characters}
}
\value{
reencoded vector or data.frame
}
\description{
This function changes encoding of a vector or data.frame from "utf8" to "cp1251"
}
\examples{
str_utf2cp("привет")
}

