\name{clipX-package}
\alias{clipX-package}
\docType{package}
\title{process clipboard content}
\description{
Tiny \pkg{clipr} extension that simply puts out clipboard content as a ready to paste \code{c()} string.
}
\details{
\tabular{ll}{
Package: \tab clipX\cr
Version: \tab 0.1.1\cr
License: \tab GNU\cr
Imports: \tab clipr, stringi\cr
Built: \tab R 4.2.0; ; 2024-01-25 14:42:20 UTC; unix\cr
}

This is a tiny function to process clipboard content. you can directly paste in the newly created clipboard content which consists of a string wrapped array like \code{c("eins","zwei","drei")} into a commandline starting with an assignment \code{var = }. so you first execute the \code{\link{clipX}} function that converts the actual clipboard content to the desired string, then paste the clipboard to the position after your assignment command.
}
\author{
esteeschwarz
}
\references{
}
\keyword{ package }
\seealso{}
\examples{

}
