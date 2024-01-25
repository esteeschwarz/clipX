\name{clipX}
\alias{clipX}
\title{clipboard processing}
\description{
  This is a function that simply puts out clipboard content as ready to paste \code{c()} string.
}
\usage{
clipX (sep = "")
}
 \arguments{
  \item{sep}{possible seperator for clipboard content. you can set that if the text is seperated by another seperator than \code{c(","," ","\n")}
}
}
 \value{returns clipboard content as string combined in the form: \code{c("eins","zwei","drei","...")}}
 \examples{}
 \keyword{IO}
 \concept{writing R}
