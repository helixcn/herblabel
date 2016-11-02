\name{spfrps}
\alias{spfrps}
\docType{data}
\title{
The Chinese Names and full scientific names for the vascular Plants in China
}
\description{
The dataframe contains all the Chinese (Both Traditional Chinese and Simplified Chinese) and the scientific names for the vascular plants in China, as included in Flora Rpublicae Popularis Sinicae, as well as all the Chinese Names of for the genera by Liu et al. 2014. 
}
\usage{data("spfrps")}
\format{
  A data frame with 77832 observations on the following 2 variables.
  \describe{
    \item{\code{NAME_CN}}{a character vector}
    \item{\code{SCIENTIFIC_NAME}}{a character vector}
  }
}
\details{
Encoding of the Chinese Characters is "UTF-8".
}
\source{
Chinese Virtual Herbarium \url{ http://www.cvh.org.cn/cnpc }
}
\references{
\url{ http://frps.eflora.cn/ }
}
\examples{
data(spfrps)
}
\keyword{datasets}
