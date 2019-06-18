\name{genera_cn}
\alias{genera_cn}
\docType{data}
\title{
Genera of higher plants with Chinese Names
}
\description{
A database containing the Chinese Names for higher plants
}
\usage{data("genera_cn")}
\format{
  A data frame with 7014 observations on the following 3 variables.
  \describe{
    \item{\code{GENUS_CN}}{a character vector showing the Chinese Names}
    \item{\code{GENUS}}{a character vector showing the genus}
    \item{\code{GENUS_AUTHOR}}{a character vector showing the author who described the genus}
  }
}
\details{
This database only contains the genera that have chinese names. Note that it is not comprehensive. The names mainly follow Flora of China (\url{http://www.efloras.org/flora_page.aspx?flora_id=2}). New Chinese names are given to the recently discovered/described genera in- and outside China. 

The Chinese names for brophytes, ferns, gymnospersm come from the DUOCET website \url{http://duocet.ibiodiversity.net}. On this website, a group of young botanist are working on naming the plants in Chinese based on the existing scientific names.

}
\source{

\url{http://duocet.ibiodiversity.net}

\url{http://www.biodiversity-science.net/CN/10.17520/biods.2015052}

}
\references{

Bing Liu, Jianfei Ye, Su Liu, Yuan Wang, Yong Yang, Yangjun Lai, Gang Zeng, Qinwen Lin.(2015) Families and genera of Chinese angiosperms: A synoptic classification based on APG III. Biodiversity Science, 23(2), 225-231.
\url{http://www.biodiversity-science.net/CN/10.17520/biods.2015052}
}
\examples{
data(genera_cn)
}
\keyword{datasets}
