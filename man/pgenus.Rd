\name{pgenus}
\alias{pgenus}
\docType{data}
\title{
The Genus-Family Relationship for higher plants based on Plantlist Website
}
\description{
Curated data for the Genus-Family Relationship for all the higher plants based on Plantlist Website.
}
\usage{data("pgenus")}
\format{
  A data frame with 24815 observations on the following 2 variables.
  \describe{
    \item{\code{GENUS}}{a character vector}
    \item{\code{FAMILY}}{a character vector}
  }
}
\details{
This dataframe contains the Famlies for all the genera of higher plants including Moss, Ferns, Gymnosperms and Angiosperms. The classification systems are: 
Ferns: Christenhusz, M., Zhang, X. C., and Schneider, H. (2011);
Gymnosperms: Christenhusz, M., Reveal, J., Farjon, A., Gardner, M. F., Mill, R. R., and Chase, M. W. (2011);
Angiosperms: APGIII, Bremer, Birgitta, K. Bremer, Mark Chase, Mike Fay, James Reveal, Douglas Soltis, Pamella Soltis and Peter Stevens. (2009).
}
\source{
 \url{ http://www.theplantlist.org/1.1/browse/-/-/ }
}
\references{

The Plant List (2013). Version 1.1. Published on the Internet; \url{ http://www.theplantlist.org/ } (accessed 26 July 2016). 

Bremer, Birgitta, K. Bremer, Mark Chase, Mike Fay, James Reveal, Douglas Soltis, Pamella Soltis and Peter Stevens. (2009) "An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG III." Botanical Journal of the Linnean Society.

Christenhusz, M., Zhang, X. C., and Schneider, H. (2011). A linear sequence of extant families and genera of lycophytes and ferns. Phytotaxa. 19:7-54

Christenhusz, M., Reveal, J., Farjon, A., Gardner, M. F., Mill, R. R., and Chase, M. W. (2011). A new classification and linear sequence of extant gymnosperms. Phytotaxa. 19:55-70

}
\examples{
data(pgenus)
}
\keyword{datasets}
