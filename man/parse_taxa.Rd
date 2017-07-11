\name{parse_taxa}
\alias{parse_taxa}
\title{
Parsing scientific names for a species
}
\description{
Parsing scientif names to FAMILY,GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK
}
\usage{
parse_taxa(taxa)
}

\arguments{
  \item{taxa}{
A string representing scientific name
}
}
\details{
This functions finds the gaps in the scientific name, and parse the name to GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK. There may be errors in parsing scientific name with "f.". 
}
\value{
A dataframe containing the following columns TAXON_PARSED, GENUS_PARSED, SPECIES_PARSED, AUTHOR_OF_SPECIES_PARSED, INFRASPECIFIC_RANK_PARSED, INFRASPECIFIC_EPITHET_PARSED, AUTHOR_OF_INFRASPECIFIC_RANK_PARSED
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
See Also as \code{\link{fill_dwc}} for the application of this function.
} 
\examples{

parse_taxa("Acer")
parse_taxa("Acer caudatifolium Hayata")
parse_taxa("Acer caudatifolium")
parse_taxa("Acer sp.")
parse_taxa("Acer caudatum Wall. var. multiserratum (Maxim.) Rehd.")
parse_taxa("Acer caudatum var. multiserratum (Maxim.) Rehd.")
parse_taxa("Acer caudatum Wall. var. multiserratum")
parse_taxa("Acer caudatum var. multiserratum")
parse_taxa("Acer caesium Wall. ex Brandis subsp. giraldii (Pax) E. Murr.")
parse_taxa("Acer caesium subsp. giraldii (Pax) E. Murr.")
parse_taxa("Acer caesium Wall. ex Brandis subsp. giraldii")
parse_taxa("Acer caesium subsp. giraldii")                               

############# Not a real species #############

tax <- "   Acanthodium   furcatum ( L. f. ) Nees        "
parse_taxa(tax)

taxon <- "   Acanthodium   furcatum ( L. f. ) Nees        "
parse_taxa(taxon)


tax <- "Acanthus ebracteatus var. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu            "
parse_taxa(tax)

tax <- "Acanthus ebracteatus subsp. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu          "
parse_taxa(tax)

tax <- "Acanthus ebracteatus XXXX var. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu       "
parse_taxa(tax)

tax <- "Acanthus ebracteatus XXXX f. subsp. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu  "
parse_taxa(tax)

tax <- "Acanthus ebracteatus XXXX  subsp. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu f. "
parse_taxa(tax)

tax <- "Acanthus ebracteatus XXXX f. subsp. xiamenensis (R.T.Zhang) C.Y.Wu & C.C.Hu  "
parse_taxa(tax)

tax <- "Acanthodium furcatum f. variagata Nees      "
parse_taxa(tax)

tax <- "Acanthodium furcatum (L. f. ) Nees          "
parse_taxa(tax)

tax <- "Acanthus ebracteatus subsp. ebracteatus"
parse_taxa(tax)

}

\keyword{ taxonomic }
