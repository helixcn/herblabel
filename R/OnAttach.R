.onAttach <- function(libname, pkgname)  {
    Sys.setlocale("LC_TIME", "C")
    options(stringsAsFactors = FALSE)
    packageStartupMessage(paste("This is herblabel ", packageVersion("herblabel"),".", sep = ""))
    ### packageStartupMessage(paste("To input data, see Excel Template at: \n", system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.xlsx", package = "herblabel"),"\n", sep = ""))
    ### packageStartupMessage(paste("To make labels, see the R script at: \n", system.file("extdata", "working_example_herblabel.R", package = "herblabel"), sep = ""))
}
