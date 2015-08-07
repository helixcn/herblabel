.onAttach <- function(libname, pkgname)  {
    Sys.setlocale("LC_TIME", "C")
    packageStartupMessage(paste("This is herblabel ", packageVersion("herblabel"),"\n", sep = ""))
    packageStartupMessage(paste("To input data, see Darwin Core Template: \n", system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.XLSX", package = "herblabel"),"\n", sep = ""))
    packageStartupMessage(paste("To generate herbarium specimen labels, see the R script for a Working Example at: \n", system.file("extdata", "working_example_herblabel.R", package = "herblabel"),"\n", sep = ""))
}
