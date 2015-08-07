.onAttach <- function(libname, pkgname)  {
    Sys.setlocale("LC_TIME", "C")
    options(stringsAsFactors = FALSE)
    packageStartupMessage(paste("This is R package herblabel ", packageVersion("herblabel"),"\n", sep = ""))
    packageStartupMessage(paste("To input data, see Darwin Core Template at: \n", system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.xlsx", package = "herblabel"),"\n", sep = ""))
    packageStartupMessage(paste("To generate herbarium specimen labels, see the R script for a Working Example at: \n", system.file("extdata", "working_example_herblabel.R", package = "herblabel"), sep = ""))
}
