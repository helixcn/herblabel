.onAttach <- function(libname, pkgname)  {
    suppressMessages(Sys.setlocale("LC_TIME", "C"))
    options(stringsAsFactors = FALSE)
    options(scipen=999)
    packageStartupMessage(paste("This is herblabel ", packageVersion("herblabel"),".", sep = ""))
}
