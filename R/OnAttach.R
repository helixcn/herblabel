.onAttach <- function(libname, pkgname)  {

    syst <- Sys.info()[['sysname']]
    if(syst == "Windows"){
        suppressMessages(Sys.setlocale("LC_TIME", "C"))
    } 
    
    options(stringsAsFactors = FALSE)
    options(scipen=999)
    packageStartupMessage(paste("This is herblabel ", packageVersion("herblabel"),".", sep = ""))
}
