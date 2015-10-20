# Convert the BG-BASE exported csv file to herbarium templated

bgbase_csv2ht  <- function(infile, outfile = NULL, 
                                HERBARIUM = "Kadoorie Farm and Botanic Garden Herbarium (KFBG)",
                                HERBARIUM_CODE = "KFBG", 
                                TITLE = NA,
                                PROJECT = NA, 
                                PROCESSED_BY = NA, 
                                DATE_LASTMODIFIED = NA
                                )
{
    cap  <- function(x) {
        paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
    }

    format_GUID <- function(x){
        no2guid <- function(x, HERBARIUM_CODE = "KFBG", digits = 6){  ## digits for generating barcodes
           if(nchar(x) > 6){
              stop("Number of digits in Coll ID is greater than digits.")
           }
           res <-  paste(HERBARIUM_CODE, paste(rep("0", digits - nchar(x)), collapse = ""), x, sep = "")
           return(res)
        }
        res <- c()
        for(i in 1:length(x)){
        if(is.na(x[i])|x[i] == ""){
            res[i] <- " "
        } else {
            res[i] <- no2guid(x[i])
            }
        }
            return(res)
    }
    
    ###
    bg_base_export_dat    <- read.csv( infile, header = TRUE, stringsAsFactors = FALSE)
    bg_base_export_dat    <- bg_base_export_dat[1:(nrow(bg_base_export_dat)-1),]
    bg_base_export_dat    <- bg_base_export_dat[!bg_base_export_dat$SPECIMENS == " ", ]
    ADDITIONAL_COLLECTOR  <- ifelse(bg_base_export_dat$COLLECTED_WITH            == " ", NA,  bg_base_export_dat$COLLECTED_WITH           )
    ATTRIBUTES            <- ifelse(bg_base_export_dat$DESCRIPTION               == " ", NA,  bg_base_export_dat$DESCRIPTION              )
    COLLECTOR             <- ifelse(bg_base_export_dat$COLLECTOR                 == " ", NA,  bg_base_export_dat$COLLECTOR                )
    COLLECTOR_NUMBER      <- ifelse(bg_base_export_dat$COLL_.                    == " ", NA,  bg_base_export_dat$COLL_.                   )
    COUNTRY               <- ifelse(toupper(bg_base_export_dat$COUNTRY_ON_LABEL) == " ", NA,  toupper(bg_base_export_dat$COUNTRY_ON_LABEL))
    COUNTY                <- ifelse(bg_base_export_dat$SUB.SUB.COUNTRY           == " ", NA,  bg_base_export_dat$SUB.SUB.COUNTRY          )
    DATE_COLLECTED        <- ifelse(bg_base_export_dat$COLL_DT                   == " ", NA,  bg_base_export_dat$COLL_DT                  )
    DATE_IDENTIFIED       <- ifelse(bg_base_export_dat$REC_UPDATE                == " ", NA,  bg_base_export_dat$REC_UPDATE               )
    ELEVATION             <- ifelse(bg_base_export_dat$ALTITUDE                  == " ", NA,  bg_base_export_dat$ALTITUDE                 )

    GEOREFERENCE_SOURCES  <- ifelse(bg_base_export_dat$GEOREF                    == " ", NA,  bg_base_export_dat$GEOREF                   )
    

    GLOBAL_UNIQUE_IDENTIFIER <- format_GUID(bg_base_export_dat$REC_ID)
    
    IDENTIFIED_BY         <- ifelse(bg_base_export_dat$BY                        == " ", NA,  bg_base_export_dat$BY                       )
    LAT_DEGREE            <- ifelse(bg_base_export_dat$LATD                      == " ", NA,  bg_base_export_dat$LATD                     )
    LAT_FLAG              <- ifelse(bg_base_export_dat$LADI                      == " ", NA,  bg_base_export_dat$LADI                     )
    LAT_MINUTE            <- ifelse(bg_base_export_dat$LATM                      == " ", NA,  bg_base_export_dat$LATM                     )
    LAT_SECOND            <- ifelse(bg_base_export_dat$LATS                      == " ", NA,  bg_base_export_dat$LATS                     )
    LOCALITY1             <- ifelse(bg_base_export_dat$SUB.SUB.SUB.COUNTRY       == " ", NA,  bg_base_export_dat$SUB.SUB.SUB.COUNTRY      )
    LOCALITY2             <- ifelse(bg_base_export_dat$LOCALITY                  == " ", NA,  bg_base_export_dat$LOCALITY                 )
    LON_DEGREE            <- ifelse(bg_base_export_dat$LONGD                     == " ", NA,  bg_base_export_dat$LONGD                    )
    LON_FLAG              <- ifelse(bg_base_export_dat$LODI                      == " ", NA,  bg_base_export_dat$LODI                     )
    LON_MINUTE            <- ifelse(bg_base_export_dat$LONGM                     == " ", NA,  bg_base_export_dat$LONGM                    )
    LON_SECOND            <- ifelse(bg_base_export_dat$LONGS                     == " ", NA,  bg_base_export_dat$LONGS                    )
    REMARKS1              <- ifelse(bg_base_export_dat$COLLECTION_MISC           == " ", NA,  bg_base_export_dat$COLLECTION_MISC          )
    REMARKS2              <- ifelse(bg_base_export_dat$HABITAT                   == " ", NA,  bg_base_export_dat$HABITAT                  )
    REMARKS3              <- ifelse(bg_base_export_dat$ACCESSION_.               == " ", NA,  bg_base_export_dat$ACCESSION_.              )


    SPECIMEN_LOCATION     <- ifelse(bg_base_export_dat$LOCN                      == " ", NA,  bg_base_export_dat$LOCN                     )
    STATE_PROVINCE        <- ifelse(bg_base_export_dat$SUB.COUNTRY               == " ", NA,  bg_base_export_dat$SUB.COUNTRY              )
    LOCAL_NAME            <- ifelse(bg_base_export_dat$SCIENTIFIC_NAME           == " ", NA,  bg_base_export_dat$SCIENTIFIC_NAME          )
    
    HERBARIUM                    <- rep(HERBARIUM,         nrow(bg_base_export_dat))
    TITLE                        <- rep(TITLE,             nrow(bg_base_export_dat))
    FAMILY                       <- rep(NA,                nrow(bg_base_export_dat))
    GENUS                        <- rep(NA,                nrow(bg_base_export_dat))
    SPECIES                      <- rep(NA,                nrow(bg_base_export_dat))
    AUTHOR_OF_SPECIES            <- rep(NA,                nrow(bg_base_export_dat))
    INFRASPECIFIC_RANK           <- rep(NA,                nrow(bg_base_export_dat))
    INFRASPECIFIC_EPITHET        <- rep(NA,                nrow(bg_base_export_dat))
    AUTHOR_OF_INFRASPECIFIC_RANK <- rep(NA,                nrow(bg_base_export_dat))
    IMAGE_URL                    <- rep(NA,                nrow(bg_base_export_dat))
    RELATED_INFORMATION          <- rep(NA,                nrow(bg_base_export_dat))
    PROJECT                      <- rep(PROJECT,           nrow(bg_base_export_dat))
    TYPE_STATUS                  <- rep(NA,                nrow(bg_base_export_dat))
    PROCESSED_BY                 <- rep(PROCESSED_BY,      nrow(bg_base_export_dat))
    DATE_LASTMODIFIED            <- rep(DATE_LASTMODIFIED, nrow(bg_base_export_dat))
    
    LOCALITY    <- paste(ifelse(is.na(LOCALITY1), "", LOCALITY1),
                         ifelse(is.na(LOCALITY2), "", LOCALITY2), sep = " ")
    REMARKS     <- paste(ifelse(is.na(REMARKS1),  "", REMARKS1 ), 
                         ifelse(is.na(REMARKS2),  "", REMARKS2 ), 
                         ifelse(is.na(REMARKS3),  "", 
                         paste("Living Acc. No.:", REMARKS3)), sep = " ")
    res.final   <- data.frame(GLOBAL_UNIQUE_IDENTIFIER, HERBARIUM, TITLE, 
                             COLLECTOR, ADDITIONAL_COLLECTOR, 
                             COLLECTOR_NUMBER, DATE_COLLECTED, LOCAL_NAME, 
                             FAMILY, GENUS, SPECIES, AUTHOR_OF_SPECIES, 
                             INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, 
                             AUTHOR_OF_INFRASPECIFIC_RANK, COUNTRY, 
                             STATE_PROVINCE, COUNTY, LOCALITY, IMAGE_URL, 
                             RELATED_INFORMATION, LAT_DEGREE, LAT_MINUTE,
                             LAT_SECOND, LAT_FLAG, LON_DEGREE, LON_MINUTE, 
                             LON_SECOND, LON_FLAG, ELEVATION, ATTRIBUTES,
                             REMARKS, GEOREFERENCE_SOURCES, PROJECT, 
                             IDENTIFIED_BY, DATE_IDENTIFIED, TYPE_STATUS, 
                             PROCESSED_BY, DATE_LASTMODIFIED, 
                             SPECIMEN_LOCATION)
    if(!is.null(outfile)){
        write.csv(res.final, outfile, row.names = FALSE)
        cat("herbarium records have been converted and saved to:\n", 
            file.path(getwd(), outfile),"\n", sep = "")
    }
    return(res.final)
}
