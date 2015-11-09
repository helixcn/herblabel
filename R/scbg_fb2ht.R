########## Converting the template of fieldbook of 
########## South China Botanic Garden to the herblabel template 

scbg_fb2ht  <- function(infile, outfile = "herblabel_temp.csv", 
                                HERBARIUM_ARGUMENT = "Kadoorie Farm and Botanic Garden Herbarium",
                                HERBARIUM_CODE_ARGUMENT = "KFBG", 
                                TITLE_ARGUMENT = NA,
                                PROJECT_ARGUMENT = NA, 
                                IDENTIFIED_BY_ARGUMENT = NA,   
                                DATE_IDENTIFIED_ARGUMENT = NA,
                                PROCESSED_BY_ARGUMENT = NA, 
                                DATE_LASTMODIFIED_ARGUMENT = NA
                                )
{
    herbdat000 <- read.csv(infile, header = TRUE, stringsAsFactors = FALSE)
    GLOBAL_UNIQUE_IDENTIFIER      <- rep(NA, nrow(herbdat000))
    HERBARIUM                     <- rep(NA, nrow(herbdat000))
    TITLE                         <- rep(NA, nrow(herbdat000))
    COLLECTOR                     <- rep(NA, nrow(herbdat000))
    ADDITIONAL_COLLECTOR          <- rep(NA, nrow(herbdat000))
    COLLECTOR_NUMBER              <- rep(NA, nrow(herbdat000))
    DATE_COLLECTED                <- rep(NA, nrow(herbdat000))
    LOCAL_NAME                    <- rep(NA, nrow(herbdat000))
    FAMILY                        <- rep(NA, nrow(herbdat000))
    GENUS                         <- rep(NA, nrow(herbdat000))
    SPECIES                       <- rep(NA, nrow(herbdat000))
    AUTHOR_OF_SPECIES             <- rep(NA, nrow(herbdat000))
    INFRASPECIFIC_RANK            <- rep(NA, nrow(herbdat000))
    INFRASPECIFIC_EPITHET         <- rep(NA, nrow(herbdat000))
    AUTHOR_OF_INFRASPECIFIC_RANK  <- rep(NA, nrow(herbdat000))
    COUNTRY                       <- rep(NA, nrow(herbdat000))
    STATE_PROVINCE                <- rep(NA, nrow(herbdat000))
    COUNTY                        <- rep(NA, nrow(herbdat000))
    LOCALITY                      <- rep(NA, nrow(herbdat000))
    IMAGE_URL                     <- rep(NA, nrow(herbdat000))
    RELATED_INFORMATION           <- rep(NA, nrow(herbdat000))
    LAT_DEGREE                    <- rep(NA, nrow(herbdat000))
    LAT_MINUTE                    <- rep(NA, nrow(herbdat000))
    LAT_SECOND                    <- rep(NA, nrow(herbdat000))
    LAT_FLAG                      <- rep(NA, nrow(herbdat000))
    LON_DEGREE                    <- rep(NA, nrow(herbdat000))
    LON_MINUTE                    <- rep(NA, nrow(herbdat000))
    LON_SECOND                    <- rep(NA, nrow(herbdat000))
    LON_FLAG                      <- rep(NA, nrow(herbdat000))
    ELEVATION                     <- rep(NA, nrow(herbdat000))
    ATTRIBUTES                    <- rep(NA, nrow(herbdat000))
    REMARKS                       <- rep(NA, nrow(herbdat000))
    GEOREFERENCE_SOURCES          <- rep(NA, nrow(herbdat000))
    PROJECT                       <- rep(NA, nrow(herbdat000))
    IDENTIFIED_BY                 <- rep(NA, nrow(herbdat000))
    DATE_IDENTIFIED               <- rep(NA, nrow(herbdat000))
    TYPE_STATUS                   <- rep(NA, nrow(herbdat000))
    PROCESSED_BY                  <- rep(NA, nrow(herbdat000))
    DATE_LASTMODIFIED             <- rep(NA, nrow(herbdat000))
    SPECIMEN_LOCATION             <- rep(NA, nrow(herbdat000))

    for(i in 1:nrow(herbdat000)){
        herbdat <- herbdat000[i,]
        herbdat[herbdat == ""] <- NA
        GLOBAL_UNIQUE_IDENTIFIER    [i] <- ifelse(is.null(herbdat$GLOBAL_UNIQUE_IDENTIFIER ),NA,                         herbdat$GLOBAL_UNIQUE_IDENTIFIER  )
        HERBARIUM                   [i] <- ifelse(is.null(herbdat$HERBARIUM                ),HERBARIUM_ARGUMENT,         herbdat$HERBARIUM                 )
        TITLE                       [i] <- ifelse(is.null(herbdat$TITLE                    ),TITLE_ARGUMENT,             herbdat$TITLE                     )
        COLLECTOR                   [i] <- ifelse(is.null(herbdat$COLLECTOR                ),NA,                         herbdat$COLLECTOR                 )
        ADDITIONAL_COLLECTOR        [i] <- ifelse(is.null(herbdat$ADDITIONAL_COLLECTOR     ),NA,                         herbdat$ADDITIONAL_COLLECTOR      )
        COLLECTOR_NUMBER            [i] <- ifelse(is.null(herbdat$COLLECTOR_NUMBER         ),NA,                         herbdat$COLLECTOR_NUMBER          )
        DATE_COLLECTED              [i] <- ifelse(is.null(herbdat$COLL_DATE                ),NA,                         herbdat$COLL_DATE                 )
        LOCAL_NAME                  [i] <- ifelse(is.null(herbdat$LOCAL_NAME               ),NA,                         herbdat$LOCAL_NAME                )
        FAMILY                      [i] <- ifelse(is.null(herbdat$FAMILY                   ),NA,                         herbdat$FAMILY                    )
        GENUS                       [i] <- ifelse(is.null(herbdat$FIELD_ID_GENUS           ),NA,                         herbdat$FIELD_ID_GENUS            )
        SPECIES                     [i] <- ifelse(is.null(herbdat$FIELD_ID_SP              ),NA,                         herbdat$FIELD_ID_SP               )
        AUTHOR_OF_SPECIES           [i] <- ifelse(is.null(herbdat$FIELD_ID_AUTHOR          ),NA,                         herbdat$FIELD_ID_AUTHOR           )
        INFRASPECIFIC_RANK          [i] <- ifelse(is.null(herbdat$FIELD_ID_INFRA_RANK      ),NA,                         herbdat$FIELD_ID_INFRA_RANK       )
        INFRASPECIFIC_EPITHET       [i] <- ifelse(is.null(herbdat$FIELD_ID_INFRA_SP        ),NA,                         herbdat$FIELD_ID_INFRA_SP         )
        AUTHOR_OF_INFRASPECIFIC_RANK[i] <- ifelse(is.null(herbdat$FIELD_ID_INFRA_AUTHOR    ),NA,                         herbdat$FIELD_ID_INFRA_AUTHOR     )
        COUNTRY                     [i] <- ifelse(is.null(herbdat$COUNTRY                  ),NA,                         herbdat$COUNTRY                   )
        STATE_PROVINCE              [i] <- ifelse(is.null(herbdat$STATE_PROVINCE           ),NA,                         herbdat$STATE_PROVINCE            )
        COUNTY                      [i] <- ifelse(is.null(herbdat$COUNTY                   ),NA,                         herbdat$COUNTY                    )
        LOCALITY                    [i] <- ifelse(is.null(herbdat$LOCALITY                 ),NA,                         herbdat$LOCALITY                  )
        IMAGE_URL                   [i] <- ifelse(is.null(herbdat$IMAGE_URL                ),NA,                         herbdat$IMAGE_URL                 )
        RELATED_INFORMATION         [i] <- ifelse(is.null(herbdat$RELATED_INFORMATION      ),NA,                         herbdat$RELATED_INFORMATION       )
        LAT_DEGREE                  [i] <- ifelse(is.null(herbdat$LAT_DEGREE               ),NA,                         herbdat$LAT_DEGREE                )
        LAT_MINUTE                  [i] <- ifelse(is.null(herbdat$LAT_MINUTE               ),NA,                         herbdat$LAT_MINUTE                )
        LAT_SECOND                  [i] <- ifelse(is.null(herbdat$LAT_SECOND               ),NA,                         herbdat$LAT_SECOND                )
        LAT_FLAG                    [i] <- ifelse(is.null(herbdat$LAT_FLAG                 ),NA,                         herbdat$LAT_FLAG                  )
        LON_DEGREE                  [i] <- ifelse(is.null(herbdat$LON_DEGREE               ),NA,                         herbdat$LON_DEGREE                )
        LON_MINUTE                  [i] <- ifelse(is.null(herbdat$LON_MINUTE               ),NA,                         herbdat$LON_MINUTE                )
        LON_SECOND                  [i] <- ifelse(is.null(herbdat$LON_SECOND               ),NA,                         herbdat$LON_SECOND                )
        LON_FLAG                    [i] <- ifelse(is.null(herbdat$LON_FLAG                 ),NA,                         herbdat$LON_FLAG                  )
        ELEVATION                   [i] <- ifelse(is.null(herbdat$ELEVATION                ),NA,                         herbdat$ELEVATION                 )
        ATTRIBUTES                  [i] <- ifelse(all(c(is.na(herbdat$ATTRIBUTES_1), 
                                                        is.na(herbdat$ATTRIBUTES_2), 
                                                        is.na(herbdat$ATTRIBUTES_3), 
                                                        is.na(herbdat$ATTRIBUTES_4), 
                                                        is.na(herbdat$ATTRIBUTES_5))), NA,  paste(na.omit(c(
                                                                                               ifelse(is.na(herbdat$ATTRIBUTES_1), NA, paste("Growth Form:", herbdat$ATTRIBUTES_1)),
                                                                                               ifelse(is.na(herbdat$ATTRIBUTES_2), NA, paste("Bark:", herbdat$ATTRIBUTES_2)),
                                                                                               ifelse(is.na(herbdat$ATTRIBUTES_3), NA, paste("Leave:", herbdat$ATTRIBUTES_3)),
                                                                                               ifelse(is.na(herbdat$ATTRIBUTES_4), NA, paste("Flower:", herbdat$ATTRIBUTES_4)),
                                                                                               ifelse(is.na(herbdat$ATTRIBUTES_5), NA, paste("Fruit or Seed:", herbdat$ATTRIBUTES_5)))), 
                                                                                              collapse = ", "))
        REMARKS                     [i] <- ifelse(all(c(is.na(herbdat$REMARKS_1), is.na(herbdat$REMARKS_2))),NA, paste(na.omit(c(herbdat$REMARKS_1, 
                                                                                                                           herbdat$REMARKS_2)), collapse = ", "))
        GEOREFERENCE_SOURCES        [i] <- ifelse(is.null(herbdat$GEOREFERENCE_SOURCES     ),NA,                         herbdat$GEOREFERENCE_SOURCES      )
        PROJECT                     [i] <- ifelse(is.null(herbdat$PROJECT                  ),PROJECT_ARGUMENT,           herbdat$PROJECT                   )
        IDENTIFIED_BY               [i] <- ifelse(is.null(herbdat$IDENTIFIED_BY            ),IDENTIFIED_BY_ARGUMENT,     herbdat$IDENTIFIED_BY             )
        DATE_IDENTIFIED             [i] <- ifelse(is.null(herbdat$DATE_IDENTIFIED          ),DATE_IDENTIFIED_ARGUMENT,   herbdat$DATE_IDENTIFIED           )
        TYPE_STATUS                 [i] <- ifelse(is.null(herbdat$TYPE_STATUS              ),NA,                         herbdat$TYPE_STATUS               )
        PROCESSED_BY                [i] <- ifelse(is.null(herbdat$PROCESSED_BY             ),PROCESSED_BY_ARGUMENT,      herbdat$PROCESSED_BY              )
        DATE_LASTMODIFIED           [i] <- ifelse(is.null(herbdat$DATE_LASTMODIFIED        ),DATE_LASTMODIFIED_ARGUMENT, herbdat$DATE_LASTMODIFIED         )
        SPECIMEN_LOCATION           [i] <- ifelse(is.null(herbdat$SPECIMEN_LOCATION        ),NA,                         herbdat$SPECIMEN_LOCATION         )
    }
    res  <- data.frame(GLOBAL_UNIQUE_IDENTIFIER, HERBARIUM, TITLE, COLLECTOR, ADDITIONAL_COLLECTOR, 
                             COLLECTOR_NUMBER, DATE_COLLECTED, LOCAL_NAME, FAMILY, GENUS, SPECIES, AUTHOR_OF_SPECIES, 
                             INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK, COUNTRY, 
                             STATE_PROVINCE, COUNTY, LOCALITY, IMAGE_URL, RELATED_INFORMATION, LAT_DEGREE, LAT_MINUTE,
                             LAT_SECOND, LAT_FLAG, LON_DEGREE, LON_MINUTE, LON_SECOND, LON_FLAG, ELEVATION, ATTRIBUTES,
                             REMARKS, GEOREFERENCE_SOURCES, PROJECT, IDENTIFIED_BY, DATE_IDENTIFIED, TYPE_STATUS, 
                             PROCESSED_BY, DATE_LASTMODIFIED, SPECIMEN_LOCATION)
    if(!is.null(outfile)){
        write.csv(res, outfile, row.names = FALSE)
        cat("Herbarium records have been converted and saved to:\n", file.path(getwd(), outfile),"\n", sep = "")
    }
    return(res)
}
