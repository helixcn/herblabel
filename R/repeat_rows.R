repeat_rows <-
    function(dat = NULL) {
        if (is.null(dat)) {
            stop("No data supplied.")
        } else {
            col_names <- colnames(dat)
        }
        
        if (!"NUMBER_OF_LABELS_TO_CREATE" %in% col_names) {
            NUMBER_OF_LABELS_TO_CREATE <- rep(1, nrow(dat))
        } else {
            ### Detect missing values
            NUMBER_OF_LABELS_TO_CREATE <-
                as.character(dat$NUMBER_OF_LABELS_TO_CREATE)
        }
        ### If any is missing, replace with ONE
        NUMBER_OF_LABELS_TO_CREATE[is.na(NUMBER_OF_LABELS_TO_CREATE)] <-
            1
        
        
        ### Convert to integers
        NUMBER_OF_LABELS_TO_CREATE <-
            as.integer(NUMBER_OF_LABELS_TO_CREATE)
        
        ### All the integers < 1 will be replaced using one to avoid typo error.
        if (any(which(NUMBER_OF_LABELS_TO_CREATE < 1))) {
            warning(
                paste(
                    "In Column NUMBER_OF_LABELS_TO_CREATE, value in row:",
                    paste(which(NUMBER_OF_LABELS_TO_CREATE < 1), collapse = ","),
                    " less than one. Replaced using one.",
                    sep = ""
                )
            )
            NUMBER_OF_LABELS_TO_CREATE[NUMBER_OF_LABELS_TO_CREATE < 1] <-
                1
        }
        
        if (any(which(is.na(NUMBER_OF_LABELS_TO_CREATE)))) {
            warning(paste(
                "In Column NUMBER_OF_LABELS_TO_CREATE check rows:",
                paste(which(
                    is.na(NUMBER_OF_LABELS_TO_CREATE)
                ),
                collapse = ","),
                sep = ""
            ))
            NUMBER_OF_LABELS_TO_CREATE[is.na(NUMBER_OF_LABELS_TO_CREATE)] <-
                1
        }
        
        #### A shorter name
        each <- NUMBER_OF_LABELS_TO_CREATE
        dat2 <- dat[, colnames(dat) != "NUMBER_OF_LABELS_TO_CREATE"]
        
        ## Use The first column as seed
        res <- as.character(rep(dat2[, 1], each))
        for (i in 2:ncol(dat2)) {
            res <- cbind(res, as.character(rep(dat2[, i], each)))
        }
        
        colnames(res) <- colnames(dat2)
        return(res)
    }
