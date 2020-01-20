
reshapeData <- function(
    list.input      = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL,
    output.file     = NULL
    ) {

    thisFunctionName <- "reshapeData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output.file) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));

        list.output <- readRDS(file = output.file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        list.output <- list();
        for ( temp.name in names(list.input) ) {
            list.output[[ temp.name ]] <- reshapeData_long(
                DF.input        = list.input[[ temp.name ]],
                beam.swath      = beam.swath,
                colname.pattern = colname.pattern
                );
	        }

        if (!is.null(output.file)) {
            saveRDS(object = list.output, file = output.file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

###################################################
reshapeData_long <- function(
    DF.input        = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL
    ) {

    require(dplyr);
    require(tidyr);

    temp.colnames <- grep(
        x       = colnames(DF.input),
        pattern = colname.pattern,
        value   = TRUE
        );

    DF.temp <- DF.input[,c("X","Y",temp.colnames)];

    colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = paste0(beam.swath,"_"), replacement = "z_");

    DF.temp <- DF.temp %>%
        tidyr::gather(variable,value,-X,-Y);

    DF.temp <- as.data.frame(DF.temp);
    colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = "z_", replacement = "");

    DF.temp[,"variable"] <- gsub(
        x           = DF.temp[,"variable"],
	    pattern     = "z_",
  	    replacement = ""
        );

    DF.temp[,"date"] <- gsub(
        x           = DF.temp[,"variable"],
        pattern     = "_.+",
        replacement = ""
        );

    DF.temp[,"band"] <- gsub(
        x           = DF.temp[,"variable"],
        pattern     = "[0-9]{8}_",
        replacement = ""
        );

    DF.temp <- DF.temp[,c("X","Y","date","band","value")];

    DF.output <- DF.temp %>%
        dplyr::select(X,Y,date,band,value) %>%
        tidyr::spread(key=band,value=value);

    DF.output[,"date"] <- as.Date(x = DF.output[,"date"], tryFormats = c("%Y%m%d"));

    return(DF.output);

    }

