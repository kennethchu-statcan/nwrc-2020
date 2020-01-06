
reshapeData <- function(
    list_input  = NULL,
    output_file = NULL
    ) {

    thisFunctionName <- "reshapeData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output_file) ) {

        cat(paste0("\n### ",output_file," already exists; loading this file ...\n"));

        list.output <- readRDS(file = output_file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

	list.output <- list();
	for ( temp.name in names(list_input) ) {
	    list.output[[ temp.name ]] <- reshapeData_long(
	        DF.input = list_input[[ temp.name ]]
                );
	    }

        if (!is.null(output_file)) {
            saveRDS(object = list.output, file = output_file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

###################################################
reshapeData_long <- function(DF.input = NULL) {

    require(tidyr);

    colnames(DF.input) <- paste0("z_",colnames(DF.input));
    DF.temp <- DF.input %>%
        tidyr::gather(variable,value,z_20190413_Band_1:z_20191115_Band_3);

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
	pattern     = "[0-9]+_",
	replacement = ""
	);

    DF.temp <- DF.temp[,c("X","Y","date","band","value")];

    DF.output <- DF.temp %>%
        dplyr::select(X,Y,date,band,value) %>%
	tidyr::spread(key=band,value=value);

    return(DF.output);

    }

