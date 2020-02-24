
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
		land.type       = temp.name,
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
    land.type       = NULL,
    colname.pattern = NULL
    ) {

    require(dplyr);
    require(tidyr);

    cat(paste0("\nreshapeData_long(): ",beam.swath,", ",land.type,"\n"));

    temp.colnames <- grep(
        x       = colnames(DF.input),
        pattern = colname.pattern,
        value   = TRUE
        );

    DF.temp <- DF.input[,c("X","Y",temp.colnames)];
#   DF.temp       <- DF.input[,temp.colnames];
#   DF.temp[,"X"] <- paste0(land.type,"_",seq(1,nrow(DF.temp)));
#   DF.temp[,"Y"] <- paste0(land.type,"_",seq(1,nrow(DF.temp)));

    DF.temp <- DF.temp %>%
        tidyr::gather(column.name,value,-X,-Y);
    DF.temp <- as.data.frame(DF.temp);

    DF.temp[,"date"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
        pattern = "[0-9]{8}"
        );

    DF.temp[,"variable"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
        pattern = paste0(colname.pattern,".*")
        );

    DF.temp <- DF.temp[,c("X","Y","date","variable","value")];

    DF.output <- DF.temp %>%
        dplyr::select(X,Y,date,variable,value) %>%
        tidyr::spread(key=variable,value=value);

    DF.output[,"date"] <- as.Date(x = DF.output[,"date"], tryFormats = c("%Y%m%d"));

    temp.colnames <- grep(
        x       = colnames(DF.output),
        pattern = colname.pattern,
        value   = TRUE
        );

    for ( temp.colname in temp.colnames ) {
        DF.output[,temp.colname] <- as.numeric(DF.output[,temp.colname]); 
        }

    return(DF.output);

    }

