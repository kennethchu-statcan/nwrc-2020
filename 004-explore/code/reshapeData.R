
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

    print("A-1");
    cat("\nstr(DF.input)\n");
    print( str(DF.input)   );

    colnames(DF.input) <- gsub(
	x           = colnames(DF.input),
        pattern     = "X2",
	replacement = "X"
        );

    colnames(DF.input) <- gsub(
	x           = colnames(DF.input),
        pattern     = "X3",
	replacement = "Y"
        );

    temp.colnames <- grep(
        x       = colnames(DF.input),
        pattern = colname.pattern,
        value   = TRUE
        );

    print("A-2");
    cat("\ntemp.colnames\n");
    print( temp.colnames   );

    DF.temp <- DF.input[,c("X","Y",temp.colnames)];

    print("A-3");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.temp <- DF.temp %>%
        tidyr::gather(column.name,value,-X,-Y);

    print("A-4");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.temp <- as.data.frame(DF.temp);

    print("A-5");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.temp[,"date"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
	pattern = "[0-9]{8}"
	);

    print("A-6");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.temp[,"variable"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
	pattern = paste0(colname.pattern,".*")
	);

    print("A-7");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.temp <- DF.temp[,c("X","Y","date","variable","value")];

    print("A-8");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.output <- DF.temp %>%
        dplyr::select(X,Y,date,variable,value) %>%
        tidyr::spread(key=variable,value=value);

    print("A-9");
    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );
    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );
    cat("\nhead(DF.temp)\n");
    print( head(DF.temp)   );

    DF.output[,"date"] <- as.Date(x = DF.output[,"date"], tryFormats = c("%Y%m%d"));

    print("A-10");

    return(DF.output);

    }

