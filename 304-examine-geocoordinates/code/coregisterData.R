
coregisterData <- function(
    DF.input    = NULL,
    output.file = NULL
    ) {

    thisFunctionName <- "coregisterData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ifelse(is.null(output.file),FALSE,file.exists(output.file)) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));
        DF.output <- readRDS(file = output.file);
        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        DF.master.lat.lon <- coregisterData_master.lat.lon(
            DF.input = DF.input
            );

        DF.master.lat.lon <- DF.master.lat.lon[,c('row_index','col_index','master_lat','master_lon')];

        DF.output <- dplyr::left_join(
            x  = DF.input,
            y  = DF.master.lat.lon,
            by = c('row_index','col_index')
            );

        if (!is.null(output.file)) {
            saveRDS(object = DF.output, file = output.file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
coregisterData_master.lat.lon <- function(
    DF.input = NULL
    ) {
    DF.output <- DF.input[DF.input[,'date'] == min(DF.input[,'date']),c('date','row_index','col_index','lat','lon')];
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "lat",
        replacement = "master_lat"
        );
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "lon",
        replacement = "master_lon"
        );
    return( DF.output );
    }
