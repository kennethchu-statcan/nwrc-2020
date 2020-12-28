
getData <- function(
    data.directory = NULL,
    output.file    = NULL
    ) {

    thisFunctionName <- "getData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(readr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ifelse(is.null(output.file),FALSE,file.exists(output.file)) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));
        DF.output <- readRDS(file = output.file);
        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        temp.files <- list.files(path = data.directory);

        DF.metadata <- getData_metadata(
            data.files = temp.files
            );

        DF.output <- NULL;
        for ( temp.string in unique(as.character(DF.metadata[,'date'])) ) {
            DF.given.date <- getData_given.date(
                given.date     = as.Date(temp.string),
                DF.metadata    = DF.metadata,
                data.directory = data.directory
                );
            if ( is.null(DF.output) ) {
                DF.output <- DF.given.date;
            } else {
                DF.output <- rbind(DF.output,DF.given.date);
                }
            }

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
getData_given.date <- function(
    given.date     = NULL,
    DF.metadata    = NULL,
    data.directory = NULL
    ) {
    DF.metadata <- DF.metadata[DF.metadata[,'date'] == given.date,];
    DF.output <- NULL;
    for ( i in seq(1,nrow(DF.metadata))) {
        DF.temp   <- read.csv(file = file.path(data.directory,DF.metadata[i,'data.file']), header = FALSE);
        if ( is.null(DF.output) ) {
            DF.output <- data.frame(temp_colname = as.vector(as.matrix(DF.temp)));
        } else {
            DF.output <- cbind(DF.output, temp_colname = as.vector(as.matrix(DF.temp)));
            }
        DF.output <- as.data.frame(DF.output);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = 'temp_colname', replacement = DF.metadata[i,'variable']);
        }
    DF.output[,'date'] <- given.date;
    DF.output <- DF.output[,c('date',setdiff(colnames(DF.output),'date'))];
    return( DF.output );
    }

getData_metadata <- function(
    data.files = NULL
    ) {

    DF.output <- as.data.frame(t(as.data.frame(strsplit(
        x     = gsub(x = data.files, pattern = "^_", replacement = ""),
        split = "(_|\\.)"
        ))));
    rownames(DF.output) <- NULL;
    colnames(DF.output) <- c('date','beam.mode','variable','file.format');

    DF.output[,'date']      <- as.Date(x = DF.output[,'date'], format = "%Y%m%d");
    DF.output[,'variable']  <- tolower(x = DF.output[,'variable']);
    DF.output[,'data.file'] <- data.files;

    return( DF.output );
    }
