
getData <- function(
    data.folder = NULL,
    year        = NULL,
    output.file = NULL
    ) {

    thisFunctionName <- "getData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(readr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output.file) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));

        list.data.raw <- readRDS(file = output.file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        temp.files.given.year <- list.files(path = data.folder, pattern = year);

        land.types <- unique(gsub(
            x           = temp.files.given.year,
            pattern     = "_[0-9]{4}_.+",
            replacement = ""
            ));

        list.data.raw <- list();
        for ( land.type in land.types ) {
            temp.file <- grep(x = temp.files.given.year, pattern = land.type, value = TRUE);
            DF.temp <- as.data.frame(readr::read_csv(
                file = file.path(data.folder,temp.file)
                ));
            colnames(DF.temp) <- getData_fixColnames(
                input.colnames = colnames(DF.temp)
                );
            list.data.raw[[ land.type ]] <- DF.temp;
            }

        if (!is.null(output.file)) {
            saveRDS(object = list.data.raw, file = output.file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.data.raw );

    }

##################################################
getData_fixColnames <- function(input.colnames = NULL) {

    output.colnames <- input.colnames;

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "X2",
        replacement = "X"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "X3",
        replacement = "Y"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_1_1",
        replacement = "_1"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_2_2",
        replacement = "_2"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_3_3",
        replacement = "_3"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_4_4",
        replacement = "_4"
        );

    return( output.colnames );

    }

