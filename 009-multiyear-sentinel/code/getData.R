
getData <- function(
    data.directory = NULL,
    beam.swath     = NULL,
    year           = NULL,
    output.file    = NULL
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

        temp.files.given.year <- list.files(path = data.directory, pattern = year);
        temp.files.given.year <- grep(
            x       = temp.files.given.year,
            pattern = beam.swath,
            value   = TRUE
            );

        land.types <- unique(gsub(
            x           = temp.files.given.year,
            pattern     = "_[0-9]{4}_.+",
            replacement = ""
            ));

        list.data.raw <- list();
        for ( land.type in land.types ) {
            temp.file <- grep(x = temp.files.given.year, pattern = land.type, value = TRUE);
            DF.temp <- as.data.frame(readr::read_csv(
                file = file.path(data.directory,temp.file)
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

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "Yamaguchi_double_bounc$",
        replacement = "Yamaguchi_double_bounce"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_re$",
        replacement = "_real"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_im$",
        replacement = "_imag"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "S_par$",
        replacement = "S_param"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "Helicit$",
        replacement = "Helicity"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "TouzDisc_diff_maxmin_r$",
        replacement = "TouzDisc_diff_maxmin_res"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "_pol_respo$",
        replacement = "_pol_respons"
        );

    # standardize order of components in column names
    output.colnames <- as.character(sapply(
        X   = output.colnames,
        FUN = function(x) {
            y <- unlist(strsplit(x = x, "_"));
            y <- c(y[1],paste0(y[2],y[5]),paste0(y[3],y[4]),y[6]);
            y <- paste(y,collapse = "_");
            return(y)
            }
	));

    return( output.colnames );

    }

