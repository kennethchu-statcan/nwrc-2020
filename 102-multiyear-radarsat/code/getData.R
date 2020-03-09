
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
            colnames(DF.temp) <- getData_fixColnames( input.colnames = colnames(DF.temp) );

            retained.colnames <- grep(x = colnames(DF.temp), pattern = "Unnamed", ignore.case = TRUE, value = TRUE, invert = TRUE);
            DF.temp <- DF.temp[,retained.colnames];

            DF.temp <- getData_add.dBZ.variables(DF.input = DF.temp);

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
getData_add.dBZ.variables <- function(DF.input = NULL) {
    DF.output <- DF.input;
    temp.colnames <- grep(x = colnames(DF.output), pattern = "cov_matrix_real_comp", value = TRUE);
    for ( temp.colname in temp.colnames ) {
        new.colname <- paste0(temp.colname,"_dBZ");
        DF.output[,new.colname] <- 10 * log10(DF.output[,temp.colname]);
        }
    return(DF.output);
    }

getData_fixColnames <- function(input.colnames = NULL) {

    output.colnames <- input.colnames;

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "POINT_X",
        replacement = "X"
        );

    output.colnames <- gsub(
        x           = output.colnames,
        pattern     = "POINT_Y",
        replacement = "Y"
        );

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

    return( output.colnames );

    }

