
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

        cat("\ntemp.files\n");
        print( temp.files   );

        DF.metadata <- getData_metadata(
            data.files = temp.files
            );

        cat("\nstr(DF.metadata)\n");
        print( str(DF.metadata)   );

        cat("\nDF.metadata\n");
        print( DF.metadata   );

        cat("\nunique(DF.metadata[,'date'])\n");
        print( unique(DF.metadata[,'date'])   );

        cat("\nunique(DF.metadata[,'date'])[1:2]\n");
        print( unique(DF.metadata[,'date'])[1:2]   );

        DF.output <- NULL;
        for ( temp.string in unique(as.character(DF.metadata[,'date'])) ) {
            cat("\ntemp.string\n");
            print( temp.string   );
            DF.given.date <- getData_given.date(
                given.date     = as.Date(temp.string),
                DF.metadata    = DF.metadata,
                data.directory = data.directory
                );
            cat("\nstr(DF.given.date)\n");
            print( str(DF.given.date)   );
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
    cat("\ngiven.date\n");
    print( given.date   );
    DF.metadata <- DF.metadata[DF.metadata[,'date'] == given.date,];
    DF.output <- NULL;
    for ( i in seq(1,nrow(DF.metadata))) {
        cat("\ndata.file\n");
        print( file.path(data.directory,DF.metadata[i,'data.file']) );
        DF.temp   <- read.csv(file = file.path(data.directory,DF.metadata[i,'data.file']), header = FALSE);
        cat("\nis.null(DF.output)\n");
        print( is.null(DF.output)   );
        if ( is.null(DF.output) ) {
            DF.output <- data.frame(temp_colname = as.vector(as.matrix(DF.temp)));
        } else {
            DF.output <- cbind(DF.output, temp_colname = as.vector(as.matrix(DF.temp)));
            }
        DF.output <- as.data.frame(DF.output);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = 'temp_colname', replacement = DF.metadata[i,'variable']);
        cat("\nstr(DF.output)\n");
        print( str(DF.output)   );
        }
    cat("\ngiven.date\n");
    print( given.date   );
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

getData_add.dBZ.variables <- function(DF.input = NULL) {
    DF.output <- DF.input;
    temp.colnames <- grep(x = colnames(DF.output), pattern = "cov_matrix_real_comp", value = TRUE);
    for ( temp.colname in temp.colnames ) {
        new.colname <- paste0(temp.colname,"_dBZ");
        DF.output[,new.colname] <- 10 * log10(DF.output[,temp.colname]);
        }
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "cov_matrix_real_comp_1_dBZ",
        replacement = "dBZ_cov_comp_1"
        );
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "cov_matrix_real_comp_2_dBZ",
        replacement = "dBZ_cov_comp_2"
        );
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "cov_matrix_real_comp_3_dBZ",
        replacement = "dBZ_cov_comp_3"
        );
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "cov_matrix_real_comp_4_dBZ",
        replacement = "dBZ_cov_comp_4"
        );
    return(DF.output);
    }

getData_fixColnames_Sentinel <- function(input.colnames = NULL) {

    thisFunctionName <- "getData_fixColnames_Sentinel";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

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

    # standardize order of components in column names
    output.colnames <- as.character(sapply(
        X   = output.colnames,
        FUN = function(x) {
            y <- unlist(strsplit(x = x, split = "_"));
            if ( length(y) > 1 ) {
                y <- c(y[1],paste0(y[2],y[5]),paste0(y[3],y[4]),y[6]);
                y <- paste(y,collapse = "_");
                }
            return(y);
            }
        ));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.colnames );

    }

getData_fixColnames_RADARSAT <- function(input.colnames = NULL) {

    thisFunctionName <- "getData_fixColnames_RADARSAT";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.colnames );

    }
