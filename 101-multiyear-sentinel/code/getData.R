
getData <- function(
    data.directory     = NULL,
    beam.swath         = NULL,
    year               = NULL,
    exclude.land.types = NULL,
    output.file        = NULL
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

        if ( !is.null(exclude.land.types) ) {
            land.types <- setdiff( land.types, exclude.land.types ); 
            }

        list.data.raw <- list();
        for ( land.type in land.types ) {
            temp.file <- grep(x = temp.files.given.year, pattern = land.type, value = TRUE);
            DF.temp <- as.data.frame(readr::read_csv(
                file = file.path(data.directory,temp.file)
                ));
            colnames(DF.temp) <- getData_fixColnames( input.colnames = colnames(DF.temp) );

	    retained.colnames <- grep(x = colnames(DF.temp), pattern = "Unnamed", ignore.case = TRUE, value = TRUE, invert = TRUE);
	    DF.temp <- DF.temp[,retained.colnames];

	    # The following is a special patch for the 2020-02-24.02 data snapshot.
	    # It turns out that the agriculture data in this snapshot does NOT have
	    # the timepoint 2017-09-02, while data for the other wetland types do
	    # have that timepoint. If unmitigated, this would in all agricultural
	    # lands in 2017 being dropped from the analysis.
	    # The following patch is to add this timepoint for the 2017 agricultural
	    # lands, and the value of the tree variables at the added timepoint is simply
	    # the mean of the preceding and following timepoints.
            if ( beam.swath == "IW106" & land.type == "ag" & year == "2017" ) {
                DF.temp[,"S1_IW106_20170902_VH"   ] <- (DF.temp[,"S1_IW106_20170821_VH"   ] + DF.temp[,"S1_IW106_20170914_VH"   ])/2;
                DF.temp[,"S1_IW106_20170902_VV"   ] <- (DF.temp[,"S1_IW106_20170821_VV"   ] + DF.temp[,"S1_IW106_20170914_VV"   ])/2;
                DF.temp[,"S1_IW106_20170902_angle"] <- (DF.temp[,"S1_IW106_20170821_angle"] + DF.temp[,"S1_IW106_20170914_angle"])/2;
                }

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

    return( output.colnames );

    }

