
getData <- function(
    data_folder = NULL,
    output_file = NULL
    ) {

    thisFunctionName <- "getData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !file.exists(data_folder) ) { return(NULL); }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(readr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output_file) ) {

        cat(paste0("\n### ",output_file," already exists; loading this file ...\n"));

        list.data_raw <- readRDS(file = output_file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

	DF.marsh <- readr::read_csv(
	    file = file.path(data_folder,"marsh_master.csv")
            );

	DF.swamp <- readr::read_csv(
	    file = file.path(data_folder,"swamp_master.csv")
            );

	DF.marsh <- as.data.frame(DF.marsh);
	DF.swamp <- as.data.frame(DF.swamp);

	DF.marsh <- DF.marsh[,setdiff(colnames(DF.marsh),"X1")];
	DF.swamp <- DF.marsh[,setdiff(colnames(DF.swamp),"X1")];

        list.data_raw <- list(
	    marsh = DF.marsh,
	    swamp = DF.swamp
	    );

        if (!is.null(output_file)) {
            saveRDS(object = list.data_raw, file = output_file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.data_raw );

    }
