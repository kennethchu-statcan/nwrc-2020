
beam.swath.diagnostics <- function(
    data.directory = NULL,
    beam.swath     = NULL
    ) {

    thisFunctionName <- "beam.swath.diagnostics";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    beam.swath.directory <- file.path(data.directory,beam.swath);

    years <- beam.swath.diagnostics_getYears(data.folder = beam.swath.directory);
    for ( temp.year in years ) {
        beam.swath.diagnostics_processYear(
            beam.swath  = beam.swath,
            year        = temp.year,
            data.folder = beam.swath.directory
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
beam.swath.diagnostics_getYears <- function(data.folder = NULL) {
    require(stringr);
    temp.files <- list.files(path = data.folder);
    years <- sort(unique(as.character(
        stringr::str_match(string = temp.files, pattern = "[0-9]{4}")
        )));
    return( years );
    }

beam.swath.diagnostics_processYear <- function(
    beam.swath  = NULL,
    year        = NULL,
    data.folder = NULL
    ) {
    list.data_raw <- getData(
        data.folder  = data.folder,
        year         = year,
        output.file  = paste0("tmp-",beam.swath,"-",year,".Rdata") 
        );
    }

