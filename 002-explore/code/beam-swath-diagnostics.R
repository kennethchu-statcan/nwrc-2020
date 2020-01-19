
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

    list.data.raw <- getData(
        data.folder  = data.folder,
        year         = year,
        output.file  = paste0("data-",beam.swath,"-",year,"-raw.Rdata") 
        );

    list.data.reshaped <- reshapeData(
        list.input  = list.data.raw,
        beam.swath  = beam.swath,
        output.file = paste0("data-",beam.swath,"-",year,"-reshaped.RData")
        );

    cat("\nstr(list.data.reshaped)\n");
    print( str(list.data.reshaped)   );

    return( NULL );

    visualize(
        list.input = list.data.reshaped
        );

    DF.pca <- doPCA(
        list_input = list.data.reshaped,
        make_plots = TRUE
        );

    cat("\nstr(DF.pca)\n");
    print( str(DF.pca)   );

    DF.fpca <- doFPCA(
        DF.input            = DF.pca,
        target.variable     = "scaled_Comp.1",
        spline.grid         = NULL,
        n.order             = 3,
        n.basis             = 9,
        smoothing.parameter = 0.1,
        n.harmonics         = 7,
        FILE.output.RData   = "tmp-FPCA-scaled-Comp1.RData",
        FILE.output.csv     = "tmp-FPCA-scaled-Comp1.csv"
        );

    cat("\nstr(DF.fpca)\n");
    print( str(DF.fpca)   );

    return( NULL );


    }

