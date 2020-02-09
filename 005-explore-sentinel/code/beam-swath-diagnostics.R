
beam.swath.diagnostics <- function(
    data.directory  = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL,
    land.types      = c("ag","forest","marsh","shallow","swamp","water"),
    make.plots      = TRUE,
    make.heatmaps   = TRUE
    ) {

    thisFunctionName <- "beam.swath.diagnostics";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    beam.swath.directory <- file.path(data.directory);

    #years <- c("2017");
    years <- beam.swath.diagnostics_getYears(data.folder = beam.swath.directory);
    for ( temp.year in years ) {
        beam.swath.diagnostics_processYear(
            beam.swath      = beam.swath,
            year            = temp.year,
            data.folder     = beam.swath.directory,
            colname.pattern = colname.pattern,
            land.types      = land.types,
            make.plots      = make.plots,
            make.heatmaps   = make.heatmaps
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
    beam.swath      = NULL,
    year            = NULL,
    data.folder     = NULL,
    colname.pattern = NULL,
    land.types      = NULL,
    make.plots      = TRUE,
    make.heatmaps   = TRUE
    ) {

    cat(paste0("\nbeam.swath.diagnostics_processYear(): ",beam.swath,", ",year,"\n"));

    list.data.raw <- getData(
        data.folder  = data.folder,
        beam.swath   = beam.swath,
        year         = year,
        output.file  = paste0("data-",beam.swath,"-",year,"-raw.RData") 
        );

    list.data.reshaped <- reshapeData(
        list.input      = list.data.raw,
        beam.swath      = beam.swath,
        colname.pattern = colname.pattern,
        output.file     = paste0("data-",beam.swath,"-",year,"-reshaped.RData")
        );

    cat("\nstr(list.data.reshaped)\n");
    print( str(list.data.reshaped)   );

    if ( length(names(list.data.reshaped)) > 0 ) {

        #visualize(
        #    list.input = list.data.reshaped,
        #    beam.swath = beam.swath,
        #    year       = year
        #    );

        DF.pca <- doPCA(
            list.input      = list.data.reshaped,
            beam.swath      = beam.swath,
            year            = year,
            colname.pattern = colname.pattern,
            land.types      = land.types,
            make.plots      = make.plots,
            make.heatmaps   = make.heatmaps
            );

        cat("\nstr(DF.pca)\n");
        print( str(DF.pca)   );

	fpca.variables <- grep(x = colnames(DF.pca), pattern = colname.pattern, value = TRUE);
        for ( fpca.variable in fpca.variables ) {
            DF.fpca <- doFPCA(
                DF.input            = DF.pca,
                target.variable     = fpca.variable,
                beam.swath          = beam.swath,
                year                = year,
                spline.grid         = NULL,
                n.order             = 3,
                n.basis             = 9,
                smoothing.parameter = 0.1,
                n.harmonics         = 7
                );
            cat("\nstr(DF.fpca)\n");
            print( str(DF.fpca)   );
	    }

        }

    return( NULL );

    }

