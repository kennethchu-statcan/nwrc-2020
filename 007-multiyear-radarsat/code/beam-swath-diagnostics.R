
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
    initial.directory     <- getwd();
    temp.output.directory <- file.path(initial.directory,colname.pattern);
    if ( !dir.exists(temp.output.directory) ) {
        dir.create(path = temp.output.directory, recursive = TRUE);
        }
    setwd( temp.output.directory );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    stdout.connection <- file(file.path(temp.output.directory,"stdout.R.beam-swath-diagnostics"), open = "wt");
    stderr.connection <- file(file.path(temp.output.directory,"stderr.R.beam-swath-diagnostics"), open = "wt");

    sink(file = stdout.connection, type = "output" );
    sink(file = stderr.connection, type = "message");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    beam.swath.directory <- file.path(data.directory,beam.swath);

    DF.data.standardized.timepoints <- beam.swath.diagnostics_getDataStandardizedTimepoints(
        data.directory  = beam.swath.directory,
        beam.swath      = beam.swath,
        colname.pattern = colname.pattern
        );

    cat("\nstr(DF.data.standardized.timepoints)\n");
    print( str(DF.data.standardized.timepoints)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nwarnings()\n");
    print( warnings()   );

    cat("\ngetOption('repos')\n");
    print( getOption('repos')   );

    cat("\n.libPaths()\n");
    print( .libPaths()   );

    cat("\nsessionInfo\n");
    print( sessionInfo() );

    sink(file = NULL);
    sink(file = NULL);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd( initial.directory );
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
beam.swath.diagnostics_getDataStandardizedTimepoints <- function(
    data.directory  = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL
    ) {

    years <- beam.swath.diagnostics_getYears(data.directory = data.directory);
    for ( temp.year in years ) {

        list.data.raw <- getData(
            data.directory = data.directory,
            beam.swath     = beam.swath,
            year           = temp.year,
            output.file    = paste0("data-",beam.swath,"-",temp.year,"-raw.RData") 
            );

        list.data.reshaped <- reshapeData(
            list.input      = list.data.raw,
            beam.swath      = beam.swath,
            colname.pattern = colname.pattern,
            output.file     = paste0("data-",beam.swath,"-",temp.year,"-reshaped.RData")
            );

        cat("\nstr(list.data.reshaped)\n");
        print( str(list.data.reshaped)   );

        }

    return( NULL );

    }

beam.swath.diagnostics_getYears <- function(data.directory = NULL) {
    require(stringr);
    temp.files <- list.files(path = data.directory);
    years <- sort(unique(as.character(
        stringr::str_match(string = temp.files, pattern = "[0-9]{4}")
        )));
    return( years );
    }

beam.swath.diagnostics_processYear <- function(
    beam.swath      = NULL,
    year            = NULL,
    data.directory  = NULL,
    colname.pattern = NULL,
    land.types      = NULL,
    make.plots      = TRUE,
    make.heatmaps   = TRUE
    ) {

    cat(paste0("\nbeam.swath.diagnostics_processYear(): ",beam.swath,", ",year,"\n"));

    list.data.raw <- getData(
        data.directory = data.directory,
        beam.swath     = beam.swath,
        year           = year,
        output.file    = paste0("data-",beam.swath,"-",year,"-raw.RData") 
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

