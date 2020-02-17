
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
    cat(paste0("\nbeam.swath: ",beam.swath,"\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    beam.swath.directory <- file.path(data.directory,beam.swath);

    DF.data <- beam.swath.diagnostics_getData(
        data.directory  = beam.swath.directory,
        beam.swath      = beam.swath,
        colname.pattern = colname.pattern,
        land.types      = land.types
        );

    cat(paste0("\nstr(DF.data) -- ",beam.swath,"\n"));
    print(        str(DF.data) );

    if ( make.plots ) {
        beam.swath.diagnostics_plotGroupedTimeSeries(
            DF.input        = DF.data,
            beam.swath      = beam.swath,
            colname.pattern = colname.pattern
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#    DF.OPC.attached <- beam.swath.diagnostics_attach.OPC(
#        DF.input   = DF.data,
#        beam.swath = beam.swath
#        );
#
#    cat("\nstr(DF.OPC.attached)\n");
#    print( str(DF.OPC.attached)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#    DF.standardizedTimepoints <- beam.swath.diagnostics_getDataStandardizedTimepoints(
#        DF.input   = DF.data,
#        beam.swath = beam.swath
#        );
#
#    cat("\nstr(DF.standardizedTimepoints)\n");
#    print( str(DF.standardizedTimepoints)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
beam.swath.diagnostics_plotGroupedTimeSeries <- function(
    DF.input        = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL
    ) {

    require(ggplot2);

    cat("\nstr(DF.input)\n");
    print( str(DF.input)   );

    years            <- unique(DF.input[,"year"]);
    target.variables <- grep(x = colnames(DF.input), pattern = colname.pattern, value = TRUE);

    for ( year            in years            ) {
    for ( target.variable in target.variables ) {

        PNG.output <- paste0('tmp-timeseries-',beam.swath,'-',year,'-',target.variable,'.png');

	is.current.year   <- (DF.input[,"year"] == year);
        DF.temp           <- DF.input[is.current.year,c("X_Y_year","date","type",target.variable)];
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = target.variable,
            replacement = "target.variable"
            );

        my.ggplot <- initializePlot(
            title    = NULL,
            subtitle = paste0(beam.swath,", ",year,", ",target.variable)
            );

        my.ggplot <- my.ggplot + scale_x_date(
            breaks       = sort(unique(DF.temp[,"date"])),
            minor_breaks = NULL
            );

        my.ggplot <- my.ggplot + theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5)
            );

        if ( grepl(x = target.variable, pattern = "_scaled$") ) {
            my.ggplot <- my.ggplot + scale_y_continuous(
                #limits = c(  -0.3,1.6),
                #breaks = seq(-0.2,1.6,0.2)
                limits = c(  -3.0,3.0),
                breaks = seq(-3.0,3.0,1.0)
                );
            }

        my.ggplot <- my.ggplot + geom_line(
            data    = DF.temp,
            mapping = aes(x=date,y=target.variable,group=X_Y_year,color=type),
            alpha   = 0.3
            );

        my.ggplot <- my.ggplot + facet_grid(type ~ .);

        ggsave(
            file   = PNG.output,
            plot   = my.ggplot,
            dpi    = 150,
            height =  16,
            width  =  16,
            units  = 'in'
            );

        }}

    return( NULL );

    }

beam.swath.diagnostics_getDataStandardizedTimepoints <- function(
    DF.input     = NULL,
    beam.swath   = NULL,
    RData.output = paste0("data-",beam.swath,"-standardizedTimepoints.RData")
    ) {

    if ( file.exists(RData.output) ) {
        DF.output <- readRDS(file = RData.output);    
        return( DF.output );
        } 

    DF.data <- data.frame();

    years <- beam.swath.diagnostics_getYears(data.directory = data.directory);
    for ( temp.year in years ) {

        list.data.raw <- getData(
            data.directory = data.directory,
            beam.swath     = beam.swath,
            year           = temp.year,
            output.file    = file.path(temp.directory,paste0("data-",beam.swath,"-",temp.year,"-raw.RData"))
            );

        DF.data.reshaped <- reshapeData(
            list.input      = list.data.raw,
            beam.swath      = beam.swath,
            colname.pattern = colname.pattern,
            output.file     = file.path(temp.directory,paste0("data-",beam.swath,"-",temp.year,"-reshaped.RData"))
            );

	DF.data <- rbind(DF.data,DF.data.reshaped);

        }

    return( DF.data );

    }

beam.swath.diagnostics_getData <- function(
    data.directory  = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL,
    land.types      = NULL,
    RData.output    = paste0("data-",beam.swath,".RData")
    ) {

    if ( file.exists(RData.output) ) {
        DF.output <- readRDS(file = RData.output);    
        return( DF.output );
        } 

    DF.output <- data.frame();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory <- getwd();
    temp.directory    <- file.path(initial.directory,"data");
    if ( !dir.exists(temp.directory) ) {
        dir.create(path = temp.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years <- beam.swath.diagnostics_getYears(data.directory = data.directory);
    for ( temp.year in years ) {

        list.data <- getData(
            data.directory = data.directory,
            beam.swath     = beam.swath,
            year           = temp.year,
            output.file    = file.path(temp.directory,paste0("data-",beam.swath,"-",temp.year,"-raw.RData"))
            );

        #cat(paste0("\nstr(list.data) -- ",temp.year,"\n"));
	#print(        str(list.data) );

        DF.data.reshaped <- reshapeData(
            list.input      = list.data,
            beam.swath      = beam.swath,
            colname.pattern = colname.pattern,
	    land.types      = land.types,
            output.file     = file.path(temp.directory,paste0("data-",beam.swath,"-",temp.year,"-reshaped.RData"))
            );

        cat(paste0("\nstr(DF.data.reshaped) -- ",beam.swath,", ",temp.year,"\n"));
	print(        str(DF.data.reshaped) );

	DF.output <- rbind(DF.output,DF.data.reshaped);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (!is.null(RData.output)) {
        saveRDS(object = DF.output, file = RData.output);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    remove(list = c("list.data.raw"));
    return( DF.output );

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

