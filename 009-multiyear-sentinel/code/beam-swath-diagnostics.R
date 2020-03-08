
beam.swath.diagnostics <- function(
    data.directory      = NULL,
    beam.swath          = NULL,
    colname.pattern     = NULL,
    exclude.years       = NULL,
    land.types          = c("ag","forest","marsh","shallow","swamp","water"),
    n.partition         = 20,
    n.order             =  3,
    n.basis             =  9,
    smoothing.parameter =  0.1,
    n.harmonics         =  7,
    plot.timeseries     = TRUE,
    plot.heatmaps       = TRUE
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
        land.types      = land.types,
        exclude.years   = exclude.years
        );

    cat(paste0("\nstr(DF.data) -- ",beam.swath,"\n"));
    print(        str(DF.data) );

    cat(paste0("\nsummary(DF.data) -- ",beam.swath,"\n"));
    print(        summary(DF.data) );

    if ( plot.timeseries ) {
        beam.swath.diagnostics_plotGroupedTimeSeries(
            DF.input        = DF.data,
            beam.swath      = beam.swath,
            colname.pattern = colname.pattern
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.standardizedTimepoints <- getDataStandardizedTimepoints(
        DF.input            = DF.data,
        beam.swath          = beam.swath,
        colname.pattern     = colname.pattern,
        n.partition         = n.partition,
        n.order             = n.order,
        n.basis             = n.basis,
        smoothing.parameter = smoothing.parameter,
        n.harmonics         = n.harmonics
        );

    cat("\nstr(LIST.standardizedTimepoints)\n");
    print( str(LIST.standardizedTimepoints)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    fpca.variables <- grep(
        x       = colnames(LIST.standardizedTimepoints[["df_standardized_timepoints"]]),
        pattern = colname.pattern,
        value   = TRUE
        );

    for ( fpca.variable in fpca.variables ) {

        LIST.fpca <- doFPCA(
            DF.input            = LIST.standardizedTimepoints[["df_standardized_timepoints"]],
            target.variable     = fpca.variable,
            beam.swath          = beam.swath,
            spline.grid         = NULL,
            n.order             = 3,
            n.basis             = 9,
            smoothing.parameter = 0.1,
            n.harmonics         = 7
            );

        cat("\nstr(LIST.fpca)\n");
        print( str(LIST.fpca)   );

        beam.swath.diagnostics_FPCA.harmonics(
            beam.swath                   = beam.swath,
            fpca.variable                = fpca.variable,
            DF.data                      = DF.data,
            LIST.standardized_timepoints = LIST.standardizedTimepoints,
            LIST.fpca                    = LIST.fpca,
            n.harmonics                  = n.harmonics
            );

        beam.swath.diagnostics_FPCA.fit(
            beam.swath                   = beam.swath,
            fpca.variable                = fpca.variable,
            DF.data                      = DF.data,
            LIST.standardized_timepoints = LIST.standardizedTimepoints,
            LIST.fpca                    = LIST.fpca
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
beam.swath.diagnostics_FPCA.harmonics <- function(
    beam.swath                   = NULL,
    fpca.variable                = NULL,
    DF.data                      = NULL,
    LIST.standardized_timepoints = NULL,
    LIST.fpca                    = NULL,
    n.harmonics                  = NULL
    ) {

    require(ggplot2);
    require(cowplot);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory     <- getwd();
    temp.output.directory <- file.path(initial.directory,"diagnostics-fpca-harmonics");
    if ( !dir.exists(temp.output.directory) ) {
        dir.create(path = temp.output.directory, recursive = TRUE);
        }
    setwd( temp.output.directory );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.evalarg <- seq(min(LIST.fpca[["spline_grid"]]),max(LIST.fpca[["spline_grid"]]),0.01);

    vector.meanfd <- fda::eval.fd(
        evalarg = temp.evalarg,
        fdobj   = LIST.fpca[["target_variable_fpc"]][["meanfd"]]
        );

    DF.fpca.standardizedTimepoints <- fda::eval.fd(
        evalarg = temp.evalarg,
        fdobj   = LIST.fpca[["target_variable_fpc"]][["harmonics"]]
        );

    DF.fpca.harmonics.plus  <- DF.fpca.standardizedTimepoints %*% diag(sqrt( LIST.fpca[["target_variable_fpc"]][["values"]][1:n.harmonics] ));
    DF.fpca.harmonics.minus <- DF.fpca.harmonics.plus;
    for ( j in seq(1,ncol(DF.fpca.harmonics.plus)) ) {
        DF.fpca.harmonics.plus[,j]  <- vector.meanfd + DF.fpca.harmonics.plus[, j];
        DF.fpca.harmonics.minus[,j] <- vector.meanfd - DF.fpca.harmonics.minus[,j];
        }
    colnames(DF.fpca.harmonics.plus ) <- paste0("harmonic",1:ncol(DF.fpca.harmonics.plus ));
    colnames(DF.fpca.harmonics.minus) <- paste0("harmonic",1:ncol(DF.fpca.harmonics.minus));
    DF.fpca.harmonics.plus  <- cbind("date_index" = temp.evalarg, DF.fpca.harmonics.plus );
    DF.fpca.harmonics.minus <- cbind("date_index" = temp.evalarg, DF.fpca.harmonics.minus);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    PNG.output <- paste0('fpca-harmonics-',beam.swath,'-',fpca.variable,'.png');

    my.cowplot <- my.ggplot <- ggplot(data = NULL) + theme_bw();

    list.plots <- list();
    temp.harmonics <- setdiff(colnames(DF.fpca.harmonics.plus),"date_index");
    #for ( temp.harmonic in setdiff(colnames(DF.fpca.harmonics.plus),"date_index") ) {
    for ( temp.index in seq(1,length(temp.harmonics)) ) {

        temp.harmonic <- temp.harmonics[temp.index];

        temp.varprop <- LIST.fpca[["target_variable_fpc"]][["varprop"]][temp.index];
        temp.varprop <- round(x = 100 * temp.varprop, digits = 3);

        my.ggplot <- initializePlot(
            title    = NULL,
            subtitle = paste0(beam.swath,", ",fpca.variable, " (variability captured = ",temp.varprop,"%)")
            );

        temp.xlab <- ifelse(temp.index == length(temp.harmonics),"date index","");
        temp.ylab <- gsub(x = temp.harmonic, pattern = "harmonic", replacement = "FPC ");

        my.ggplot <- my.ggplot + ggplot2::xlab( label = temp.xlab );
        my.ggplot <- my.ggplot + ggplot2::ylab( label = temp.ylab );
        my.ggplot <- my.ggplot + scale_x_continuous(limits=c(75,325),breaks=seq(100,300,50));

        DF.temp <- as.data.frame(DF.fpca.harmonics.plus[,c("date_index",temp.harmonic)]);
        colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = temp.harmonic, replacement = "dummy.colname");
        my.ggplot <- my.ggplot + ggplot2::geom_line(
            data     = DF.temp,
            mapping  = aes(x = date_index, y = dummy.colname),
            colour   = "#FF9700",
            linetype = "solid",
            size     = 2.0,
            alpha    = 0.8
            );

        DF.temp <- data.frame("date_index" = temp.evalarg, "dummy.colname" = vector.meanfd);
        colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = "mean", replacement = "dummy.colname");
        my.ggplot <- my.ggplot + ggplot2::geom_line(
            data     = DF.temp,
            mapping  = aes(x = date_index, y = dummy.colname),
            colour   = "gray",
            linetype = "solid",
            size     = 1.0,
            alpha    = 0.8
            );

        DF.temp <- as.data.frame(DF.fpca.harmonics.minus[,c("date_index",temp.harmonic)]);
        colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = temp.harmonic, replacement = "dummy.colname");
        my.ggplot <- my.ggplot + ggplot2::geom_line(
            data     = DF.temp,
            mapping  = aes(x = date_index, y = dummy.colname),
            colour   = "#0068FF",
            linetype = "solid",
            size     = 2.0,
            alpha    = 0.8
            );

        list.plots[[temp.harmonic]] <- my.ggplot;

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        plotlist = list.plots,
        ncol     = 1,
        align    = "v",
        axis     = "lr"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ggplot2::ggsave(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 150,
        height = 4 * n.harmonics,
        width  =  16,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd( initial.directory );
    return( NULL );

    }

beam.swath.diagnostics_FPCA.fit <- function(
    beam.swath                   = NULL,
    fpca.variable                = NULL,
    DF.data                      = NULL,
    LIST.standardized_timepoints = NULL,
    LIST.fpca                    = NULL
    ) {

    require(fda);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory     <- getwd();
    temp.output.directory <- file.path(initial.directory,"diagnostics-fpca-complete");
    if ( !dir.exists(temp.output.directory) ) {
        dir.create(path = temp.output.directory, recursive = TRUE);
        }
    setwd( temp.output.directory );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years <- unique(DF.data[,"year"]);
    types <- unique(DF.data[,"type"]);

    for ( temp.year in years ) {
    for ( temp.type in types ) {

        cat(paste0("\n# year: ",temp.year,", type: ",temp.type,"\n"));

        is.selected   <- (DF.data[,"year"] == temp.year) & (DF.data[,"type"] == temp.type);
        DF.year.type  <- DF.data[is.selected,c("X_Y_year","date_index",fpca.variable)];
        temp.XY.years <- sample(x = unique(DF.year.type[,"X_Y_year"]), size = 2, replace = FALSE);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.evalarg <- seq(min(DF.year.type[,"date_index"]),max(DF.year.type[,"date_index"]),0.1);
        DF.bsplines.original <- fda::eval.fd(
            evalarg = temp.evalarg,
            fdobj   = LIST.standardized_timepoints[["list_bsplines"]][[fpca.variable]][[temp.year]][["target_in_basis_fd"]][["fd"]]
	    );
        DF.bsplines.original <- DF.bsplines.original[,temp.XY.years];
        DF.bsplines.original <- cbind("date_index" = temp.evalarg, DF.bsplines.original);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.evalarg <- seq(min(LIST.fpca[["spline_grid"]]),max(LIST.fpca[["spline_grid"]]),0.1);

        vector.meanfd <- fda::eval.fd(
            evalarg = temp.evalarg,
            fdobj   = LIST.fpca[["target_variable_fpc"]][["meanfd"]]
            );

        DF.fpca.standardizedTimepoints <- fda::eval.fd(
            evalarg = temp.evalarg,
            fdobj   = LIST.fpca[["target_variable_fpc"]][["harmonics"]]
            );

        DF.fpca.fit <- DF.fpca.standardizedTimepoints %*% t( LIST.fpca[["target_variable_fpc"]][["scores"]] );
        for ( j in seq(1,ncol(DF.fpca.fit)) ) {
            DF.fpca.fit[,j] <- DF.fpca.fit[,j] + vector.meanfd;
            }
        colnames(DF.fpca.fit) <- LIST.fpca[["target_variable_scores"]][,"X_Y_year"];
        DF.fpca.fit <- cbind("date_index" = temp.evalarg, DF.fpca.fit);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        for ( temp.XY.year in temp.XY.years ) {

            cat(paste0("\n# X_Y_year = ",temp.XY.year,"\n"));

            PNG.output <- paste0('fpca-fit-',beam.swath,'-',temp.year,'-',temp.type,'-',fpca.variable,'-',temp.XY.year,'.png');

            XY.string  <- temp.XY.year;
            XY.string  <- gsub(x = XY.string, pattern = "_[0-9]{4}$", replacement = "" );
            XY.string  <- gsub(x = XY.string, pattern = "_",          replacement = ",");
            XY.string  <- paste0("(x,y) = (",XY.string,")");

            my.ggplot <- initializePlot(
                title    = NULL,
                subtitle = paste0(beam.swath,", ",temp.year,", ",temp.type,", ",XY.string)
                );

            my.ggplot <- my.ggplot + ggplot2::xlab( label = "date index"  );
            my.ggplot <- my.ggplot + ggplot2::ylab( label = fpca.variable );
            my.ggplot <- my.ggplot + scale_x_continuous(limits=c(75,325),breaks=seq(100,300,50));

            my.ggplot <- my.ggplot + geom_vline(
                xintercept = range(LIST.fpca[["spline_grid"]]),
                colour     = "red",
                alpha      = 0.5,
                linetype   = 2,
                size       = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.year.type[DF.year.type[,"X_Y_year"] == temp.XY.year,c("date_index",fpca.variable)];
            DF.temp <- as.data.frame(DF.temp);
            colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = fpca.variable, replacement = "dummy.colname");
            my.ggplot <- my.ggplot + ggplot2::geom_point(
                data    = DF.temp,
                mapping = aes(x = date_index, y = dummy.colname),
                alpha   = 0.8,
                size    = 3,
                colour  = "black"
                );

            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.temp,
                mapping = aes(x = date_index, y = dummy.colname),
                colour  = "black",
                alpha   = 0.5
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.bsplines.original[,c("date_index",temp.XY.year)];
            DF.temp <- as.data.frame(DF.temp);
            colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = temp.XY.year, replacement = "dummy.colname");
            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.temp,
                mapping = aes(x = date_index, y = dummy.colname),
                colour  = "blue",
                size    = 1.3,
                alpha   = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.fpca.fit[,c("date_index",temp.XY.year)];
            DF.temp <- as.data.frame(DF.temp);
            colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = temp.XY.year, replacement = "dummy.colname");
            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.temp,
                mapping = aes(x = date_index, y = dummy.colname),
                colour  = "red",
                size    = 1.3,
                alpha   = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ggplot2::ggsave(
                file   = PNG.output,
                plot   = my.ggplot,
                dpi    = 150,
                height =   6,
                width  =  16,
                units  = 'in'
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            remove( list = c("my.ggplot") );

            }

        remove(list = c(
            "DF.year.type",
            "DF.bsplines.original",
            "vector.meanfd",
            "DF.fpca.standardizedTimepoints",
            "DF.fpca.fit"
            ));

        }}

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd( initial.directory );
    return( NULL );

    }

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
                limits = c(  -3,3),
                breaks = seq(-3,3,1)
                );
        } else {
            my.ggplot <- my.ggplot + scale_y_continuous(
                limits = c(  -40,20),
                breaks = seq(-40,20,10)
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

beam.swath.diagnostics_getData <- function(
    data.directory  = NULL,
    beam.swath      = NULL,
    colname.pattern = NULL,
    exclude.years   = NULL,
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
    years <- beam.swath.diagnostics_getYears(
        data.directory = data.directory,
	exclude.years  = exclude.years
	);

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

beam.swath.diagnostics_getYears <- function(data.directory = NULL, exclude.years = NULL) {
    require(stringr);
    temp.files <- list.files(path = data.directory);
    if ( !is.null(exclude.years) ) {
        temp.files <- grep(x = temp.files, pattern = exclude.years, value = TRUE, invert = TRUE)
        }
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

