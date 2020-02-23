
doFPCA <- function(
    DF.input            = NULL,
    target.variable     = NULL,
    beam.swath          = NULL,
    week.indices        = NULL,
    spline.grid         = NULL,
    n.order             = NULL,
    n.basis             = NULL,
    smoothing.parameter = NULL,
    n.harmonics         = NULL,
    FILE.output.RData   = paste0("tmp-",beam.swath,"-FPC-",target.variable,".RData"),
    FILE.output.csv     = paste0("tmp-",beam.swath,"-FPC-",target.variable,".csv"  )
    ) {

    this.function.name <- "doFPCA";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output.RData)) {
        cat(paste0("\nloading file: ",FILE.output.RData,"\n"));
        DF.output <- base::readRDS(file = FILE.output.RData);
        cat(paste0("\nexiting: ",this.function.name,"()"));
        cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
        return( DF.output );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(dplyr);
    require(tidyr);
    require(fda);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nstr(DF.input) -- doFPCA(), ",target.variable,"\n"));
    print( str(DF.input)   );

    DF.temp <- DF.input[,c("X_Y_year","year","type","date_index",target.variable)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = target.variable,
        replacement = "target_variable"
        );

    DF.temp <- DF.temp %>%
        tidyr::spread(key = date_index, value = target_variable);
    DF.temp <- as.data.frame(DF.temp);
    rownames(DF.temp) <- DF.temp[,"X_Y_year"];

    DF.temp <- DF.temp[0 == rowSums(is.na(DF.temp)),];

    cat(paste0("\nstr(DF.temp) -- doFPCA(), ",target.variable,"\n"));
    print( str(DF.temp) );

    cat(paste0("\nsummary(DF.temp) -- doFPCA(), ",target.variable,"\n"));
    print( summary(DF.temp) );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    date_index.colnames <- grep(
        x       = colnames(DF.temp),
        pattern = "^[0-9]+$",
        value = TRUE
        );

    #first.new.year.day <- as.Date(paste0(min(as.integer(DF.input[,"year"])),"-01-01"));

    DF.dates <- data.frame(
        date_index_char  = as.character(date_index.colnames),
        date_index       = as.integer(  date_index.colnames),
        stringsAsFactors = FALSE
        );

    cat("\nstr(DF.dates)\n");
    print( str(DF.dates)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    t.DF.temp <- t(DF.temp[,DF.dates[,"date_index_char"]]);
    cat("\nstr(t.DF.temp)\n");
    print( str(t.DF.temp)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # create basis functions with respect to which
    # FPCA computations will be carried out
    bspline.basis <- fda::create.bspline.basis(
        rangeval    = range(DF.dates[,"date_index"]),
        norder      = n.order,
        nbasis      = n.basis,
        dropind     = NULL,
        quadvals    = NULL,
        values      = NULL,
        basisvalues = NULL,
        names       = "bspl"
        );

    cat("\nstr(bslpine.basis):\n");
    print( str(bspline.basis)    );

    # create associated functional data
    # parameter object for the basis functions
    # created above
    bspline.basis.fdParObj <- fda::fdPar(
        fdobj  = bspline.basis,
        Lfdobj = NULL,
        lambda = smoothing.parameter,
        penmat = NULL
        );

    cat("\nstr(bspline.basis.fdParObj):\n");
    print( str(bspline.basis.fdParObj)    );

    # express NDVI data as linear combinations
    # of the basis functions created above
    target.variable.fd <- fda::smooth.basis(
        argvals      = DF.dates[,"date_index"],
        y            = t.DF.temp,
        fdParobj     = bspline.basis.fdParObj,
        wtvec        = NULL,
        fdnames      = NULL,
        covariates   = NULL,
        method       = "chol",
        dfscale      = 1,
        returnMatrix = FALSE
        );

    cat("\nstr(target.variable.fd):\n");
    print( str(target.variable.fd)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    spline.grid <- seq(
        min(DF.dates[,"date_index"]),
        max(DF.dates[,"date_index"]),
        0.1
        );

    visualize.bslpine.fit(
        week.indices     = DF.dates[,"date_index"],
        spline.grid      = spline.grid,
        t.DF.time.series = t.DF.temp,
        time.series.fd   = target.variable.fd,
        prefix           = paste0(beam.swath,"-",target.variable)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return.value.tryCatch <- tryCatch(
        expr = {
            results.pca.fd <- fda::pca.fd(
                fdobj = target.variable.fd[["fd"]],
                nharm = n.harmonics
                );
            },
        error = function(e) {
            my.message <- paste0("Error: fda::pca.fd(), ",beam.swath,", ",target.variable);
            message("\n");
            message(my.message);
            message(e);
            message("\n");
            return( -1 );
            }
        );

    if ( "pca.fd" != class(return.value.tryCatch) ) {
        if ( 0 > return.value.tryCatch ) { return(NULL) }
        }

    cat("\nstr(results.pca.fd):\n");
    print( str(results.pca.fd)    );

    cat("\nresults.pca.fd[['values']]:\n");
    print( results.pca.fd[['values']]    );

    cat("\nresults.pca.fd[['values']] / sum(results.pca.fd[['values']]):\n");
    print( results.pca.fd[['values']] / sum(results.pca.fd[['values']])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return.value.tryCatch <- tryCatch(
        expr = {
            n.plots    <- min(5,n.harmonics);
            PNG.output <- paste0("tmp-",beam.swath,"-FPCA-",target.variable,".png");
            png(filename = PNG.output, height = 4 * n.plots, width = 12, units = "in", res = 300);
            par(mfrow=c(n.plots,1));
            plot.pca.fd(x = results.pca.fd, harm = 1:n.plots);
            dev.off();
            },
	error = function(e) {
            my.message <- paste0("Error: fda::plot.pca.fd(), ",beam.swath,", ",target.variable);
            message("\n");
            message(my.message);
            message(e);
            message("\n");
	    message("\nstr(results.pca.fd)\n");
	    message(   str(results.pca.fd)   );
            return( -1 );
	    }
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualize.fpca.fit(
        week.indices     = DF.dates[,"date_index"],
        spline.grid      = spline.grid,
        t.DF.time.series = t.DF.temp,
        time.series.fd   = target.variable.fd,
        results.pca.fd   = results.pca.fd,
        prefix           = paste0(beam.swath,"-",target.variable)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### Recalculating the FPCA scores.
    ### It is necessary to know how to do this
    ### in order to incorporate FPCA-based feature
    ### extraction into the preprocessing component
    ### of a model training pipeline.

    reconstruct.fpca.scores(
        week.indices   = DF.dates[,"date_index"],
    	time.series.fd = target.variable.fd,
        week.fdParObj  = bspline.basis.fdParObj,
        results.pca.fd = results.pca.fd
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    fpc.scores <- results.pca.fd[["scores"]];
    colnames(fpc.scores) <- paste0("fpc_",seq(1,ncol(fpc.scores)));

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nstr(fpc.scores)\n");
    print( str(fpc.scores)   );

    DF.output <- cbind(DF.temp,fpc.scores);

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nsaving to file: ",FILE.output.RData,"\n"));
    base::saveRDS(object = DF.output, file = FILE.output.RData);

    if ( !file.exists(FILE.output.csv) ) {
        cat(paste0("\nwriting file: ",FILE.output.csv,"\n"));
        write.csv(
            file      = FILE.output.csv,
            x         = DF.output,
            row.names = FALSE
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    doFPCA_scatter(
        DF.input   = DF.output,
        beam.swath = beam.swath,
        x.var      = "fpc_1",
        y.var      = "fpc_2",
        title      = NULL,
        subtitle   = paste0(beam.swath,', ',target.variable),
        PNG.output = paste0('tmp-',beam.swath,'-FPCA-scatter-',target.variable,'.png')
        );

    years <- unique(DF.output[,"year"]);
    for ( year in years ) {
        doFPCA_scatter(
            DF.input   = DF.output[DF.output[,"year"] == year,],
            beam.swath = beam.swath,
            x.var      = "fpc_1",
            y.var      = "fpc_2",
            title      = NULL,
            subtitle   = paste0(beam.swath,', ',target.variable,', ',year),
            PNG.output = paste0('tmp-',beam.swath,'-FPCA-scatter-',target.variable,'-',year,'.png')
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
doFPCA_scatter <- function(
    DF.input   = NULL,
    beam.swath = NULL,
    x.var      = NULL,
    y.var      = NULL,
    title      = NULL,
    subtitle   = NULL,
    PNG.output = 'tmp-FPCA-scatter.png'
    ) {

    require(ggplot2);

    DF.temp <- DF.input[,c("type",x.var,y.var)];

    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = x.var,
        replacement = "x_var"
        );

    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = y.var,
        replacement = "y_var"
        );

    my.ggplot <- initializePlot(
        title    = title,
        subtitle = subtitle
        );

    if ( grepl(x = subtitle, pattern = "scaled") ) {
        my.ggplot <- my.ggplot + scale_x_continuous(limits=20*c(-1,1),breaks=seq(-20,20,5));
        my.ggplot <- my.ggplot + scale_y_continuous(limits=20*c(-1,1),breaks=seq(-20,20,5));
    } else {
        my.ggplot <- my.ggplot + scale_x_continuous(limits=300*c(-1,1),breaks=seq(-300,300,100));
        my.ggplot <- my.ggplot + scale_y_continuous(limits=300*c(-1,1),breaks=seq(-300,300,100));
    }

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.temp,
        mapping = aes(x = x_var, y = y_var, colour = type),
        alpha   = 0.3
        );

    ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  10,
        units  = 'in'
        );

    return( NULL );

    }

