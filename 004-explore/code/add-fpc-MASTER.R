
add.fpc <- function(
    DF.input            = NULL,
    week.indices        = NULL,
    spline.grid         = NULL,
    n.order             = NULL,
    n.basis             = NULL,
    smoothing.parameter = NULL,
    n.harmonics         = NULL,
    FILE.output.RData   = "fpc.RData",
    FILE.output.csv     = "fpc.csv"
    ) {

    this.function.name <- "add.fpc";
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
    require(fda);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.data <- DF.input;

    rownames(DF.data) <- sapply(
        X   = rownames(DF.data),
        FUN = function(x) { paste0(sample(x=letters,size=10,replace=TRUE),collapse="") }
        );

    cat("\nstr(DF.data):\n");
    print( str(DF.data)    );

    cat("\nsummary(DF.data):\n");
    print( summary(DF.data)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    NDVI.colnames <- paste0("NDVI",week.indices);

    t.DF.NDVI <- t(DF.data[,NDVI.colnames]);
    cat("\nstr(t.DF.NDVI):\n");
    print( str(t.DF.NDVI)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # create basis functions with respect to which
    # FPCA computations will be carried out
    week.basis <- fda::create.bspline.basis(
        rangeval    = range(week.indices),
        norder      = n.order,
        nbasis      = n.basis,
        dropind     = NULL,
        quadvals    = NULL,
        values      = NULL,
        basisvalues = NULL,
        names       = "bspl"
        );

    cat("\nstr(week.basis):\n");
    print( str(week.basis)    );

    # create associated functional data
    # parameter object for the basis functions
    # created above
    week.fdParObj <- fda::fdPar(
        fdobj  = week.basis,
        Lfdobj = NULL,
        lambda = smoothing.parameter,
        penmat = NULL
        );

    cat("\nstr(week.fdParObj):\n");
    print( str(week.fdParObj)    );

    # express NDVI data as linear combinations
    # of the basis functions created above
    NDVI.fd <- fda::smooth.basis(
        argvals     = week.indices,
        y           = t.DF.NDVI,
        fdParobj    = week.fdParObj,
        wtvec       = NULL,
        fdnames     = NULL,
        covariates  = NULL,
        method      = "chol",
        dfscale     = 1,
        returnMatrix=FALSE
        );

    cat("\nstr(NDVI.fd):\n");
    print( str(NDVI.fd)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualize.bslpine.fit(
        week.indices     = week.indices,
        spline.grid      = spline.grid,
        t.DF.time.series = t.DF.NDVI,
        time.series.fd   = NDVI.fd,
        prefix           = 'NDVI'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.pca.fd <- fda::pca.fd(
        fdobj = NDVI.fd[["fd"]],
        nharm = n.harmonics
        );

    cat("\nstr(results.pca.fd):\n");
    print( str(results.pca.fd)    );

    cat("\nresults.pca.fd[['values']]:\n");
    print( results.pca.fd[['values']]    );

    cat("\nresults.pca.fd[['values']] / sum(results.pca.fd[['values']]):\n");
    print( results.pca.fd[['values']] / sum(results.pca.fd[['values']])   );

    png("plot-fpca-NDVI.png", height = 4 * n.harmonics, width = 12, units = "in", res = 300);
    par(mfrow=c(n.harmonics,1));
    plot.pca.fd(x = results.pca.fd);
    dev.off()

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualize.fpca.fit(
        week.indices     = week.indices,
        spline.grid      = spline.grid,
        t.DF.time.series = t.DF.NDVI,
        time.series.fd   = NDVI.fd,
        results.pca.fd   = results.pca.fd,
        prefix           = 'NDVI'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### Recalculating the FPCA scores.
    ### It is necessary to know how to do this
    ### in order to incorporate FPCA-based feature
    ### extraction into the preprocessing component
    ### of a model training pipeline.

    reconstruct.fpca.scores(
        week.indices   = week.indices,
    	time.series.fd = NDVI.fd,
        week.fdParObj  = week.fdParObj,
        results.pca.fd = results.pca.fd
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    fpc.NDVI <- results.pca.fd[["scores"]];
    colnames(fpc.NDVI) <- paste0("NDVI_fpc",seq(1,ncol(fpc.NDVI)));

    cat("\nstr(fpc.NDVI)\n");
    print( str(fpc.NDVI)   );

    DF.output <- cbind(DF.data,fpc.NDVI);

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
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
