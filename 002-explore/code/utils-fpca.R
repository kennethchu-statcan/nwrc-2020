
visualize.bslpine.fit <-  function(
    week.indices     = NULL,
    spline.grid      = NULL,
    t.DF.time.series = NULL,
    time.series.fd   = NULL,
    prefix           = NULL
    ) {

    time.series.bspline <- fda::eval.fd(
        evalarg = spline.grid,
        fdobj   = time.series.fd[["fd"]]
        );

    cat("\nstr(time.series.bspline):\n");
    print( str(time.series.bspline)    );

    temp.columns <- sample(x = seq(1,ncol(t.DF.time.series)), size = 10);
    for ( temp.column in temp.columns ) {
        temp.file <- paste0("plot-bspline-",prefix,"-",temp.column,".png");
        png(temp.file, height = 12, width = 18, units = "in", res = 300);
        plot( x = week.indices, y = t.DF.time.series[   ,temp.column], type = "b", col = "black", lwd = 2);
        lines(x = spline.grid,  y = time.series.bspline[,temp.column], type = "l", col = "blue",  lwd = 1);
        dev.off();
        }

    return( NULL );

    }

visualize.fpca.fit <- function(
    week.indices     = NULL,
    spline.grid      = NULL,
    t.DF.time.series = NULL,
    time.series.fd   = NULL,
    results.pca.fd   = NULL,
    prefix           = NULL
    ) {

    time.series.bspline <- fda::eval.fd(
        evalarg = spline.grid,
        fdobj   = time.series.fd[["fd"]]
        );

    time.series.meanfd <- fda::eval.fd(
        evalarg = spline.grid, #week.indices,
        fdobj   = results.pca.fd[["meanfd"]]
        );

    time.series.harmonics <- fda::eval.fd(
        evalarg = spline.grid, #week.indices,
        fdobj   = results.pca.fd[["harmonics"]]
        );

    cat("\nstr(time.series.harmonics):\n");
    print( str(time.series.harmonics)    );

    time.series.fpca.fit <- time.series.harmonics %*% t( results.pca.fd[["scores"]] );
    for ( j in seq(1,ncol(time.series.fpca.fit)) ) {
        time.series.fpca.fit[,j] <- time.series.fpca.fit[,j] + time.series.meanfd;
        }

    cat("\nstr(time.series.fpca.fit):\n");
    print( str(time.series.fpca.fit)    );

    temp.columns <- sample(x = seq(1,ncol(t.DF.time.series)), size = 10);
    for ( temp.column in temp.columns ) {
        temp.file <- paste0("plot-fpca-",prefix,"-",temp.column,".png");
        png(temp.file, height = 12, width = 18, units = "in", res = 300);
        plot( x = week.indices, y = t.DF.time.series[    ,temp.column], type = "b", col = "black",lwd = 2);
        lines(x = spline.grid,  y = time.series.bspline[ ,temp.column], type = "l", col = "blue", lwd = 1);
        lines(x = spline.grid,  y = time.series.fpca.fit[,temp.column], type = "l", col = "red",  lwd = 1);
        dev.off();
        }

    return( NULL );

    }

reconstruct.fpca.scores <- function(
    week.indices   = NULL,
    time.series.fd = NULL,
    week.fdParObj  = NULL,
    results.pca.fd = NULL
    ) {

    ### Recalculating the FPCA scores.
    ### It is necessary to know how to do this
    ### in order to incorporate FPCA-based feature
    ### extraction into the preprocessing component
    ### of a model training pipeline.

    ### ~~~~~~~~~~~~~~~~~~~~ ###
    # The FPCA scores can be reconstructed
    # for the "training" data NDVI.fd[["fd"]]
    # very simply as follows:

    #results.inprod <- fda::inprod(
    #    fdobj1 = center.fd(NDVI.fd[["fd"]]),
    #    fdobj2 = results.pca.fd[["harmonics"]]
    #    );

    # However, for feature extraction purposes,
    # the above is NOT sufficient, because for
    # validation/teseting/new data, we need to
    # subtract from them the mean curve of the
    # training data (rather than their own
    # respective mean curves). Hence, the use of
    # center.fd() on validation/testing/new data
    # would NOT be appropriate.

    # More concretely, note that the input to
    # fda::inprod() should be:
    #
    #   fdobj1 = validation/testing/new data,
    #            minus mean curve of training data
    #
    #   fdobj2 = FPCA harmonics computed based
    #            solely on training data

    ### ~~~~~~~~~~~~~~~~~~~~ ###
    temp.ncols <- ncol(time.series.fd[['fd']][['coefs']]);

    temp.row.means <- apply(
        X      = time.series.fd[['fd']][['coefs']],
        MARGIN = 1,
        FUN    = function(x) { mean(x) }
        );

    time.series.fd.centered <- fd(
        coef     = time.series.fd[['fd']][['coefs']] - matrix(rep(temp.row.means,temp.ncols),ncol=temp.ncols),
        basisobj = time.series.fd[['fd']][['basis']],
        fdnames  = NULL
        );
    attr(time.series.fd.centered[['coefs']],"dimnames") <- NULL;

    cat("\nstr(time.series.fd.centered):\n");
    print( str(time.series.fd.centered)    );

    results.inprod <- fda::inprod(
        fdobj1 = time.series.fd.centered,
        fdobj2 = results.pca.fd[["harmonics"]]
        );

    ### ~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(results.inprod):\n");
    print( str(results.inprod)    );

    cat("\nstr(results.pca.fd[['scores']]):\n");
    print( str(results.pca.fd[['scores']])    );

    cat("\nmax(abs( results.inprod - results.pca.fd[['scores']] )):\n");
    print( max(abs( results.inprod - results.pca.fd[['scores']] ))    );

    ### ~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }
