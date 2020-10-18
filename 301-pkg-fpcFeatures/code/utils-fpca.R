
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

