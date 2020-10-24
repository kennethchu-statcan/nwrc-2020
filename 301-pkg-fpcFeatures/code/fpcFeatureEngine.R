
#' R6 Class for FPC feature engine (for collections of annual SAR measurement time series)
#'
#' @description
#' This R6 class implements an FPC feature engine for collections of annual SAR
#' measurement time series.
#'
#' @details
#' This R6 class implements an FPC feature engine for collections of annual SAR
#' measurement time series.
#' More precisely, this R6 class provides functionality to perform
#' the following:
#' *  compute the functional principal components (FPC) of given training data
#'    (a collection of annual SAR measurement time series)
#' *  compute functional principal component scores for new annual SAR
#'    measurement time series data with resepct to the FPC's of the training
#'    data.
#'
#' @import cowplot dplyr fda ggplot2 logger
#' @importFrom R6 R6Class
#'
#' @export

fpcFeatureEngine <- R6::R6Class(

    classname = 'fpcFeatureEngine',

    public = base::list(

        # instantiation parameters
        learner.metadata    = NULL,
        training.data       = NULL,
        location            = NULL,
        date                = NULL,
        variable            = NULL,
        n.partition         = NULL,
        n.order             = NULL,
        n.basis             = NULL,
        smoothing.parameter = NULL,
        n.harmonics         = NULL,

        # class attributes
        standardized.bspline.basis = NULL,
        learned.fpca.parameters    = NULL,

        #' @description
        #' Create a fpcFeatureEngine object.
        #' @param learner.metadata learner.metadata.
        #' @param training.data data frame
        #' @param location location
        #' @param date date
        #' @param variable variable
        #' @param n.partition n.partition
        #' @param n.order n.order
        #' @param n.basis n.basis
        #' @param smoothing.parameter smoothing.parameter
        #' @param n.harmonics n.harmonics
        #' @return A new `fpcFeatureEngine` object.
        initialize = function(
            learner.metadata    = NULL,
            training.data       = NULL,
            location            = NULL,
            date                = NULL,
            variable            = NULL,
            n.partition         = 100,
            n.order             =   3,
            n.basis             =   9,
            smoothing.parameter =   0.1,
            n.harmonics         =   7
            ) {
            self$learner.metadata    <- learner.metadata;
            self$training.data       <- training.data;
            self$location            <- location;
            self$date                <- date;
            self$variable            <- variable;
            self$n.partition         <- n.partition;
            self$n.order             <- n.order;
            self$n.basis             <- n.basis;
            self$smoothing.parameter <- smoothing.parameter;
            self$n.harmonics         <- n.harmonics;
            },

        #' @description
        #' learn from training data (compute functional principal components).
        fit = function() {

            DF.temp <- private$add.auxiliary.columns(DF.input = self$training.data);
            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            self$standardized.bspline.basis <- private$get.standardized.bspline.basis(
                DF.input = DF.temp
                );
            cat("\nstr(self$standardized.bspline.basis)\n");
            print( str(self$standardized.bspline.basis)   );

            DF.temp <- private$standardized.grid.interpolate(
                DF.input = DF.temp
                );
            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            self$learned.fpca.parameters <- private$learn.fpca.parameters(DF.input = DF.temp);
            cat("\nstr(self$learned.fpca.parameters)\n");
            print( str(self$learned.fpca.parameters)   );

            }, # fit()

        #' @description
        #' compute functional principal component scores for new data with respect to functional principale components of training data.
        #' @param newdata newdata
        #' @return A new `fpcFeatureEngine` object.
        transform = function(newdata = NULL) {

            DF.temp <- private$add.auxiliary.columns(DF.input = newdata);
            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            DF.dictionary <- unique(DF.temp[,c('location_year',self$location,'year')]);
            cat("\nstr(DF.dictionary)\n");
            print( str(DF.dictionary)   );

            DF.newdata.standardized.wide <- private$standardized.grid.interpolate(DF.input = DF.temp);
            base::rownames(DF.newdata.standardized.wide) <- base::sapply(
                X   = base::rownames(DF.newdata.standardized.wide),
                FUN = function(x) { base::paste0(base::sample(x=letters,size=20,replace=TRUE),collapse="") }
                );
            cat("\nstr(DF.newdata.standardized.wide)\n");
            print( str(DF.newdata.standardized.wide)   );

            DF.fpc <- private$apply.fpca.parameters(
                DF.input  = DF.newdata.standardized.wide[,base::setdiff(base::colnames(DF.newdata.standardized.wide),base::c('location_year','year'))],
                visualize = FALSE
                );

            DF.output <- base::cbind(
                DF.newdata.standardized.wide,
                DF.fpc[rownames(DF.newdata.standardized.wide),]
                );

            DF.output <- base::merge(
                x     = DF.output,
                y     = DF.dictionary[,base::c('location_year',self$location)],
                by    = "location_year",
                all.x = TRUE
                );
            base::rownames(DF.output) <- DF.output[,'location_year'];

            cat("\nstr(DF.output)\n");
            print( str(DF.output)   );

            colnames.to.delete <- base::c('location_year',self$location,'year');
            DF.output <- DF.output[,base::c(self$location,'year',base::setdiff(base::colnames(DF.output),colnames.to.delete))];

            return( DF.output );

            }, # transform()

        plot.approximations = function(
            DF.input                     = NULL,
            location                     = NULL,
            date                         = NULL,
            variable                     = NULL,
            LIST.standardized_timepoints = NULL,
            LIST.fpca                    = NULL
            ) {

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            cat("\nstr(DF.data)\n");
            print( str(DF.data)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            temp.year <- base::as.character(base::format(x = DF.input[1,date], format = "%Y"));
            temp.location <- DF.input[1,location];
            temp.location_year <- paste0(temp.location,"_",temp.year);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            my.ggplot <- initializePlot(
                title    = NULL,
                subtitle = paste0("year: ", temp.year,", location: ",temp.location)
                );

            my.ggplot <- my.ggplot + ggplot2::xlab( label = "date index (1 = New Year's Day)" );
            my.ggplot <- my.ggplot + ggplot2::ylab( label = variable );
            my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(limits=c(75,325),breaks=seq(100,300,50));

            my.ggplot <- my.ggplot + ggplot2::geom_vline(
                xintercept = range(self$standardized.bspline.basis[['spline.grid']]),
                colour     = "red",
                alpha      = 0.5,
                linetype   = 2,
                size       = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.raw <- private$add.auxiliary.columns(DF.input = DF.input, location = location, date = date);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- base::as.data.frame(DF.raw[,c("date_index",variable)]);
            base::colnames(DF.temp) <- base::gsub(x = base::colnames(DF.temp), pattern = variable, replacement = "dummy.colname");
            my.ggplot <- my.ggplot + ggplot2::geom_point(
                data    = DF.temp,
                mapping = ggplot2::aes(x = date_index, y = dummy.colname),
                alpha   = 0.8,
                size    = 3,
                colour  = "black"
                );

            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.temp,
                mapping = ggplot2::aes(x = date_index, y = dummy.colname),
                colour  = "black",
                alpha   = 0.5
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp      <- DF.raw[,base::c("date_index",variable)];
            temp.evalarg <- seq(min(DF.temp[,"date_index"]),max(DF.temp[,"date_index"]),0.1);

            temp.bspline.basis <- fda::create.bspline.basis(
                rangeval    = base::range(DF.temp[,"date_index"]),
                norder      = self$n.order,
                nbasis      = self$n.basis,
                dropind     = NULL,
                quadvals    = NULL,
                values      = NULL,
                basisvalues = NULL,
                names       = "bspl"
                );

            temp.bspline.basis.fdParObj <- fda::fdPar(
                fdobj  = temp.bspline.basis,
                Lfdobj = NULL,
                lambda = self$smoothing.parameter,
                penmat = NULL
                );

            target.in.basis.fd <- fda::smooth.basis(
                argvals      = base::as.integer(DF.temp[,"date_index"]),
                y            = base::as.matrix(x = DF.temp[,variable], ncol = 1),
                fdParobj     = temp.bspline.basis.fdParObj,
                wtvec        = NULL,
                fdnames      = NULL,
                covariates   = NULL,
                method       = "chol",
                dfscale      = 1,
                returnMatrix = FALSE
                );

            bspline.approximation <- fda::eval.fd(
                fdobj   = target.in.basis.fd[["fd"]],
                evalarg = temp.evalarg
                );

            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = data.frame(
                    date_index    = temp.evalarg,
                    dummy_colname = bspline.approximation
                    ),
                mapping = ggplot2::aes(x = date_index, y = dummy_colname),
                colour  = "blue",
                size    = 1.3,
                alpha   = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            temp.evalarg <- base::seq(
                base::min(self$standardized.bspline.basis[["spline.grid"]]),
                base::max(self$standardized.bspline.basis[["spline.grid"]]),
                0.1
                );

            DF.temp <- DF.raw[,base::c("location_year",location,"year","date_index",variable)];
            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );
            DF.newdata.standardized.wide <- private$standardized.grid.interpolate(
                DF.input = DF.temp,
                location = location,
                variable = variable
                );
            base::rownames(DF.newdata.standardized.wide) <- base::sapply(
                X   = base::rownames(DF.newdata.standardized.wide),
                FUN = function(x) { base::paste0(base::sample(x=letters,size=20,replace=TRUE),collapse="") }
                );
            cat("\nstr(DF.newdata.standardized.wide)\n");
            print( str(DF.newdata.standardized.wide)   );

            DF.fpc <- private$apply.fpca.parameters(
                DF.input  = DF.newdata.standardized.wide[,base::setdiff(base::colnames(DF.newdata.standardized.wide),base::c('location_year','year'))],
                visualize = FALSE
                );
            base::rownames(DF.fpc) <- DF.newdata.standardized.wide[,'location_year'];
            cat("\nstr(DF.fpc)\n");
            print( str(DF.fpc)   );

            vector.meanfd <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["meanfd"]] #LIST.fpca[["target_variable_fpc"]][["meanfd"]]
                );

            DF.fpca.standardizedTimepoints <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["harmonics"]] # LIST.fpca[["target_variable_fpc"]][["harmonics"]]
                );

            #DF.fpca.fit <- DF.fpca.standardizedTimepoints %*% t( LIST.fpca[["target_variable_fpc"]][["scores"]] );
            DF.fpca.fit <- DF.fpca.standardizedTimepoints %*% t( DF.fpc );
            for ( j in seq(1,ncol(DF.fpca.fit)) ) {
                DF.fpca.fit[,j] <- DF.fpca.fit[,j] + vector.meanfd;
                }
            #colnames(DF.fpca.fit) <- LIST.fpca[["target_variable_scores"]][,"X_Y_year"];
            DF.fpca.fit <- cbind("date_index" = temp.evalarg, DF.fpca.fit);
            colnames(DF.fpca.fit) <- gsub(x = colnames(DF.fpca.fit), pattern = temp.location_year, replacement = "dummy_colname");
            DF.fpca.fit <- as.data.frame(DF.fpca.fit)

            cat("\nstr(DF.fpca.fit)\n");
            print( str(DF.fpca.fit)   );

            # DF.temp <- DF.fpca.fit[,c("date_index",temp.XY.year)];
            # DF.temp <- as.data.frame(DF.temp);
            # colnames(DF.temp) <- gsub(x = colnames(DF.temp), pattern = temp.XY.year, replacement = "dummy.colname");
            # fpca.approximation <- fda::eval.fd(
            #     fdobj   = target.in.basis.fd[["fd"]],
            #     evalarg = temp.evalarg
            #     );
            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.fpca.fit,
                mapping = ggplot2::aes(x = date_index, y = dummy_colname),
                colour  = "red",
                size    = 1.3,
                alpha   = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            return( my.ggplot );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            initial.directory     <- getwd();
            temp.output.directory <- file.path(initial.directory,"diagnostics-fpca-complete");
            if ( !dir.exists(temp.output.directory) ) {
                dir.create(path = temp.output.directory, recursive = TRUE);
                }
            setwd( temp.output.directory );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.data <- private$add.auxiliary.columns(DF.input = DF.data);

            years <- unique(DF.data[,"year"]);
            for ( temp.year in years ) {

                cat(paste0("\n# year: ",temp.year,"\n"));

                is.selected   <- (DF.data[,"year"] == temp.year);
                DF.year.type  <- DF.data[is.selected,c("location_year","date_index",self$variable)];
                temp.XY.years <- sample(x = unique(DF.year.type[,"location_year"]), size = 10, replace = FALSE);

                ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
                temp.evalarg <- seq(min(DF.year.type[,"date_index"]),max(DF.year.type[,"date_index"]),0.1);

                temp.bspline.basis <- fda::create.bspline.basis(
                    rangeval    = base::range(base::as.integer(base::colnames(DF.temp.year))),
                    norder      = self$n.order,
                    nbasis      = self$n.basis,
                    dropind     = NULL,
                    quadvals    = NULL,
                    values      = NULL,
                    basisvalues = NULL,
                    names       = "bspl"
                    );

                temp.bspline.basis.fdParObj <- fda::fdPar(
                    fdobj  = temp.bspline.basis,
                    Lfdobj = NULL,
                    lambda = self$smoothing.parameter,
                    penmat = NULL
                    );

                # express target.variable as linear combinations
                # of the B-spline basis functions
                target.in.basis.fd <- fda::smooth.basis(
                    argvals      = base::as.integer(base::colnames(DF.temp.year)),
                    y            = t.DF.temp.year,
                    fdParobj     = temp.bspline.basis.fdParObj,
                    wtvec        = NULL,
                    fdnames      = NULL,
                    covariates   = NULL,
                    method       = "chol",
                    dfscale      = 1,
                    returnMatrix = FALSE
                    );

                # evaluate the B-spline approximations at grid points
                # of the across-year common grid
                bspline.approximation <- fda::eval.fd(
                    fdobj   = target.in.basis.fd[["fd"]],
                    evalarg = self$standardized.bspline.basis[["spline.grid"]]
                    );

                DF.bsplines.original <- fda::eval.fd(
                    evalarg = temp.evalarg,
                    # fdobj = LIST.standardized_timepoints[["list_bsplines"]][[fpca.variable]][[temp.year]][["target_in_basis_fd"]][["fd"]]
                    fdobj   = standardized.bspline.basis[['bspline.basis']][["fd"]]
                    );

                temp.XY.years <- intersect(temp.XY.years,colnames(DF.bsplines.original));

                DF.bsplines.original <- DF.bsplines.original[,temp.XY.years];
                DF.bsplines.original <- cbind("date_index" = temp.evalarg, DF.bsplines.original);

                ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

                ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
                for ( temp.XY.year in temp.XY.years ) {

                    cat(paste0("\n# X_Y_year = ",temp.XY.year,"\n"));

                    PNG.output <- paste0('fpca-fit-',beam.swath,'-',temp.year,'-',temp.type,'-',fpca.variable,'-',temp.XY.year,'.png');

                    XY.string  <- temp.XY.year;
                    XY.string  <- gsub(x = XY.string, pattern = "_[0-9]{4}$", replacement = "" );
                    XY.string  <- gsub(x = XY.string, pattern = "_",          replacement = ",");
                    XY.string  <- paste0("(x,y) = (",XY.string,")");

                    my.ggplot <- initializePlot(
                        title    = NULL,
                        subtitle = paste0(temp.year,", ",XY.string)
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

                    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

                    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

                    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

                    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
                    ggplot2::ggsave(
                        file   = PNG.output,
                        plot   = my.ggplot,
                        dpi    = 150,
                        height =   6,
                        width  =  16,
                        units  = 'in'
                        );

                    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
                    remove( list = c("my.ggplot") );

                    }

                remove(list = c(
                    "DF.year.type",
                    "DF.bsplines.original",
                    "vector.meanfd",
                    "DF.fpca.standardizedTimepoints",
                    "DF.fpca.fit"
                    ));

                }

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            setwd( initial.directory );
            return( NULL );

            }, # plot.approximations()

        #' @description
        #' plot the harmonics (functional principal components) computed based on the training data.
        plot.harmonics = function() {

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            temp.evalarg <- base::seq(
                base::min(self$standardized.bspline.basis[["spline.grid"]]),
                base::max(self$standardized.bspline.basis[["spline.grid"]]),
                0.1
                );

            vector.meanfd <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["meanfd"]]
                );

            DF.fpca.standardizedTimepoints <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["harmonics"]]
                );

            DF.fpca.harmonics.plus  <- DF.fpca.standardizedTimepoints %*% diag(sqrt( self$learned.fpca.parameters[["training.pca.fd"]][["values"]][1:self$n.harmonics] ));

            DF.fpca.harmonics.minus <- DF.fpca.harmonics.plus;
            for ( j in base::seq(1,base::ncol(DF.fpca.harmonics.plus)) ) {
                DF.fpca.harmonics.plus[, j] <- vector.meanfd + DF.fpca.harmonics.plus[, j];
                DF.fpca.harmonics.minus[,j] <- vector.meanfd - DF.fpca.harmonics.minus[,j];
                }
            base::colnames(DF.fpca.harmonics.plus ) <- base::paste0("harmonic",base::seq(1,base::ncol(DF.fpca.harmonics.plus )));
            base::colnames(DF.fpca.harmonics.minus) <- base::paste0("harmonic",base::seq(1,base::ncol(DF.fpca.harmonics.minus)));
            DF.fpca.harmonics.plus  <- base::cbind("date_index" = temp.evalarg, DF.fpca.harmonics.plus );
            DF.fpca.harmonics.minus <- base::cbind("date_index" = temp.evalarg, DF.fpca.harmonics.minus);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            list.plots <- base::list();
            temp.harmonics <- base::setdiff(base::colnames(DF.fpca.harmonics.plus),"date_index");
            for ( temp.index in base::seq(1,base::length(temp.harmonics)) ) {

                temp.harmonic <- temp.harmonics[temp.index];

                temp.varprop <- self$learned.fpca.parameters[["training.pca.fd"]][["varprop"]][temp.index];
                temp.varprop <- base::round(x = 100 * temp.varprop, digits = 3);

                my.ggplot <- initializePlot(
                    title    = NULL,
                    subtitle = base::paste0(self$variable," (variability captured = ",temp.varprop,"%)")
                    );

                temp.xlab <- ifelse(temp.index == base::length(temp.harmonics),"date index (1 = New Year's Day)","");
                temp.ylab <- base::gsub(x = temp.harmonic, pattern = "harmonic", replacement = "FPC ");

                my.ggplot <- my.ggplot + ggplot2::xlab( label = temp.xlab );
                my.ggplot <- my.ggplot + ggplot2::ylab( label = temp.ylab );
                my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(limits=c(75,325),breaks=seq(100,300,50));

                DF.temp <- base::as.data.frame(DF.fpca.harmonics.plus[,base::c("date_index",temp.harmonic)]);
                base::colnames(DF.temp) <- base::gsub(x = base::colnames(DF.temp), pattern = temp.harmonic, replacement = "dummy.colname");
                my.ggplot <- my.ggplot + ggplot2::geom_line(
                    data     = DF.temp,
                    mapping  = ggplot2::aes(x = date_index, y = dummy.colname),
                    colour   = "#FF9700",
                    linetype = "solid",
                    size     = 2.0,
                    alpha    = 0.8
                    );

                DF.temp <- base::data.frame("date_index" = temp.evalarg, "dummy.colname" = vector.meanfd);
                base::colnames(DF.temp) <- base::gsub(x = base::colnames(DF.temp), pattern = "mean", replacement = "dummy.colname");
                my.ggplot <- my.ggplot + ggplot2::geom_line(
                    data     = DF.temp,
                    mapping  = ggplot2::aes(x = date_index, y = dummy.colname),
                    colour   = "gray",
                    linetype = "solid",
                    size     = 1.0,
                    alpha    = 0.8
                    );

                DF.temp <-  base::as.data.frame(DF.fpca.harmonics.minus[,c("date_index",temp.harmonic)]);
                base::colnames(DF.temp) <-  base::gsub(x =  base::colnames(DF.temp), pattern = temp.harmonic, replacement = "dummy.colname");
                my.ggplot <- my.ggplot + ggplot2::geom_line(
                    data     = DF.temp,
                    mapping  = ggplot2::aes(x = date_index, y = dummy.colname),
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
            base::return( my.cowplot );

            } # plot.harmonics()

        ), # public = base::list()

    private = base::list(

        add.auxiliary.columns = function(DF.input = self$training.data, date = self$date, location = self$location) {
            DF.output <- DF.input;
            DF.output[,'year'] <- base::as.character(base::format(x = DF.output[,date], format = "%Y"));
            DF.output[,'location_year'] <- base::apply(
                X      = DF.output[,c(location,'year')],
                MARGIN = 1,
                FUN    = function(x) {base::return(base::paste(x,collapse="_"))}
                );
            DF.output[,"new_year_day"] <- base::as.Date(base::paste0(DF.output[,"year"],"-01-01"));
            DF.output[,"date_index"]   <- base::as.integer(DF.output[,date]) - base::as.integer(DF.output[,"new_year_day"]) + 1;
            base::return( DF.output );
            },

        get.standardized.bspline.basis = function(DF.input = NULL) {

            DF.yearly.endpoints <- base::unique(DF.input[,c("year",self$date,"date_index")]) %>%
                dplyr::group_by(year) %>%
                dplyr::mutate( start_index = min(date_index), end_index = max(date_index) );
            DF.yearly.endpoints <- base::as.data.frame(DF.yearly.endpoints);
            DF.yearly.endpoints <- DF.yearly.endpoints[base::order(DF.yearly.endpoints[,self$date]),];

            common.start.index <- base::max(DF.yearly.endpoints[,"start_index"]);
            common.end.index   <- base::min(DF.yearly.endpoints[,  "end_index"]);

            step.size   <- base::round( (common.end.index - common.start.index) / self$n.partition );
            spline.grid <- base::seq(common.start.index, common.end.index, step.size);

            my.bspline.basis <- fda::create.bspline.basis(
               rangeval    = base::range(spline.grid),
               norder      = self$n.order,
               nbasis      = self$n.basis,
               dropind     = NULL,
               quadvals    = NULL,
               values      = NULL,
               basisvalues = NULL,
               names       = "bspl"
               );

            my.bspline.basis.fdParObj <- fda::fdPar(
                fdobj  = my.bspline.basis,
                Lfdobj = NULL,
                lambda = self$smoothing.parameter,
                penmat = NULL
                );

            list.output <- base::list(
                start.end.indexes      = base::c(common.start.index,common.end.index),
                step.size              = step.size,
                spline.grid.range      = base::range(spline.grid),
                spline.grid            = spline.grid,
                bspline.basis          = my.bspline.basis,
                bspline.basis.fdParObj = my.bspline.basis.fdParObj
                );

            base::return( list.output );

            }, # get.standardized.bspline.basis()

        standardized.grid.interpolate = function(
            DF.input = NULL,
            location = self$location,
            variable = self$variable
            ) {

            cat("\nstandardized.grid.interpolate()\n");

            cat("\nstr(DF.input)\n");
            print( str(DF.input)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.input[,c("location_year","year","date_index",variable)];
            colnames(DF.temp) <- gsub(
                x           = colnames(DF.temp),
                pattern     = variable,
                replacement = "target_variable"
                );

            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.stack <- base::data.frame();

            years <- base::unique(DF.input[,"year"]);
            for ( year in years ) {

                cat(paste0("\nstandized.grid.interpolate() -- ",variable,", ",year,"\n"));

                DF.temp.year <- DF.temp[DF.temp[,"year"] == year,] %>%
                    tidyr::spread(key = date_index, value = target_variable);
                DF.temp.year <- base::as.data.frame(DF.temp.year);
                base::rownames(DF.temp.year) <- DF.temp.year[,"location_year"];
                DF.temp.year <- DF.temp.year[,base::setdiff(base::colnames(DF.temp.year),base::c("location_year","year"))];

                cat("\nstr(DF.temp.year)\n");
                print( str(DF.temp.year)   );

                DF.temp.year <- DF.temp.year[0 == base::rowSums(base::is.na(DF.temp.year)),];

                cat("\nstr(DF.temp.year)\n");
                print( str(DF.temp.year)   );

                t.DF.temp.year <- t(DF.temp.year);

                # cat("\nstr(t.DF.temp.year)\n");
                # print( str(t.DF.temp.year)   );

                temp.bspline.basis <- fda::create.bspline.basis(
                    rangeval    = base::range(base::as.integer(base::colnames(DF.temp.year))),
                    norder      = self$n.order,
                    nbasis      = self$n.basis,
                    dropind     = NULL,
                    quadvals    = NULL,
                    values      = NULL,
                    basisvalues = NULL,
                    names       = "bspl"
                    );

                temp.bspline.basis.fdParObj <- fda::fdPar(
                    fdobj  = temp.bspline.basis,
                    Lfdobj = NULL,
                    lambda = self$smoothing.parameter,
                    penmat = NULL
                    );

                # express target.variable as linear combinations
                # of the B-spline basis functions
                target.in.basis.fd <- fda::smooth.basis(
                    argvals      = base::as.integer(base::colnames(DF.temp.year)),
                    y            = t.DF.temp.year,
                    fdParobj     = temp.bspline.basis.fdParObj,
                    wtvec        = NULL,
                    fdnames      = NULL,
                    covariates   = NULL,
                    method       = "chol",
                    dfscale      = 1,
                    returnMatrix = FALSE
                    );

                # evaluate the B-spline approximations at grid points
                # of the across-year common grid
                bspline.approximation <- fda::eval.fd(
                    fdobj   = target.in.basis.fd[["fd"]],
                    evalarg = self$standardized.bspline.basis[["spline.grid"]]
                    );

                base::rownames(bspline.approximation) <- self$standardized.bspline.basis[["spline.grid"]];
                bspline.approximation <- base::as.data.frame(t(bspline.approximation));

                if ( 1 == base::nrow(bspline.approximation) ) {
                    base::rownames(bspline.approximation) <- base::rownames(DF.temp.year);
                    }

                bspline.approximation[,"location_year"] <- base::rownames(bspline.approximation);

                bspline.approximation.long <- bspline.approximation %>%
                    tidyr::gather(key = date_index, value = target_variable, -location_year);

                bspline.approximation.long <- base::as.data.frame(bspline.approximation.long);
                bspline.approximation.long[,"date_index"] <- base::as.numeric(bspline.approximation.long[,"date_index"]);
                base::colnames(bspline.approximation.long) <- base::gsub(
                    x           = base::colnames(bspline.approximation.long),
                    pattern     = "target_variable",
                    replacement = variable
                    );

                DF.stack <- base::rbind(DF.stack,bspline.approximation.long);

                }

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- base::unique(DF.input[,base::c(location,"year","location_year")]);
            DF.output[,"dummy"] <- 1;

            DF.date.indexes <- base::data.frame(
                date_index = self$standardized.bspline.basis[["spline.grid"]],
                dummy      = base::rep(1,base::length(self$standardized.bspline.basis[["spline.grid"]]))
                );

            DF.output <- dplyr::full_join(x = DF.output, y = DF.date.indexes, by = "dummy");
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),"dummy")];

            DF.output <- dplyr::left_join(
                x  = DF.output,
                y  = DF.stack,
                by = base::c("location_year","date_index")
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- DF.output[,base::c("location_year","year","date_index",variable)];

            base::colnames(DF.output) <- base::gsub(
                x           = base::colnames(DF.output),
                pattern     = variable,
                replacement = "target_variable"
                );

            DF.output <- DF.output %>%
                tidyr::spread(key = date_index, value = target_variable);
            DF.output <- base::as.data.frame(DF.output);
            base::rownames(DF.output) <- DF.output[,"location_year"];

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- DF.output[0 == base::rowSums(base::is.na(DF.output)),];

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            base::return( DF.output );

            }, # standardized.grid.interpolate()

        learn.fpca.parameters = function(
            DF.input  = NULL,
            visualize = FALSE
            ) {

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            t.DF.input <- base::t(DF.input[,base::as.character(self$standardized.bspline.basis[['spline.grid']])]);
            cat("\nstr(t.DF.input)\n");
            print( str(t.DF.input)   );

            # express standardized-grid time series as linear combinations
            # of the standardized-grid B-spline basis
            target.variable.fd <- fda::smooth.basis(
                argvals      = self$standardized.bspline.basis[['spline.grid']],
                y            = t.DF.input,
                fdParobj     = self$standardized.bspline.basis[['bspline.basis.fdParObj']],
                method       = 'cho1',
                dfscale      = 1,
                returnMatrix = FALSE
                );

            # perform FPCA computations on standardized-grid time time series
            # in terms of standardized-grid B-spline basis
            results.pca.fd <- fda::pca.fd(
                fdobj     = target.variable.fd[['fd']],
                nharm     = self$n.harmonics,
                harmfdPar = self$standardized.bspline.basis[['bspline.basis.fdParObj']]
                );

            training.row.means <- base::apply(
                X      = target.variable.fd[['fd']][['coefs']],
                MARGIN = 1,
                FUN    = function(x) { mean(x) }
                );

            if ( visualize == TRUE ) {

                private$visualize.bslpine.fit(
                    week.indices     = week.indices,
                    spline.grid      = seq(min(week.indices),max(week.indices),0.1),
                    t.DF.time.series = transposed.df.value,
                    time.series.fd   = time.series.fd,
                    prefix           = paste0("fit-",self$variable)
                    );

                private$visualize.fpca.fit(
                    week.indices     = week.indices,
                    spline.grid      = seq(min(week.indices),max(week.indices),0.1),
                    t.DF.time.series = transposed.df.value,
                    time.series.fd   = time.series.fd,
                    results.pca.fd   = results.pca.fd,
                    prefix           = paste0("fit-",self$variable)
                    );

                }

            base::return(
                base::list(
                    training.pca.fd    = results.pca.fd,
                    training.row.means = training.row.means,
                    harmonics          = results.pca.fd[["harmonics"]]
                    )
                );

            }, # learn.fpca.parameters()

        apply.fpca.parameters = function(
            DF.input  = NULL,
            visualize = FALSE
            ) {

            #t.DF.time.series <- t(DF.input);
            t.DF.input <- base::t(DF.input);

            cat('\nstr(t.DF.input)\n');
            print( str(t.DF.input)   );

            #time.series.fd <- fda::smooth.basis(
            t.DF.input.fd <- fda::smooth.basis(
                argvals      = self$standardized.bspline.basis[['spline.grid']],
                y            = t.DF.input, # t.DF.time.series,
                fdParobj     = self$standardized.bspline.basis[['bspline.basis.fdParObj']], #self$learned.fpca.parameters[["week.fdParObj"]],
                method       = 'cho1',
                dfscale      = 1,
                returnMatrix = FALSE
                );

            training.row.means <- self$learned.fpca.parameters[["training.row.means"]];
            temp.ncols         <- base::ncol(t.DF.input.fd[['fd']][['coefs']]);

            t.DF.input.fd.centered <- fda::fd(
                coef     = t.DF.input.fd[['fd']][['coefs']] - matrix(rep(training.row.means,temp.ncols),ncol=temp.ncols),
                basisobj = t.DF.input.fd[['fd']][['basis']],
                fdnames  = NULL
                );
            attr(t.DF.input.fd.centered[['coefs']],"dimnames") <- NULL;

            results.inprod <- fda::inprod(
                fdobj1 = t.DF.input.fd.centered,
                fdobj2 = self$learned.fpca.parameters[["training.pca.fd"]][["harmonics"]]
                );

            DF.fpc <- results.inprod;
            #colnames(DF.fpc)<- paste0(paste0('fpc_',variable.series,'_'),seq(1,ncol(DF.fpc)));
            colnames(DF.fpc) <- paste0('fpc_',seq(1,ncol(DF.fpc)));
            rownames(DF.fpc) <- rownames(DF.input);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            if ( visualize == TRUE ) {

                time.series.fd <- fda::smooth.basis(
                    argvals      = self$standardized.bspline.basis[['spline.grid']], #week.indices,
                    y            = t.DF.input, #t.DF.time.series,
                    fdParobj     = self$standardized.bspline.basis[['bspline.basis.fdParObj']], #self$learned.fpca.parameters[[variable.series]][["week.fdParObj"]],
                    method       = 'cho1',
                    dfscale      = 1,
                    returnMatrix = FALSE
                    );

                private$visualize.bslpine.fit(
                    week.indices     = self$standardized.bspline.basis[['spline.grid']], #week.indices,
                    spline.grid      = seq(min(week.indices),max(week.indices),0.1),
                    t.DF.time.series = t.DF.input, #t.DF.time.series,
                    time.series.fd   = time.series.fd,
                    prefix           = "transform"
                    );

                # private$visualize.fpca.fit(
                #     week.indices     = week.indices,
                #     spline.grid      = seq(min(week.indices),max(week.indices),0.1),
                #     t.DF.time.series = t.DF.time.series,
                #     time.series.fd   = time.series.fd,
                #     results.pca.fd   = results.pca.fd,
                #     prefix           = "transform"
                #     );

                }

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            return( DF.fpc );

            }, # apply.fpca.parameters()

        visualize.bslpine.fit =  function(
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
                lines(x = spline.grid,  y = time.series.bspline[,temp.column], type = "l", col = "red",   lwd = 1);
                dev.off();
                }

            return( NULL );

            }, # visualize.bslpine.fit()

        visualize.fpca.fit = function(
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

            # cat("\nstr(time.series.harmonics):\n");
            # print( str(time.series.harmonics)    );

            time.series.fpca.fit <- time.series.harmonics %*% t( results.pca.fd[["scores"]] );
            for ( j in seq(1,ncol(time.series.fpca.fit)) ) {
                time.series.fpca.fit[,j] <- time.series.fpca.fit[,j] + time.series.meanfd;
                }

            # cat("\nstr(time.series.fpca.fit):\n");
            # print( str(time.series.fpca.fit)    );

            temp.columns <- sample(x = seq(1,ncol(t.DF.time.series)), size = 10);
            for ( temp.column in temp.columns ) {
                temp.file <- paste0("plot-fpca-",prefix,"-",temp.column,".png");
                png(temp.file, height = 12, width = 18, units = "in", res = 300);
                plot( x = week.indices, y = t.DF.time.series[    ,temp.column], type = "b", col = "black",lwd = 2);
                lines(x = spline.grid,  y = time.series.bspline[ ,temp.column], type = "l", col = "red",  lwd = 1);
                lines(x = spline.grid,  y = time.series.fpca.fit[,temp.column], type = "l", col = "blue", lwd = 1);
                dev.off();
                }

            return( NULL );

            } # visualize.fpca.fit()

        ) # private = list()

    ) # R6Class()
