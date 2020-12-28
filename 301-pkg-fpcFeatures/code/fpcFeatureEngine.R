
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
        min.date            = NULL,
        max.date            = NULL,
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
        #' @param min.date min.date
        #' @param max.date max.date
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
            min.date            = NULL,
            max.date            = NULL,
            n.partition         = 100,
            n.order             =   3,
            n.basis             =   9,
            smoothing.parameter =   0.1,
            n.harmonics         =   7
            ) {

            private$input.validity.checks(
                learner.metadata    = learner.metadata,
                training.data       = training.data,
                location            = location,
                date                = date,
                variable            = variable,
                min.date            = min.date,
                max.date            = max.date,
                n.partition         = n.partition,
                n.order             = n.order,
                n.basis             = n.basis,
                smoothing.parameter = smoothing.parameter,
                n.harmonics         = n.harmonics
                );

            self$learner.metadata    <- learner.metadata;
            self$training.data       <- training.data;
            self$location            <- location;
            self$date                <- date;
            self$variable            <- variable;
            self$min.date            <- min.date;
            self$max.date            <- max.date;
            self$n.partition         <- n.partition;
            self$n.order             <- n.order;
            self$n.basis             <- n.basis;
            self$smoothing.parameter <- smoothing.parameter;
            self$n.harmonics         <- n.harmonics;

            },

        #' @description
        #' learn from training data (compute functional principal components).
        #' @param log.threshold log threshold. Must be one of the log levels supported by the \code{logger} package. Default: logger::log_threshold()
        #' @return NULL
        fit = function(
            log.threshold = logger::log_threshold()
            ) {

            log.threshold.original <- logger::log_threshold();
            logger::log_threshold(level = log.threshold);

            DF.temp <- private$add.auxiliary.columns(DF.input = self$training.data);
            logger::log_debug('str(DF.temp):\n{paste0(capture.output(str(DF.temp)),collapse="\n")}');

            self$standardized.bspline.basis <- private$get.standardized.bspline.basis(
                DF.input = DF.temp,
                min.date = self$min.date,
                max.date = self$max.date
                );
            logger::log_debug('str(self$standardized.bspline.basis):\n{paste0(capture.output(str(self$standardized.bspline.basis)),collapse="\n")}');

            DF.temp <- private$standardized.grid.interpolate(
                DF.input = DF.temp
                );
            logger::log_debug('str(DF.temp):\n{paste0(capture.output(str(DF.temp)),collapse="\n")}');

            self$learned.fpca.parameters <- private$learn.fpca.parameters(DF.input = DF.temp);
            logger::log_debug('str(self$learned.fpca.parameters):\n{paste0(capture.output(str(self$learned.fpca.parameters)),collapse="\n")}');

            logger::log_threshold(level = log.threshold.original);
            base::return( NULL );

            }, # fit()

        #' @description
        #' compute functional principal component scores for new data with respect to functional principale components of training data.
        #' @param newdata newdata
        #' @param location location
        #' @param date date
        #' @param variable variable
        #' @param log.threshold log threshold. Must be one of the log levels supported by the \code{logger} package. Default: logger::log_threshold()
        #' @return matrix of FPC scores
        transform = function(
            newdata       = NULL,
            location      = NULL,
            date          = NULL,
            variable      = NULL,
            log.threshold = logger::log_threshold()
            ) {

            log.threshold.original <- logger::log_threshold();
            logger::log_threshold(level = log.threshold);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- private$add.auxiliary.columns(
                DF.input = newdata,
                location = location,
                date     = date
                );
            logger::log_debug('str(DF.temp):\n{paste0(capture.output(str(DF.temp)),collapse="\n")}');

            DF.dictionary <- unique(DF.temp[,c('location_year',location,'year')]);
            logger::log_debug('str(DF.dictionary):\n{paste0(capture.output(str(DF.dictionary)),collapse="\n")}');

            DF.newdata.standardized.wide <- private$standardized.grid.interpolate(
                DF.input = DF.temp,
                location = location,
                variable = variable
                );
            base::rownames(DF.newdata.standardized.wide) <- base::sapply(
                X   = base::rownames(DF.newdata.standardized.wide),
                FUN = function(x) { base::paste0(base::sample(x=letters,size=20,replace=TRUE),collapse="") }
                );
            logger::log_debug('str(DF.newdata.standardized.wide):\n{paste0(capture.output(str(DF.newdata.standardized.wide)),collapse="\n")}');

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
                y     = DF.dictionary[,base::c('location_year',location)],
                by    = "location_year",
                all.x = TRUE
                );
            base::rownames(DF.output) <- DF.output[,'location_year'];

            logger::log_debug('str(DF.output):\n{paste0(capture.output(str(DF.output)),collapse="\n")}');

            colnames.to.delete <- base::c('location_year',location,'year');
            DF.output <- DF.output[,base::c(location,'year',base::setdiff(base::colnames(DF.output),colnames.to.delete))];

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_threshold(level = log.threshold.original);
            return( DF.output );

            }, # transform()

        #' @description
        #' plot input time series, its B-spline approximnation and its FPcA approximations
        #' @param DF.input DF.input
        #' @param location location
        #' @param date date
        #' @param variable variable
        #' @param log.threshold log threshold. Must be one of the log levels supported by the \code{logger} package. Default: logger::log_threshold()
        #' @return plot of bslpine and FPCA approximations
        plot.approximations = function(
            DF.input      = NULL,
            location      = NULL,
            date          = NULL,
            variable      = NULL,
            log.threshold = logger::log_threshold()
            ) {

            log.threshold.original <- logger::log_threshold();
            logger::log_threshold(level = log.threshold);

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
            logger::log_debug('str(DF.temp):\n{paste0(capture.output(str(DF.temp)),collapse="\n")}');

            DF.newdata.standardized.wide <- private$standardized.grid.interpolate(
                DF.input = DF.temp,
                location = location,
                variable = variable
                );
            base::rownames(DF.newdata.standardized.wide) <- base::sapply(
                X   = base::rownames(DF.newdata.standardized.wide),
                FUN = function(x) { base::paste0(base::sample(x=letters,size=20,replace=TRUE),collapse="") }
                );
            logger::log_debug('str(DF.newdata.standardized.wide):\n{paste0(capture.output(str(DF.newdata.standardized.wide)),collapse="\n")}');

            DF.fpc <- private$apply.fpca.parameters(
                DF.input  = DF.newdata.standardized.wide[,base::setdiff(base::colnames(DF.newdata.standardized.wide),base::c('location_year','year'))],
                visualize = FALSE
                );
            base::rownames(DF.fpc) <- DF.newdata.standardized.wide[,'location_year'];
            logger::log_debug('str(DF.fpc):\n{paste0(capture.output(str(DF.fpc)),collapse="\n")}');

            vector.meanfd <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["meanfd"]]
                );

            DF.fpca.standardizedTimepoints <- fda::eval.fd(
                evalarg = temp.evalarg,
                fdobj   = self$learned.fpca.parameters[["training.pca.fd"]][["harmonics"]]
                );

            DF.fpca.fit <- DF.fpca.standardizedTimepoints %*% t( DF.fpc );
            for ( j in seq(1,ncol(DF.fpca.fit)) ) {
                DF.fpca.fit[,j] <- DF.fpca.fit[,j] + vector.meanfd;
                }
            DF.fpca.fit <- cbind("date_index" = temp.evalarg, DF.fpca.fit);
            colnames(DF.fpca.fit) <- gsub(x = colnames(DF.fpca.fit), pattern = temp.location_year, replacement = "dummy_colname");
            DF.fpca.fit <- as.data.frame(DF.fpca.fit)

            logger::log_debug('str(DF.fpca.fit):\n{paste0(capture.output(str(DF.fpca.fit)),collapse="\n")}');

            my.ggplot <- my.ggplot + ggplot2::geom_line(
                data    = DF.fpca.fit,
                mapping = ggplot2::aes(x = date_index, y = dummy_colname),
                colour  = "red",
                size    = 1.3,
                alpha   = 0.8
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_threshold(level = log.threshold.original);
            return( my.ggplot );

            }, # plot.approximations()

        #' @description
        #' plot the harmonics (functional principal components) computed based on the training data.
        #' @param log.threshold log threshold. Must be one of the log levels supported by the \code{logger} package. Default: logger::log_threshold()
        #' @return plot of FPCA harmonics
        plot.harmonics = function(
            log.threshold = logger::log_threshold()
            ) {

            log.threshold.original <- logger::log_threshold();
            logger::log_threshold(level = log.threshold);

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
            logger::log_threshold(level = log.threshold.original);
            base::return( my.cowplot );

            } # plot.harmonics()

        ), # public = base::list()

    private = base::list(

        add.auxiliary.columns = function(
            DF.input = self$training.data,
            date     = self$date,
            location = self$location
            ) {
            this.function.name <- "add.auxiliary.columns";
            logger::log_debug('{this.function.name}(): starts');
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- DF.input;
            DF.output[,'year'] <- base::as.character(base::format(x = DF.output[,date], format = "%Y"));
            DF.output[,'location_year'] <- base::apply(
                X      = DF.output[,c(location,'year')],
                MARGIN = 1,
                FUN    = function(x) {base::return(base::paste(x,collapse="_"))}
                );
            DF.output[,"new_year_day"] <- base::as.Date(base::paste0(DF.output[,"year"],"-01-01"));
            DF.output[,"date_index"]   <- base::as.integer(DF.output[,date]) - base::as.integer(DF.output[,"new_year_day"]) + 1;
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_debug('{this.function.name}(): exits');
            base::return( DF.output );
            },

        get.standardized.bspline.basis = function(
            DF.input = NULL,
            min.date = NULL,
            max.date = NULL
            ) {

            this.function.name <- "get.standardized.bspline.basis";
            logger::log_debug('{this.function.name}(): starts');
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

            DF.yearly.endpoints <- base::unique(DF.input[,c("year",self$date,"date_index")]) %>%
                dplyr::group_by(year) %>%
                dplyr::mutate( start_index = min(date_index), end_index = max(date_index) );
            DF.yearly.endpoints <- base::as.data.frame(DF.yearly.endpoints);
            DF.yearly.endpoints <- DF.yearly.endpoints[base::order(DF.yearly.endpoints[,self$date]),];

            common.start.index <- base::max(DF.yearly.endpoints[,"start_index"]);
            if ( !base::is.null(min.date) ) {
                min.date.year      <- base::as.character(base::format(x = min.date, format = "%Y"));
                new.year.day       <- base::as.Date(base::paste0(min.date.year,"-01-01"));
                min.date.index     <- base::as.integer(min.date) - base::as.integer(new.year.day) + 1;
                common.start.index <- base::max(common.start.index,min.date.index);
                }

            common.end.index   <- base::min(DF.yearly.endpoints[,  "end_index"]);
            if ( !base::is.null(max.date) ) {
                max.date.year    <- base::as.character(base::format(x = max.date, format = "%Y"));
                new.year.day     <- base::as.Date(base::paste0(max.date.year,"-01-01"));
                max.date.index   <- base::as.integer(max.date) - base::as.integer(new.year.day) + 1;
                common.end.index <- base::min(common.end.index,max.date.index);
                }

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

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_debug('{this.function.name}(): exits');
            base::return( list.output );

            }, # get.standardized.bspline.basis()

        standardized.grid.interpolate = function(
            DF.input = NULL,
            location = self$location,
            variable = self$variable
            ) {

            this.function.name <- "standardized.grid.interpolate";
            logger::log_debug('{this.function.name}(): starts');
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

            logger::log_debug('{this.function.name}(): str(DF.input):\n{paste0(capture.output(str(DF.input)),collapse="\n")}');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.input[,c("location_year","year","date_index",variable)];
            colnames(DF.temp) <- gsub(
                x           = colnames(DF.temp),
                pattern     = variable,
                replacement = "target_variable"
                );

            logger::log_debug('{this.function.name}(): str(DF.temp):\n{paste0(capture.output(str(DF.temp)),collapse="\n")}');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.stack <- base::data.frame();

            years <- base::unique(DF.input[,"year"]);
            for ( year in years ) {

                logger::log_debug('{this.function.name}(): variable = {variable}, year = {year}');

                DF.temp.year <- DF.temp[DF.temp[,"year"] == year,] %>%
                    tidyr::spread(key = date_index, value = target_variable);
                DF.temp.year <- base::as.data.frame(DF.temp.year);
                base::rownames(DF.temp.year) <- DF.temp.year[,"location_year"];

                DF.temp.year <- DF.temp.year[,base::setdiff(base::colnames(DF.temp.year),base::c("location_year","year"))];
                logger::log_debug('{this.function.name}(): str(DF.temp.year):\n{paste0(capture.output(str(DF.temp.year)),collapse="\n")}');

                DF.temp.year <- DF.temp.year[0 == base::rowSums(base::is.na(DF.temp.year)),];
                logger::log_debug('{this.function.name}(): str(DF.temp.year):\n{paste0(capture.output(str(DF.temp.year)),collapse="\n")}');

                t.DF.temp.year <- t(DF.temp.year);
                logger::log_debug('{this.function.name}(): str(t.DF.temp.year):\n{paste0(capture.output(str(t.DF.temp.year)),collapse="\n")}');

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

                logger::log_debug('{this.function.name}(): base::as.integer(base::colnames(DF.temp.year)):\n{paste(capture.output(base::as.integer(base::colnames(DF.temp.year))),collapse="\n")}');
                logger::log_debug('{this.function.name}(): self$standardized.bspline.basis[["spline.grid"]]:\n{paste(capture.output(self$standardized.bspline.basis[["spline.grid"]]),collapse="\n")}');

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
            logger::log_debug('{this.function.name}(): exits');
            base::return( DF.output );

            }, # standardized.grid.interpolate()

        learn.fpca.parameters = function(
            DF.input  = NULL,
            visualize = FALSE
            ) {

            this.function.name <- "learn.fpca.parameters";
            logger::log_debug('{this.function.name}(): starts');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            t.DF.input <- base::t(DF.input[,base::as.character(self$standardized.bspline.basis[['spline.grid']])]);
            logger::log_debug('{this.function.name}(): str(t.DF.input):\n{paste0(capture.output(str(t.DF.input)),collapse="\n")}');

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

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_debug('{this.function.name}(): exits');
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

            this.function.name <- "apply.fpca.parameters";
            logger::log_debug('{this.function.name}(): starts');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            t.DF.input <- base::t(DF.input);
            logger::log_debug('{this.function.name}(): str(t.DF.input):\n{paste0(capture.output(str(t.DF.input)),collapse="\n")}');

            t.DF.input.fd <- fda::smooth.basis(
                argvals      = self$standardized.bspline.basis[['spline.grid']],
                y            = t.DF.input,
                fdParobj     = self$standardized.bspline.basis[['bspline.basis.fdParObj']],
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
            colnames(DF.fpc) <- paste0('fpc_',seq(1,ncol(DF.fpc)));
            rownames(DF.fpc) <- rownames(DF.input);

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_debug('{this.function.name}(): exits');
            return( DF.fpc );

            }, # apply.fpca.parameters()

        input.validity.checks = function(
            learner.metadata    = NULL,
            training.data       = NULL,
            location            = NULL,
            date                = NULL,
            variable            = NULL,
            min.date            = NULL,
            max.date            = NULL,
            n.partition         = NULL,
            n.order             = NULL,
            n.basis             = NULL,
            smoothing.parameter = NULL,
            n.harmonics         = NULL
            ) {

            private$input.validity.checks_data.structure(
                DF.input = training.data,
                location = location,
                date     = date,
                variable = variable
                );

            }, # input.validity.checks()

        input.validity.checks_data.structure = function(
            DF.input = NULL,
            location = NULL,
            date     = NULL,
            variable = NULL
            ) {

            base::stopifnot(
                base::identical( base::class(DF.input) , "data.frame" ),
                base::all( base::c(location,date,variable) %in% base::colnames(DF.input) ),
                base::identical( base::class( DF.input[,location] ) , "character" ),
                base::identical( base::class( DF.input[,date    ] ) , "Date"      ),
                base::identical( base::class( DF.input[,variable] ) , "numeric"   )
                );

            } # input.validity.checks_data.structure()

        ) # private = list()

    ) # R6Class()
