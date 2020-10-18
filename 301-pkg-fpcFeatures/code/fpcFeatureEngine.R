
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
#' @importFrom R6 R6Class
#' @import dplyr fda ggplot2 logger
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

            DF.newdata.standardized.wide <- private$standardized.grid.interpolate(
                DF.input = DF.temp
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

            } # transform()

        ), # public = base::list()

    private = base::list(

        add.auxiliary.columns = function(DF.input = self$training.data) {
            DF.output <- DF.input;
            DF.output[,'year'] <- base::as.character(base::format(x = DF.output[,self$date], format = "%Y"));
            DF.output[,'location_year'] <- base::apply(
                X      = DF.output[,c(self$location,'year')],
                MARGIN = 1,
                FUN    = function(x) {base::return(base::paste(x,collapse="_"))}
                );
            DF.output[,"new_year_day"] <- base::as.Date(base::paste0(DF.output[,"year"],"-01-01"));
            DF.output[,"date_index"]   <- base::as.integer(DF.output[,self$date]) - base::as.integer(DF.output[,"new_year_day"]);
            base::return( DF.output );
            },

        get.standardized.bspline.basis = function(DF.input = NULL) {

            DF.yearly.endpoints <- base::unique(DF.input[,c("year","date","date_index")]) %>%
                dplyr::group_by(year) %>%
                dplyr::mutate( start_index = min(date_index), end_index = max(date_index) );
            DF.yearly.endpoints <- base::as.data.frame(DF.yearly.endpoints);
            DF.yearly.endpoints <- DF.yearly.endpoints[base::order(DF.yearly.endpoints[,"date"]),];

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
            DF.input = NULL
            ) {

            cat("\nstandardized.grid.interpolate()\n");

            cat("\nstr(DF.input)\n");
            print( str(DF.input)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.temp <- DF.input[,c("location_year","year","date_index",self$variable)];
            colnames(DF.temp) <- gsub(
                x           = colnames(DF.temp),
                pattern     = self$variable,
                replacement = "target_variable"
                );

            cat("\nstr(DF.temp)\n");
            print( str(DF.temp)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.stack      <- base::data.frame();

            years <- base::unique(DF.input[,"year"]);
            for ( year in years ) {

                cat(paste0("\nstandized.grid.interpolate() -- ",self$variable,", ",year,"\n"));

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

                rownames(bspline.approximation) <- self$standardized.bspline.basis[["spline.grid"]];
                bspline.approximation <- as.data.frame(t(bspline.approximation));
                bspline.approximation[,"location_year"] <- rownames(bspline.approximation);

                cat("\nstr(bspline.approximation)\n");
                print( str(bspline.approximation)   );

                bspline.approximation.long <- bspline.approximation %>%
                    tidyr::gather(key = date_index, value = target_variable, -location_year);

                bspline.approximation.long <- base::as.data.frame(bspline.approximation.long);
                bspline.approximation.long[,"date_index"] <- base::as.numeric(bspline.approximation.long[,"date_index"]);
                base::colnames(bspline.approximation.long) <- base::gsub(
                    x           = base::colnames(bspline.approximation.long),
                    pattern     = "target_variable",
                    replacement = self$variable
                    );

                cat("\nstr(bspline.approximation.long)\n");
                print( str(bspline.approximation.long)   );

                DF.stack <- base::rbind(DF.stack,bspline.approximation.long);

                }

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- base::unique(DF.input[,c(self$location,"year","location_year")]);
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
                by = c("location_year","date_index")
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- DF.output[,c("location_year","year","date_index",self$variable)];
            colnames(DF.output) <- gsub(
                x           = colnames(DF.output),
                pattern     = self$variable,
                replacement = "target_variable"
                );

            DF.output <- DF.output %>%
                tidyr::spread(key = date_index, value = target_variable);
            DF.output <- as.data.frame(DF.output);
            rownames(DF.output) <- DF.output[,"location_year"];

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.output <- DF.output[0 == rowSums(is.na(DF.output)),];

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            # base::return( LIST.output );
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
                fdobj2 = self$learned.fpca.parameters[["harmonics"]]
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
