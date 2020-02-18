
getDataStandardizedTimepoints <- function(
    DF.input            = NULL,
    beam.swath          = NULL,
    colname.pattern     = NULL,
    n.partition         = NULL,
    n.order             = NULL,
    n.basis             = NULL,
    smoothing.parameter = NULL,
    n.harmonics         = NULL,
    RData.output        = paste0("data-",beam.swath,"-standardizedTimepoints.RData")
    ) {

    this.function.name <- "getDataStandardizedTimepoints";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(RData.output)) {
        cat(paste0("\nloading file: ",RData.output,"\n"));
        DF.output <- base::readRDS(file = RData.output);
        cat(paste0("\nexiting: ",this.function.name,"()"));
        cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
        return( DF.output );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(dplyr);
    require(tidyr);
    require(fda);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(DF.input) -- getDataStandardizedTimepoints()\n");
    print( str(DF.input)   );

    list.bspline.basis <- getDataStandardizedTimepoints_getBsplineBasis(
        DF.input            = DF.input,
        beam.swath          = beam.swath,
        n.partition         = n.partition,
        n.order             = n.order,
        n.basis             = n.basis,
        smoothing.parameter = smoothing.parameter
        );

    cat("\nstr(list.bspline.basis)\n");
    print( str(list.bspline.basis)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    target.variables <- grep(
        x       = colnames(DF.input),
	pattern = colname.pattern,
	value   = TRUE
	);

    DF.output <- getDataStandardizedTimepoints_initializeOutput(
        DF.input           = DF.input,
	list.bspline.basis = list.bspline.basis
        );

    for ( target.variable in target.variables ) {
        DF.output <- getDataStandardizedTimepoints_attachVariable(
            DF.input            = DF.input,
	    DF.current          = DF.output,
            target.variable     = target.variable,
            n.order             = n.order,
            n.basis             = n.basis,
            smoothing.parameter = smoothing.parameter,
	    list.bspline.basis  = list.bspline.basis
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
getDataStandardizedTimepoints_attachVariable <- function(
    DF.input            = NULL,
    DF.current          = NULL,
    target.variable     = NULL,
    n.order             = NULL,
    n.basis             = NULL,
    smoothing.parameter = NULL,
    list.bspline.basis  = NULL
    ) {

    require(fda);
    require(tidyr);

    DF.temp <- DF.input[,c("X_Y_year","year","date_index",target.variable)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = target.variable,
        replacement = "target_variable"
        );

    DF.stack <- data.frame();
    years    <- unique(DF.input[,"year"]);
    for ( year in years ) {

        DF.temp.year <- DF.temp[DF.temp[,"year"] == year,] %>%
            tidyr::spread(key = date_index, value = target_variable);
        DF.temp.year <- as.data.frame(DF.temp.year);
        rownames(DF.temp.year) <- DF.temp.year[,"X_Y_year"];
	DF.temp.year <- DF.temp.year[,setdiff(colnames(DF.temp.year),c("X_Y_year","year"))];
        DF.temp.year <- DF.temp.year[0 == rowSums(is.na(DF.temp.year)),];

        # cat("\nstr(DF.temp.year)\n");
	# print( str(DF.temp.year)   );

        t.DF.temp.year <- t(DF.temp.year);

        # cat("\nstr(t.DF.temp.year)\n");
	# print( str(t.DF.temp.year)   );

        temp.bspline.basis <- fda::create.bspline.basis(
            rangeval    = range(as.integer(colnames(DF.temp.year))),
            norder      = n.order,
            nbasis      = n.basis,
            dropind     = NULL,
            quadvals    = NULL,
            values      = NULL,
            basisvalues = NULL,
            names       = "bspl"
            );

        temp.bspline.basis.fdParObj <- fda::fdPar(
            fdobj  = temp.bspline.basis,
            Lfdobj = NULL,
            lambda = smoothing.parameter,
            penmat = NULL
            );

        # express target.variable as linear combinations
        # of the B-spline basis functions
        target.in.basis.fd <- fda::smooth.basis(
            argvals      = as.integer(colnames(DF.temp.year)),
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
            evalarg = list.bspline.basis[["spline.grid"]]
            );

	rownames(bspline.approximation) <- list.bspline.basis[["spline.grid"]];
        bspline.approximation <- as.data.frame(t(bspline.approximation));
        bspline.approximation[,"X_Y_year"] <- rownames(bspline.approximation);

        # cat("\nstr(bspline.approximation)\n");
	# print( str(bspline.approximation)   );

        bspline.approximation.long <- bspline.approximation %>%
            tidyr::gather(key = date_index, value = target_variable, -X_Y_year);

        bspline.approximation.long <- as.data.frame(bspline.approximation.long);
        bspline.approximation.long[,"date_index"] <- as.numeric(bspline.approximation.long[,"date_index"]);
        colnames(bspline.approximation.long) <- gsub(
            x           = colnames(bspline.approximation.long),
            pattern     = "target_variable",
            replacement = target.variable
            );

        # cat("\nstr(bspline.approximation.long)\n");
        # print( str(bspline.approximation.long)   );

        DF.stack <- rbind(DF.stack,bspline.approximation.long);

        }

    DF.output <- dplyr::left_join(
        x  = DF.current,
        y  = DF.stack,
        by = c("X_Y_year","date_index")
        );

    return( DF.output );

    }

getDataStandardizedTimepoints_initializeOutput <- function(
    DF.input           = NULL,
    list.bspline.basis = NULL
    ) {

    require(dplyr);

    DF.output <- unique(DF.input[,c("X","Y","year","type","X_Y_year")]);
    DF.output[,"dummy"] <- 1;

    DF.date.indexes <- data.frame(
        date_index = list.bspline.basis[["spline.grid"]],
	dummy      = rep(1,length(list.bspline.basis[["spline.grid"]]))
        );

    DF.output <- dplyr::full_join(x = DF.output, y = DF.date.indexes, by = "dummy");
    DF.output <- DF.output[,setdiff(colnames(DF.output),"dummy")];

    return( DF.output );

    }

getDataStandardizedTimepoints_getBsplineBasis <- function(
    DF.input            = NULL,
    beam.swath          = NULL,
    n.partition         = NULL,
    n.order             = NULL,
    n.basis             = NULL,
    smoothing.parameter = NULL
    ) {

    require(dplyr);
    require(fda);

    DF.yearly.endpoints <- unique(DF.input[,c("year","date","date_index")]) %>%
       dplyr::group_by(year) %>%
       dplyr::mutate( start_index = min(date_index), end_index = max(date_index) );
    DF.yearly.endpoints <- as.data.frame(DF.yearly.endpoints);

    cat("\nDF.yearly.endpoints\n");
    print( DF.yearly.endpoints   );

    write.csv(
        file      = paste0("diagnostics-timepoints-",beam.swath,".csv"),
        x         = DF.yearly.endpoints,
	row.names = FALSE
        );

    common.start.index <- max(DF.yearly.endpoints[,"start_index"]);
    common.end.index   <- min(DF.yearly.endpoints[,  "end_index"]);

    step.size <- round( (common.end.index - common.start.index) / n.partition );
    spline.grid <- seq(common.start.index, common.end.index, step.size);

    my.bspline.basis <- fda::create.bspline.basis(
        rangeval    = range(spline.grid),
        norder      = n.order,
        nbasis      = n.basis,
        dropind     = NULL,
        quadvals    = NULL,
        values      = NULL,
        basisvalues = NULL,
        names       = "bspl"
        );

    my.bspline.basis.fdParObj <- fda::fdPar(
        fdobj  = my.bspline.basis,
        Lfdobj = NULL,
        lambda = smoothing.parameter,
        penmat = NULL
        );

    list.output <- list(
        start.end.indexes      = c(common.start.index,common.end.index),
	n.partition            = n.partition,
	step.size              = step.size,
        spline.grid.range      = range(spline.grid),
        spline.grid            = spline.grid,
	bspline.basis          = my.bspline.basis,
	bspline.basis.fdParObj = my.bspline.basis.fdParObj
        );

    return( list.output );

    }

