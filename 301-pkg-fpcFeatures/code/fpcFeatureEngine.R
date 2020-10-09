
fpcFeatureEngine = R6::R6Class(

	classname = 'fpcFeatureEngine',

	public = base::list(

		# instantiation parameters
		learner.metadata = NULL,
		training.data    = NULL,

		# class attributes
		weather.variable.series = c("sumfrostd", "avgprcnawhc", "sumchu", "sumpcpn", "avgsi", "sumheatd"),
		learned.fpca.parameters  = base::list(),

		initialize = function(
			learner.metadata = NULL,
			training.data    = NULL
			) {
			self$learner.metadata <- learner.metadata;
			self$training.data    <- training.data;
			# if ( is.null(self$learner.metadata[["fpca_weather"]][["variable.series"]]) ) {
			# 	self$weather.variable.series <- c("sumfrostd", "avgprcnawhc", "sumchu", "sumpcpn", "avgsi", "sumheatd");
			# 	} else {
			# 	self$weather.variable.series <- self$learner.metadata[["fpca_weather"]][["variable.series"]];
			# 	}
			},

		fit = function() {

			if ( self$learner.metadata[["fpca_ndvi"]][["transform"]] == TRUE ) {
				base::require(fda);
				private$learn.fpca.parameters(
					training.set        = self$training.data,
					variable.series     = 'ndvi',
					week.indices        = self$learner.metadata[["fpca_ndvi"]][["week.indices"]],
					n.harmonics         = self$learner.metadata[["fpca_ndvi"]][["n.harmonics"]],
					n.order             = self$learner.metadata[["fpca_ndvi"]][["n.order"]],
					n.basis             = self$learner.metadata[["fpca_ndvi"]][["n.basis"]],
					smoothing.parameter = self$learner.metadata[["fpca_ndvi"]][["smoothing.parameter"]],
					visualize           = self$learner.metadata[["fpca_ndvi"]][["visualize"]]
					);
				}

			if ( self$learner.metadata[["fpca_weather"]][["transform"]] == TRUE ) {
				require(fda);

				# if ( is.null(self$learner.metadata[["fpca_weather"]][["variable.series"]]) ) {
				# 	self$weather.variable.series <- c("sumfrostd", "avgprcnawhc", "sumchu", "sumpcpn", "avgsi", "sumheatd");
				# 	} else {
				# 	self$weather.variable.series <- self$learner.metadata[["fpca_weather"]][["variable.series"]];
				# 	}

				for ( variable.series in self$weather.variable.series ) {
					private$learn.fpca.parameters(
						training.set        = self$training.data,
						variable.series     = variable.series,
						week.indices        = self$learner.metadata[["fpca_weather"]][["week.indices"]],
						n.harmonics         = self$learner.metadata[["fpca_weather"]][["n.harmonics"]],
						n.order             = self$learner.metadata[["fpca_weather"]][["n.order"]],
						n.basis             = self$learner.metadata[["fpca_weather"]][["n.basis"]],
						smoothing.parameter = self$learner.metadata[["fpca_weather"]][["smoothing.parameter"]],
						visualize           = self$learner.metadata[["fpca_weather"]][["visualize"]]
						)
					}

				}

			}, # fit()

		transform = function(newdata = NULL) {

			DF.newdata           <- newdata;
			colnames(DF.newdata) <- tolower(colnames(DF.newdata));
			rownames(DF.newdata) <- sapply(
				X   = rownames(DF.newdata),
				FUN = function(x) { paste0(sample(x=letters,size=10,replace=TRUE),collapse="") }
				);

			DF.output <- as.data.frame(DF.newdata);

			if ( self$learner.metadata[["binarize_factors"]] == TRUE ) {
				# temp.matrix <- model.matrix(
				# 	object = ~ -1 + cropsurv + car16uid + ymecoreg,
				# 	data   = DF.output
				# 	);
				# DF.output <- cbind(
				# 	DF.output[,setdiff(colnames(DF.output),c("cropsurv","car16uid","ymecoreg"))],
				# 	temp.matrix
				# 	);
				factor.colnames <- colnames(DF.output)[sapply(X=DF.output,FUN=is.factor)];
				if ( 0 < length(factor.colnames) ) {
					formula.string  <- paste0( "~ -1 + " , paste( factor.colnames, collapse = " + " ));
					temp.matrix <- model.matrix(
						object = as.formula(formula.string),
						data   = DF.output
						);
					DF.output <- cbind(
						DF.output[,setdiff(colnames(DF.output),factor.colnames)],
						temp.matrix
						);
					}
				}

			if ( self$learner.metadata[["fpca_ndvi"]][["transform"]] == TRUE ) {
				print('Adding NDVI components...')

				time.series.colnames <- paste0("ndvi",self$learner.metadata[["fpca_ndvi"]][["week.indices"]]);
				DF.temp              <- DF.newdata[,time.series.colnames];

				DF.fpc <- private$apply.fpca.parameters(
					DF.input        = DF.temp,
					variable.series = 'ndvi',
					week.indices    = self$learner.metadata[["fpca_ndvi"]][["week.indices"]],
					visualize       = self$learner.metadata[["fpca_ndvi"]][["visualize"]]
					);

				DF.output <- cbind(
					DF.output,
					DF.fpc[rownames(DF.output),]
					);

				}

			if ( self$learner.metadata[["fpca_weather"]][["transform"]] == TRUE ) {

				for ( weather.variables in self$weather.variable.series ) {

					time.series.colnames <- paste0(weather.variables,self$learner.metadata[["fpca_weather"]][["week.indices"]]);
					DF.temp              <- DF.newdata[,time.series.colnames];

					DF.fpc <- private$apply.fpca.parameters(
						DF.input        = DF.temp,
						variable.series = weather.variables,
						week.indices    = self$learner.metadata[["fpca_weather"]][["week.indices"]],
						visualize       = self$learner.metadata[["fpca_weather"]][["visualize"]]
						);

					DF.output <- cbind(
						DF.output,
						DF.fpc[rownames(DF.output),]
						);

					}
				}

			#cat("\nstr(DF.output)\n");
			#print( str(DF.output)   );

			return( DF.output );

			} # transform()

		),

	private = list(

		learn.fpca.parameters = function(
			training.set        = NULL,
			variable.series     = NULL,
			week.indices        = NULL,
			n.harmonics         = NULL,
			n.order             = NULL,
			n.basis             = NULL,
			smoothing.parameter = NULL,
			visualize           = FALSE
			) {

				data              <- training.set;
				colnames(data)    <- tolower(colnames(data));
				value.colnames    <- paste0(variable.series,week.indices);
				working_dataframe <- data[,value.colnames];

				df <- working_dataframe;
				rownames(df) <- sapply(X = rownames(df), FUN = function(x){paste0(sample(x=letters,size = 10, replace = TRUE), collapse = "")})

				transposed.df.value <- t(df[,value.colnames])

				week.basis <- fda::create.bspline.basis(
					rangeval    = range(week.indices),
					norder      = n.order,
					nbasis      = n.basis,
					dropind     = NULL,
					quadvals    = NULL,
					values      = NULL,
					basisvalues = NULL,
					names       = 'bspl'
					);

				week.fdParObj <- fda::fdPar(
					fdobj  = week.basis,
					lambda = smoothing.parameter
					);

				time.series.fd <- fda::smooth.basis(
					argvals      = week.indices,
					y            = transposed.df.value,
					fdParobj     = week.fdParObj,
					method       = 'cho1',
					dfscale      = 1,
					returnMatrix = FALSE
					);

				results.pca.fd <- fda::pca.fd(
					fdobj     = time.series.fd[['fd']],
					nharm     = n.harmonics,
					harmfdPar = week.fdParObj
					);

			    training.row.means <- apply(
			        X      = time.series.fd[['fd']][['coefs']],
			        MARGIN = 1,
			        FUN    = function(x) { mean(x) }
			        );

				self$learned.fpca.parameters[[variable.series]] <- list(
					week.fdParObj      = week.fdParObj,
					training.row.means = training.row.means,
					harmonics          = results.pca.fd[["harmonics"]]
					);

				if ( visualize == TRUE ) {

					private$visualize.bslpine.fit(
		    			week.indices     = week.indices,
		    			spline.grid      = seq(min(week.indices),max(week.indices),0.1),
		    			t.DF.time.series = transposed.df.value,
		    			time.series.fd   = time.series.fd,
		    			prefix           = paste0("fit-",variable.series)
		    			);

					private$visualize.fpca.fit(
						week.indices     = week.indices,
						spline.grid      = seq(min(week.indices),max(week.indices),0.1),
						t.DF.time.series = transposed.df.value,
						time.series.fd   = time.series.fd,
						results.pca.fd   = results.pca.fd,
						prefix           = paste0("fit-",variable.series)
						);

					}

				}, # learn.fpca.parameters()

		apply.fpca.parameters = function(
			DF.input        = NULL,
			variable.series = NULL,
			week.indices    = NULL,
			visualize       = FALSE
			) {

				t.DF.time.series <- t(DF.input);

				time.series.fd <- fda::smooth.basis(
					argvals      = week.indices,
					y            = t.DF.time.series,
					fdParobj     = self$learned.fpca.parameters[[variable.series]][["week.fdParObj"]],
					method       = 'cho1',
					dfscale      = 1,
					returnMatrix = FALSE
					);

				training.row.means <- self$learned.fpca.parameters[[variable.series]][["training.row.means"]];
				temp.ncols         <- ncol(time.series.fd[['fd']][['coefs']]);

				time.series.fd.centered <- fd(
					coef     = time.series.fd[['fd']][['coefs']] - matrix(rep(training.row.means,temp.ncols),ncol=temp.ncols),
					basisobj = time.series.fd[['fd']][['basis']],
					fdnames  = NULL
					);
				attr(time.series.fd.centered[['coefs']],"dimnames") <- NULL;

			    results.inprod <- fda::inprod(
			    	fdobj1 = time.series.fd.centered,
			    	fdobj2 = self$learned.fpca.parameters[[variable.series]][["harmonics"]]
			    	);

			    DF.fpc <- results.inprod;
			    colnames(DF.fpc) <- paste0(paste0('fpc_',variable.series,'_'),seq(1,ncol(DF.fpc)));
			    rownames(DF.fpc) <- rownames(DF.input);

			    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
				if ( visualize == TRUE ) {

					time.series.fd <- fda::smooth.basis(
						argvals      = week.indices,
						y            = t.DF.time.series,
						fdParobj     = self$learned.fpca.parameters[[variable.series]][["week.fdParObj"]],
						method       = 'cho1',
						dfscale      = 1,
						returnMatrix = FALSE
						);

					private$visualize.bslpine.fit(
		    			week.indices     = week.indices,
		    			spline.grid      = seq(min(week.indices),max(week.indices),0.1),
		    			t.DF.time.series = t.DF.time.series,
		    			time.series.fd   = time.series.fd,
		    			prefix           = paste0("transform-",variable.series)
		    			);

					# private$visualize.fpca.fit(
					# 	week.indices     = week.indices,
					# 	spline.grid      = seq(min(week.indices),max(week.indices),0.1),
					# 	t.DF.time.series = t.DF.time.series,
					# 	time.series.fd   = time.series.fd,
					# 	results.pca.fd   = results.pca.fd,
					# 	prefix           = paste0("transform-",variable.series)
					# 	);

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

				#cat("\nstr(time.series.harmonics):\n");
				#print( str(time.series.harmonics)    );

				time.series.fpca.fit <- time.series.harmonics %*% t( results.pca.fd[["scores"]] );
				for ( j in seq(1,ncol(time.series.fpca.fit)) ) {
					time.series.fpca.fit[,j] <- time.series.fpca.fit[,j] + time.series.meanfd;
					}

				#cat("\nstr(time.series.fpca.fit):\n");
				#print( str(time.series.fpca.fit)    );

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
