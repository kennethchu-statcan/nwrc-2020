
visualize.geocoordinates <- function(
    data.directory = NULL,
    selected.rows  = NULL,
    selected.cols  = NULL
    ) {

    thisFunctionName <- "visualize.geocoordinates";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nlist.files(data.directory)\n");
    print( list.files(data.directory)   );

    years <- list.files(data.directory);
    for ( temp.year in years ) {

        DF.geocoordinates <- visualize.geocoordinates_get.geocoordinates(
            data.directory = file.path(data.directory,temp.year),
            selected.rows  = selected.rows,
            selected.cols  = selected.cols
            );

        cat("\nstr(DF.geocoordinates)\n");
        print( str(DF.geocoordinates)   );

        visualize.geocoordinates_plot.geocoordinates(
            DF.input   = DF.geocoordinates,
            year       = temp.year,
            PNG.output = paste0("plot-geocooridnates-",temp.year,".png")
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
visualize.geocoordinates_get.geocoordinates <- function(
    data.directory = NULL,
    selected.rows  = NULL,
    selected.cols  = NULL
    ) {

    files.lat.lon <- list.files(data.directory, pattern = "(Lat|Lon)\\.csv");

    temp.dates <- unique(gsub(
        x           = files.lat.lon,
        pattern     = "[^0-9]",
        replacement = ""
        ));

    DF.output <- data.frame();
    for ( temp.date in temp.dates ) {

        file.lon <- grep(x = files.lat.lon, pattern = paste0(temp.date,".+Lon"), value = TRUE);
        DF.lon   <- read.csv(
            file   = file.path(data.directory,file.lon),
            header = FALSE
            );

        file.lat <- grep(x = files.lat.lon, pattern = paste0(temp.date,".+Lat"), value = TRUE);
        DF.lat   <- read.csv(
            file   = file.path(data.directory,file.lat),
            header = FALSE
            );

        if ( is.null(selected.rows) ) { selected.rows <- seq(1,nrow(DF.lon)) }
        if ( is.null(selected.rows) ) { selected.cols <- seq(1,ncol(DF.lon)) }

        DF.lon <- DF.lon[selected.rows,selected.cols];
        DF.lat <- DF.lat[selected.rows,selected.cols];

        matrix.all.ones  <- matrix(rep(x=1,times=nrow(DF.lon)*ncol(DF.lon)),nrow=nrow(DF.lon));
        matrix.row.index <- diag(seq(1,nrow(matrix.all.ones))) %*% matrix.all.ones;
        matrix.col.index <- matrix.all.ones %*% diag(seq(1,ncol(matrix.all.ones)));

        DF.temp <- data.frame(
            lon       = as.vector(as.matrix(DF.lon)),
            lat       = as.vector(as.matrix(DF.lat)),
            row.index = as.vector(matrix.row.index),
            col.index = as.vector(matrix.col.index)
            );
        DF.temp[,'date'] <- rep(as.Date(temp.date,format="%Y%m%d"),nrow(DF.temp));
        DF.temp <- DF.temp[,c('date','row.index','col.index','lat','lon')];

        DF.output <- rbind(DF.output,DF.temp);

        }

    return( DF.output );

    }

visualize.geocoordinates_plot.geocoordinates <- function(
    DF.input   = NULL,
    year       = NULL,
    PNG.output = NULL
    ){

    my.ggplot <- initializePlot();

    my.ggplot <- my.ggplot + ggplot2::ggtitle(
        label    = NULL,
        subtitle = year
        );

    DF.input[,'date']     <- as.character(DF.input[,'date']);
    DF.input[,'parity']   <- as.character((DF.input[,'row.index'] + DF.input[,'col.index']) %% 2);
    DF.input[,'position'] <- apply(
        X      = DF.input[,c('row.index','col.index')],
        MARGIN = 1,
        FUN    = function(x) { paste0('(',paste(x,collapse='_'),')') }
        );

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.input,
        mapping = ggplot2::aes(
            x      = lon,
            y      = lat,
            colour = parity # date
            ),
        alpha = 0.9,
        size  = 1.5
        );

    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data    = DF.input,
        mapping = ggplot2::aes(
            x      = lon,
            y      = lat,
            colour = parity,
            group  = position
            ),
        alpha = 0.50,
        size  = 0.25
        );

    ggplot2::ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  16,
        units  = 'in'
        );

    return( NULL );

    }
