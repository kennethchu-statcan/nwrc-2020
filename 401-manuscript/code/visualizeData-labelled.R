
visualizeData.labelled <- function(
    DF.input         = NULL,
    colname.pattern  = NULL,
    DF.colour.scheme = data.frame(
        row.names  = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
        land_cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
        colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
        ),
    exclude.years       = NULL,
    exclude.land.covers = NULL,
    n.partition         = 20,
    n.order             =  3,
    n.basis             =  9,
    smoothing.parameter =  0.1,
    n.harmonics         =  7,
    plot.timeseries     = TRUE,
    plot.heatmaps       = TRUE
    ) {

    thisFunctionName <- "visualizeData.labelled";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory <- getwd();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.directory <- file.path(initial.directory,"diagnostics");
    if ( !dir.exists(temp.directory) ) {
        dir.create(path = temp.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    land.covers <- DF.colour.scheme[,"land_cover"];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( plot.timeseries ) {

        visualizeData.labelled_plotGroupedTimeSeries(
            DF.input         = DF.input,
            colname.pattern  = colname.pattern,
            DF.colour.scheme = DF.colour.scheme,
            output.directory = temp.directory
            );

        visualizeData.labelled_TimeSeriesRibbonPlots(
            DF.input         = DF.input,
            colname.pattern  = colname.pattern,
            DF.colour.scheme = DF.colour.scheme,
            output.directory = temp.directory
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
visualizeData.labelled_TimeSeriesRibbonPlots <- function(
    DF.input         = NULL,
    colname.pattern  = NULL,
    DF.colour.scheme = NULL,
    output.directory = NULL
    ) {

    require(ggplot2);
    require(dplyr);

    cat("\nstr(DF.input)\n");
    print( str(DF.input)   );

    years            <- unique(DF.input[,"year"]);
    target.variables <- grep(x = colnames(DF.input), pattern = colname.pattern, value = TRUE);

    for ( year            in years            ) {
    for ( target.variable in target.variables ) {

        PNG.output <- paste0('ribbon-',year,'-',target.variable,'.png');
        PNG.output <- file.path(output.directory,PNG.output);

        is.current.year   <- (DF.input[,"year"] == year);
        DF.temp           <- DF.input[is.current.year,c("date","land_cover",target.variable)];
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = target.variable,
            replacement = "target.variable"
            );

        DF.temp <- DF.temp %>%
            dplyr::group_by( date, land_cover ) %>%
            dplyr::summarize(
                target_mean = mean(target.variable, na.rm = TRUE),
                target_sd   =   sd(target.variable, na.rm = TRUE)
                );

        DF.temp <- as.data.frame(DF.temp);
        DF.temp[,"target_mean_plus_sd" ] <- DF.temp[,"target_mean"] + DF.temp[,"target_sd"];
        DF.temp[,"target_mean_minus_sd"] <- DF.temp[,"target_mean"] - DF.temp[,"target_sd"];

        cat("\nstr(DF.temp)\n");
        print( str(DF.temp)   );

        cat("\nDF.temp\n");
        print( DF.temp   );

        cat("\nlevels(DF.temp[,'land_cover'])\n");
        print( levels(DF.temp[,'land_cover'])   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot <- initializePlot(
            title      = NULL,
            subtitle   = paste0(year,", ",target.variable),
            my.palette = DF.colour.scheme[DF.colour.scheme[,"land_cover"] %in% as.character(unique(DF.temp[,"land_cover"])),"colour"]
            );

        my.ggplot <- my.ggplot + ylab(label = NULL);

        my.ggplot <- my.ggplot + scale_x_date(
            breaks       = sort(unique(DF.temp[,"date"])),
            minor_breaks = NULL
            );

        my.ggplot <- my.ggplot + theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5)
            );

        my.ggplot <- my.ggplot + geom_ribbon(
            data    = DF.temp,
            mapping = aes(x = date, ymin = target_mean_minus_sd, ymax = target_mean_plus_sd, fill = land_cover),
            alpha   = 0.2
            );

        my.ggplot <- my.ggplot + geom_line(
            data    = DF.temp,
            mapping = aes(x = date, y = target_mean),
            colour  = "black",
            size    = 1.3,
            alpha   = 0.5
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot <- my.ggplot + facet_grid(land_cover ~ ., scales = "free_y");

        ggsave(
            file   = PNG.output,
            plot   = my.ggplot,
            dpi    = 150,
            height =  16,
            width  =  16,
            units  = 'in'
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        PNG.output <- paste0('ribbon-',year,'-',target.variable,'-fixed.png');
        PNG.output <- file.path(output.directory,PNG.output);

        if ( grepl(x = target.variable, pattern = "_scaled$") ) {
            my.ggplot <- my.ggplot + scale_y_continuous(
                limits = c(  -3,3),
                breaks = seq(-3,3,1)
                );
        } else {
            my.ggplot <- my.ggplot + scale_y_continuous(
                limits = c(  -40,20),
                breaks = seq(-40,20,10)
                );
            }

        my.ggplot <- my.ggplot + facet_grid(land_cover ~ ., scales = "fixed");

        ggsave(
            file   = PNG.output,
            plot   = my.ggplot,
            dpi    = 150,
            height =  16,
            width  =  16,
            units  = 'in'
            );

        }}

    return( NULL );

    }

visualizeData.labelled_plotGroupedTimeSeries <- function(
    DF.input         = NULL,
    colname.pattern  = NULL,
    DF.colour.scheme = NULL,
    output.directory = NULL
    ) {

    require(ggplot2);

    cat("\nstr(DF.input)\n");
    print( str(DF.input)   );

    years            <- unique(DF.input[,"year"]);
    target.variables <- grep(x = colnames(DF.input), pattern = colname.pattern, value = TRUE);

    for ( year            in years            ) {
    for ( target.variable in target.variables ) {

        PNG.output <- paste0('timeseries-',year,'-',target.variable,'.png');
        PNG.output <- file.path(output.directory,PNG.output);

        is.current.year   <- (DF.input[,"year"] == year);
        DF.temp           <- DF.input[is.current.year,c("X_Y_year","date","land_cover",target.variable)];
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = target.variable,
            replacement = "target.variable"
            );

        my.ggplot <- initializePlot(
            title      = NULL,
            subtitle   = paste0(year,", ",target.variable),
            my.palette = DF.colour.scheme[DF.colour.scheme[,"land_cover"] %in% as.character(unique(DF.temp[,"land_cover"])),"colour"]
            );

        my.ggplot <- my.ggplot + ylab(label = NULL);

        my.ggplot <- my.ggplot + scale_x_date(
            breaks       = sort(unique(DF.temp[,"date"])),
            minor_breaks = NULL
            );

        my.ggplot <- my.ggplot + theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5)
            );

        if ( grepl(x = target.variable, pattern = "_scaled$") ) {
            my.ggplot <- my.ggplot + scale_y_continuous(
                limits = c(  -3,3),
                breaks = seq(-3,3,1)
                );
        } else {
            my.ggplot <- my.ggplot + scale_y_continuous(
                limits = c(  -40,20),
                breaks = seq(-40,20,10)
                );
            }

        my.ggplot <- my.ggplot + geom_line(
            data    = DF.temp,
            mapping = aes(x=date,y=target.variable,group = X_Y_year, color = land_cover),
            alpha   = 0.3
            );

        my.ggplot <- my.ggplot + facet_grid(land_cover ~ .);

        ggsave(
            file   = PNG.output,
            plot   = my.ggplot,
            dpi    = 150,
            height =  16,
            width  =  16,
            units  = 'in'
            );

        }}

    return( NULL );

    }
