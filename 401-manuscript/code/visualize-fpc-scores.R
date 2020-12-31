
visualize.fpc.scores <- function(
    variable         = NULL,
    DF.fpc.scores    = NULL,
    DF.colour.scheme = data.frame(
        row.names  = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
        land_cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
        colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
        ),
    output.directory = NULL
    ) {

    thisFunctionName <- "visualize.fpc.scores";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years <- unique(DF.fpc.scores[,'year']);

    for ( temp.year in years ) {
        DF.year <- DF.fpc.scores[DF.fpc.scores[,'year'] == temp.year,];
        visualize.fpc.scores_scatter(
            year             = temp.year,
            variable         = variable,
            DF.input         = DF.year,
            x.var            = "fpc_1",
            y.var            = "fpc_2",
            DF.colour.scheme = DF.colour.scheme,
            title            = NULL,
            subtitle         = paste0(variable,', ',temp.year),
            PNG.output       = file.path(output.directory,paste0("fpc-",variable,"-scoress-",temp.year,".png"))
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
visualize.fpc.scores_scatter <- function(
    year             = NULL,
    variable         = NULL,
    DF.input         = NULL,
    x.var            = NULL,
    y.var            = NULL,
    title            = NULL,
    subtitle         = NULL,
    DF.colour.scheme = NULL,
    PNG.output       = NULL
    ) {

    require(ggplot2);
    require(dplyr);

    DF.temp <- DF.input[,c("land_cover",x.var,y.var)];

    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = x.var,
        replacement = "x_var"
        );

    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = y.var,
        replacement = "y_var"
        );

    my.ggplot <- initializePlot(
        title      = title,
        subtitle   = subtitle,
        my.palette = DF.colour.scheme[DF.colour.scheme[,"land_cover"] %in% as.character(unique(DF.temp[,"land_cover"])),"colour"]
        );

    temp.xlab <- gsub(x = toupper(x.var), pattern = "_", replacement = " ");
    temp.ylab <- gsub(x = toupper(y.var), pattern = "_", replacement = " ");
    my.ggplot <- my.ggplot + xlab( label = temp.xlab );
    my.ggplot <- my.ggplot + ylab( label = temp.ylab );

    my.ggplot <- my.ggplot + guides(
        colour = guide_legend(override.aes = list(alpha =  0.5, size = 5))
        );

    # if ( grepl(x = subtitle, pattern = "scaled") ) {
    #     my.ggplot <- my.ggplot + scale_x_continuous(limits=20*c(-1,1),breaks=seq(-20,20,5));
    #     my.ggplot <- my.ggplot + scale_y_continuous(limits=20*c(-1,1),breaks=seq(-20,20,5));
    # } else {
    #     my.ggplot <- my.ggplot + scale_x_continuous(limits=300*c(-1,1),breaks=seq(-300,300,100));
    #     my.ggplot <- my.ggplot + scale_y_continuous(limits=150*c(-1,1),breaks=seq(-150,150, 50));
    #     }

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.temp,
        mapping = aes(x = x_var, y = y_var, colour = land_cover),
        size    = 0.2,
        alpha   = 0.3
        );

    ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  10,
        units  = 'in'
        );

    return( NULL );

    }
