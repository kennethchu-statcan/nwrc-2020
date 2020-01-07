
doPCA <- function(
    list_input = NULL
    ) {

    thisFunctionName <- "doPCA";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.temp <- data.frame();
    for ( temp.name in names(list_input) ) {
        DF.temp.1 <- list_input[[temp.name]];
        DF.temp.1[,"type"] <- temp.name;
        DF.temp <- rbind(DF.temp,DF.temp.1);
        }

    results.princomp <- princomp(
        formula = ~ Band_1 + Band_2 + Band_3,
        data    = DF.temp
        );

    DF.temp <- cbind(
        DF.temp,
        results.princomp[['scores']]
        );

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );

    saveRDS(object = DF.temp, file = "tmp-PCA.RData");    

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

###################################################
doPCA_plotly_scatter3d <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-scatter3D.png'
    ) {
    plotly::plot_ly(
        x       = DF.temp[,"Band_1"],
        y       = DF.temp[,"Band_2"],
        z       = DF.temp[,"Band_3"],
        type    = "scatter3d",
        mode    = "markers",
        size    = 0.75,
        color   = DF.temp[,"plot.colour"],
        colors  = c("red","blue"),
        opacity = 0.3
        );
    }

doPCA_scatter3D <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-scatter3D.png'
    ) {
    #require(plotly);
    #my.plot <- plotly::plot_ly(
    #    x      = DF.input[,"Band_1"],
    #    y      = DF.input[,"Band_2"],
    #    z      = DF.input[,"Band_3"],
    #    type   = "scatter3d",
    #    mode   = "markers"#,
    #    #color = as.integer(DF.input[,"type"])
    #    #width  = NULL,
    #    #height = NULL
    #    );
    #plotly::orca(
    #    p    = my.plot,
    #    file = PNG.output
    #    );
    require(plot3D);
    png(filename = PNG.output, height = 6, width = 6, units = "in", res = 300);
    plot3D::scatter3D(
        x       = DF.input[,"Band_1"],
        y       = DF.input[,"Band_2"],
        z       = DF.input[,"Band_3"],
	col.var = as.integer(DF.input[,"type"]), 
	col     = c("red","blue"),
	pch     = 19,
	cex     = 0.1
        );
    dev.off();
    }

