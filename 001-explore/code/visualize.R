
visualize <- function(
    list_input = NULL
    ) {

    thisFunctionName <- "visualize";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.temp <- data.frame();
    for ( temp.name in names(list_input) ) {
        DF.temp.1 <- list_input[[temp.name]];
        DF.temp.1[,"type"] <- temp.name;
	DF.temp <- rbind(DF.temp,DF.temp.1);
        }

    DF.temp[                           ,"plot.colour"] <- 0;
    DF.temp[DF.temp[,"type"] == "swamp","plot.colour"] <- 1;

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );

    visualize_scatter3D(
        DF.input = DF.temp
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

###################################################
visualize_scatter3D <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-scatter3D.png'
    ) {
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

