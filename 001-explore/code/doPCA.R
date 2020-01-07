
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

    DF.temp[,"X_Y"] <- paste(DF.temp[,"X"],DF.temp[,"Y"],sep="_");

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );

    saveRDS(object = DF.temp, file = "tmp-PCA.RData");    

    for (i in seq(1,50)) {
        doPCA_ggplot2_Comp1(
            DF.input = DF.temp
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

###################################################
doPCA_ggplot2_scatter <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-ggplot2-PC.png'
    ) {

    require(ggplot2);

    DF.input[,"X_Y"] <- paste(DF.input[,"X"],DF.input[,"Y"],sep="_");

    my.ggplot <- initializePlot();
    my.ggplot <- my.ggplot + geom_line(
        data    = DF.input,
        mapping = aes(x = Comp.1, y = Comp.2, group = X_Y, colour = type)
        );

    ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =   8,
        units  = 'in'
        );

    return( NULL );

    }

doPCA_ggplot2_Comp1 <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-ggplot2-Comp1.png'
    ) {

    require(ggplot2);


    my.ggplot <- initializePlot();
    my.ggplot <- my.ggplot + geom_line(
        data    = DF.input,
        mapping = aes(x = Comp.1, y = Comp.2, group = X_Y, colour = type)
        );

    selected.pixel <- base::sample(x=unique(DF.input[,"X_Y"]),size=1);
    DF.temp <- DF.input[DF.input[,"X_Y"] %in% selected.pixel,];

    my.ggplot <- initializePlot(
        title    = selected.pixel,
        subtitle = paste0("(",DF.temp[,"type"],")")
        );

    my.ggplot <- my.ggplot + geom_path(
        data    = DF.temp,
        mapping = aes(x=date,y=Comp.1,group=X_Y,color=type),
        alpha   = 0.5
        );

    PNG.output <- paste0('tmp-ggplot2-Comp1-',selected.pixel,'.png');
    ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  16,
        units  = 'in'
        );

    return( NULL );

    }

