
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

    for ( temp.variable in c("Comp.1","Comp.2","Comp.3") ) {
        DF.temp <- doPCA_attach_scaled_variable(
            DF.input        = DF.temp,
            target.variable = temp.variable,
            by.variable     = "X_Y"
            );
        }

    cat("\nstr(DF.temp)\n");
    print( str(DF.temp)   );

    cat("\nsummary(DF.temp)\n");
    print( summary(DF.temp)   );

    saveRDS(object = DF.temp, file = "tmp-PCA.RData");    

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Band_1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Band_2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Band_3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band3.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Comp.1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "scaled_Comp.1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-scaled-Comp1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Comp.2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "scaled_Comp.2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-scaled-Comp2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "Comp.3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp3.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.temp,
        target.variable = "scaled_Comp.3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-ggplot2-scaled-Comp3.png'
        );

    for (i in seq(1,50)) {
        doPCA_single_time_series(
            DF.input = DF.temp
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

###################################################
doPCA_attach_scaled_variable <- function(
    DF.input        = NULL,
    target.variable = NULL,
    by.variable     = NULL
    ) {

    require(dplyr);

    my.formula <- as.formula(paste0(target.variable," ~ ",by.variable));

    DF.means <- aggregate(formula = my.formula, data = DF.input, FUN = mean);
    colnames(DF.means) <- gsub(
        x           = colnames(DF.means),
        pattern     = target.variable,
        replacement = "mean_target"
        );

    DF.sds <- aggregate(formula = my.formula, data = DF.input, FUN = sd  );
    colnames(DF.sds) <- gsub(
        x           = colnames(DF.sds),
        pattern     = target.variable,
        replacement = "sd_target"
        );

    DF.output <- dplyr::left_join(
        x  = DF.input,
        y  = DF.means,
        by = by.variable
        );

    DF.output <- dplyr::left_join(
        x  = DF.output,
        y  = DF.sds,
        by = by.variable
        );

    DF.output <- as.data.frame(DF.output);

    DF.output[,"scaled_variable"] <- DF.output[, target.variable ] - DF.output[,"mean_target"];
    DF.output[,"scaled_variable"] <- DF.output[,"scaled_variable"] / DF.output[,  "sd_target"];

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "scaled_variable",
        replacement = paste0("scaled_",target.variable)
        );

    DF.output <- DF.output[,setdiff(colnames(DF.output),c("mean_target","sd_target"))];

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    return( DF.output );

    }

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

doPCA_grouped_time_series <- function(
    DF.input        = NULL,
    target.variable = NULL,
    limits          = c(  -3.0,3.0),
    breaks          = seq(-3.0,3.0,0.5),
    PNG.output      = 'tmp-ggplot2-Comp1.png'
    ) {

    require(ggplot2);

    DF.temp <- DF.input[,c("X_Y","date","type",target.variable)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = target.variable,
        replacement = "target.variable"
        );

    my.ggplot <- initializePlot(
        title    = NULL,
        subtitle = target.variable
        );

    my.ggplot <- my.ggplot + geom_line(
        data    = DF.temp,
        mapping = aes(x=date,y=target.variable,group=X_Y,color=type),
        alpha   = 0.3
        );

    my.ggplot <- my.ggplot + scale_x_date(
        breaks       = sort(unique(DF.temp[,"date"])),
        minor_breaks = NULL
        );

    my.ggplot <- my.ggplot + theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5)
        );

    my.ggplot <- my.ggplot + scale_y_continuous(
        #limits = c(  -0.3,1.6),
        #breaks = seq(-0.2,1.6,0.2)
        limits = c(  -3.0,3.0),
        breaks = seq(-3.0,3.0,0.5)
        );

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

doPCA_single_time_series <- function(
    DF.input   = NULL,
    PNG.output = 'tmp-ggplot2-Comp1.png'
    ) {

    require(ggplot2);

    selected.pixel <- base::sample(x=unique(DF.input[,"X_Y"]),size=1);
    DF.temp <- DF.input[DF.input[,"X_Y"] %in% selected.pixel,];

    my.ggplot <- initializePlot(
        title    = selected.pixel,
        subtitle = paste0("(",DF.temp[,"type"],")")
        );

    my.ggplot <- my.ggplot + geom_line(
        data    = DF.temp,
        mapping = aes(x=date,y=scaled_Comp.1,group=X_Y,color=type),
        alpha   = 0.5
        );

    my.ggplot <- my.ggplot + scale_x_date(
        breaks       = sort(unique(DF.temp[,"date"])),
        minor_breaks = NULL
        );

    my.ggplot <- my.ggplot + theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5)
        );

    my.ggplot <- my.ggplot + scale_y_continuous(
        limits = c(  -3.0,3.0),
        breaks = seq(-3.0,3.0,0.5)
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

