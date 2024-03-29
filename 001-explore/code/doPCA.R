
doPCA <- function(
    list_input  = NULL,
    make_plots  = TRUE,
    output_file = "tmp-PCA.RData"
    ) {

    thisFunctionName <- "doPCA";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    require(ComplexHeatmap);
    require(circlize);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output_file) ) {

        cat(paste0("\n### ",output_file," already exists; loading this file ...\n"));

        DF.PCA <- readRDS(file = output_file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        DF.PCA <- data.frame();
        for ( temp.name in names(list_input) ) {
            DF.temp.1 <- list_input[[temp.name]];
            DF.temp.1[,"type"] <- temp.name;
            DF.PCA <- rbind(DF.PCA,DF.temp.1);
            }

        results.princomp <- princomp(
            formula = ~ Band_1 + Band_2 + Band_3,
            data    = DF.PCA
            );

        DF.PCA <- cbind(
            DF.PCA,
            results.princomp[['scores']]
            );

        DF.PCA[,"X_Y"] <- paste(DF.PCA[,"X"],DF.PCA[,"Y"],sep="_");

        for ( temp.variable in c("Comp.1","Comp.2","Comp.3") ) {
            DF.PCA <- doPCA_attach_scaled_variable(
                DF.input        = DF.PCA,
                target.variable = temp.variable,
                by.variable     = "X_Y"
                );
            }

        saveRDS(object = DF.PCA, file = output_file);    

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(DF.PCA)\n");
    print( str(DF.PCA)   );

    cat("\nsummary(DF.PCA)\n");
    print( summary(DF.PCA)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !make_plots ) {
        cat(paste0("\n",thisFunctionName,"() quits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( DF.PCA );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.1",
        PNG.output      = 'tmp-clustered-heatmap-scaled-Comp1.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Comp.1",
        PNG.output      = 'tmp-clustered-heatmap-Comp1.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Band_1",
        PNG.output      = 'tmp-clustered-heatmap-Band1.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.2",
        PNG.output      = 'tmp-clustered-heatmap-scaled-Comp2.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Comp.2",
        PNG.output      = 'tmp-clustered-heatmap-Comp2.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Band_2",
        PNG.output      = 'tmp-clustered-heatmap-Band2.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.3",
        PNG.output      = 'tmp-clustered-heatmap-scaled-Comp3.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Comp.3",
        PNG.output      = 'tmp-clustered-heatmap-Comp3.png'
        );

    doPCA_clustered_heatmap(
        DF.input        = DF.PCA,
        target.variable = "Band_3",
        PNG.output      = 'tmp-clustered-heatmap-Band3.png'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Band_1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Band_2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Band_3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Band3.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Comp.1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.1",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-scaled-Comp1.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Comp.2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.2",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-scaled-Comp2.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "Comp.3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-Comp3.png'
        );

    doPCA_grouped_time_series(
        DF.input        = DF.PCA,
        target.variable = "scaled_Comp.3",
        limits          = c(  -3.0,3.0),
        breaks          = seq(-3.0,3.0,0.5),
        PNG.output      = 'tmp-scaled-Comp3.png'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    #for (i in seq(1,50)) {
    #    doPCA_single_time_series(
    #        DF.input = DF.PCA
    #        );
    #    }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.PCA );

    }

###################################################
doPCA_clustered_heatmap <- function(
    DF.input        = NULL,
    target.variable = NULL,
    heatmap_palette = circlize::colorRamp2(c(-3,0,3), c("blue","white","red")),
    PNG.output      = 'tmp-clustered-heatmap-Comp1.png'
    ) {

    if (!is.null(heatmap_palette)) {

        require(ComplexHeatmap);
        require(dplyr);
        require(tidyr);

        cat("\nstr(DF.input) -- doPCA_cluster_heatmap()\n");
        print( str(DF.input)   );

        DF.temp <- DF.input[,c("X_Y","type","date",target.variable)];
        colnames(DF.temp) <- gsub(
            x           = colnames(DF.temp),
            pattern     = target.variable,
            replacement = "target_variable"
            );

        DF.temp <- DF.temp %>%
            tidyr::spread(key = date, value = target_variable);

        cat("\nstr(DF.temp) -- doPCA_cluster_heatmap()\n");
        print( str(DF.temp) );

        rownames(DF.temp) <- DF.temp[,"X_Y"];
        DF.temp <- DF.temp[,setdiff(colnames(DF.temp),"X_Y")];

        png(filename = PNG.output, height = 5, width = 4, units = "in", res = 300);
        my.Annotation <- ComplexHeatmap::HeatmapAnnotation(
            type  = DF.temp[,"type"],
            col   = list(type = c("swamp" = "yellow", "marsh" = "green")),
            which = "row"
            );
        my.Heatmap <- ComplexHeatmap::Heatmap(
            matrix           = as.matrix(DF.temp[,setdiff(colnames(DF.temp),"type")]),
            name             = target.variable,
            clustering_distance_rows = "pearson",
            col              = heatmap_palette,
            show_row_names   = FALSE,
            row_names_side   = "left",
            row_title        = target.variable,
            row_title_side   = "left", #c("left", "right"),
            row_title_gp     = gpar(fontsize = 14),
            cluster_columns  = FALSE,
            show_column_dend = FALSE,
            column_names_gp  = gpar(fontsize = 8),
            right_annotation = my.Annotation
            );
        ComplexHeatmap::draw(
            object                  = my.Heatmap,
            legend_labels_gp        = grid::gpar(fontsize = 10, fontface = "bold"),
            heatmap_row_names_gp    = grid::gpar(fontsize = 10, fontface = "bold"),
            heatmap_column_names_gp = grid::gpar(fontsize = 10, fontface = "bold")
            );
        dev.off();
        }

    return( NULL );

    }

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

