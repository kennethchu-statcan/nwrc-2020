
doPCA <- function(
    list.input      = NULL,
    make.plots      = TRUE,
    make.heatmaps   = TRUE,
    beam.swath      = NULL,
    year            = NULL,
    colname.pattern = NULL,
    land.types      = c("ag","forest","marsh","shallow","swamp","water"),
    output.file     = paste0("tmp-",beam.swath,"-",year,"-PCA.RData"),
    has.NA.file     = paste0("tmp-",beam.swath,"-",year,"-hasNA.csv")
    ) {

    thisFunctionName <- "doPCA";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    require(ComplexHeatmap);
    require(circlize);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output.file) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));

        DF.PCA <- readRDS(file = output.file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        DF.PCA <- data.frame();
        for ( temp.name in names(list.input) ) {
            DF.temp.1 <- list.input[[temp.name]];
            DF.temp.1[,"type"] <- temp.name;
            DF.PCA <- rbind(DF.PCA,DF.temp.1);
            }

        rownames(DF.PCA) <- paste0("row_",seq(1,nrow(DF.PCA)));

        DF.PCA[,"X_Y" ] <- paste(DF.PCA[,"X"],DF.PCA[,"Y"],sep="_");
	DF.PCA[,"type"] <- factor(
	    x       = as.character(DF.PCA[,"type"]),
	    levels  = land.types,
	    ordered = FALSE
	    );

	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
	# add scaled variables
        temp.colnames <- grep(
            x       = colnames(DF.PCA),
            pattern = colname.pattern,
            value   = TRUE
            );

        for ( temp.variable in temp.colnames ) {
            DF.PCA <- doPCA_attach_scaled_variable(
                DF.input        = DF.PCA,
                target.variable = temp.variable,
                by.variable     = "X_Y"
                );
            }

	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
	# add OPC (ordinary principal component) variables
        temp.formula <- paste0("~ ",paste(x = temp.colnames, collapse = " + "));
        temp.formula <- as.formula( temp.formula );

        results.princomp <- princomp(
            formula = temp.formula,
            data    = DF.PCA
            );

        DF.scores <- results.princomp[['scores']];
        DF.scores <- as.data.frame(DF.scores);
        colnames(DF.scores) <- gsub(
            x           = colnames(DF.scores),
            pattern     = "Comp\\.",
            replacement = paste0(colname.pattern,"_opc")
            );

        DF.PCA[,   "syntheticID"] <- rownames(DF.PCA   );
        DF.scores[,"syntheticID"] <- rownames(DF.scores);

        DF.PCA <- dplyr::left_join(
            x  = DF.PCA,
            y  = DF.scores,
            by = "syntheticID"
            );
        DF.PCA <- as.data.frame(DF.PCA);

        DF.PCA <- DF.PCA[,setdiff(colnames(DF.PCA),"syntheticID")];
        rm(list = c("DF.scores"));

	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
	# add scaled OPC variables
        temp.variables <- grep(
            x       = colnames(DF.PCA),
            pattern = paste0(colname.pattern,"_opc"),
            value   = TRUE
            );

        for ( temp.variable in temp.variables ) {
            DF.PCA <- doPCA_attach_scaled_variable(
                DF.input        = DF.PCA,
                target.variable = temp.variable,
                by.variable     = "X_Y"
                );
            }

	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
	leading.colnames   <- c("X","Y","X_Y","type","date");
	reordered.colnames <- c(
	    leading.colnames,
	    setdiff(colnames(DF.PCA),leading.colnames)
	    );

        DF.PCA <- DF.PCA[,reordered.colnames];

	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        saveRDS(object = DF.PCA, file = output.file);    

        utils::write.csv(
            x         = DF.PCA[rowSums(is.na(DF.PCA)) > 0,],
            file      = has.NA.file, 
            row.names = FALSE
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(DF.PCA)\n");
    print( str(DF.PCA)   );

    cat("\nsummary(DF.PCA)\n");
    print( summary(DF.PCA)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( make.plots ) {

        temp.colnames <- grep(x = colnames(DF.PCA), pattern = colname.pattern, value = TRUE);
        for ( temp.colname in temp.colnames ) {

            doPCA_grouped_time_series(
                DF.input        = DF.PCA,
                target.variable = temp.colname,
                beam.swath      = beam.swath,
                year            = year,
                limits          = c(  -3.0,3.0),
                breaks          = seq(-3.0,3.0,0.5),
                PNG.output      = paste0('tmp-',beam.swath,'-',year,'-timeseries-',temp.colname,'.png')
                );

            if ( make.heatmaps ) {
                doPCA_clustered_heatmap(
                    DF.input        = DF.PCA,
                    target.variable = temp.colname,
                    beam.swath      = beam.swath,
                    year            = year,
                    PNG.output      = paste0('tmp-',beam.swath,'-',year,'-heatmap-',temp.colname,'.png')
                    );
	        }

            }

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        # for (i in seq(1,50)) {
        #    doPCA_single_time_series(
        #        DF.input = DF.PCA
        #        );
        #    }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.PCA );

    }

###################################################
doPCA_clustered_heatmap <- function(
    DF.input        = NULL,
    target.variable = NULL,
    beam.swath      = NULL,
    year            = NULL,
    heatmap_palette = circlize::colorRamp2(c(-3,0,3), c("blue","white","red")),
    PNG.output      = 'tmp-heatmap-Comp1.png'
    ) {

    if (!is.null(heatmap_palette)) {

        cat(paste0("\ndoPCA_clustered_heatmap(): ",beam.swath,", ",year,", ",target.variable,"\n"));

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
            matrix            = as.matrix(DF.temp[,setdiff(colnames(DF.temp),"type")]),
            name              = "pearson", #target.variable,
            clustering_distance_rows = "pearson",
            col               = heatmap_palette,
            show_row_names    = FALSE,
            row_names_side    = "left",
            row_title         = target.variable,
            row_title_side    = "left", #c("left", "right"),
            row_title_gp      = gpar(fontsize = 14),
            column_title      = paste0(beam.swath,", ",year),
            column_title_side = "top", #c("top", "bottom"),
            column_title_gp   = gpar(fontsize = 14),
            cluster_columns   = FALSE,
            show_column_dend  = FALSE,
            column_names_gp   = gpar(fontsize = 8),
            right_annotation  = my.Annotation
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
        replacement = paste0(target.variable,"_scaled")
        );

    DF.output <- DF.output[,setdiff(colnames(DF.output),c("mean_target","sd_target"))];

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
        mapping = aes(x = Comp1, y = Comp2, group = X_Y, colour = type)
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
    beam.swath      = NULL,
    year            = NULL,
    limits          = c(  -3.0,3.0),
    breaks          = seq(-3.0,3.0,0.5),
    PNG.output      = 'tmp-ggplot2-Comp1.png'
    ) {

    cat(paste0("\ndoPCA_grouped_time_series(): ",beam.swath,", ",year,", ",target.variable,"\n"));

    require(ggplot2);

    DF.temp <- DF.input[,c("X_Y","date","type",target.variable)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = target.variable,
        replacement = "target.variable"
        );

    my.ggplot <- initializePlot(
        title    = NULL,
        subtitle = paste0(beam.swath,", ",year,", ",target.variable)
        );

    #my.ggplot <- my.ggplot + geom_line(
    #    data    = DF.temp,
    #    mapping = aes(x=date,y=target.variable,group=X_Y,color=type),
    #    alpha   = 0.3
    #    );

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

    my.ggplot <- my.ggplot + geom_line(
        data    = DF.temp,
        mapping = aes(x=date,y=target.variable,group=X_Y,color=type),
        alpha   = 0.3
        );

    my.ggplot <- my.ggplot + facet_grid(type ~ .);

    ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 100,
        height =  48,
        width  =  48,
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
        mapping = aes(x=date,y=scaled_Comp1,group=X_Y,color=type),
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

