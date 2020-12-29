
reshapeData <- function(
    list.input      = NULL,
    colname.pattern = NULL,
    land.cover      = NULL,
    output.file     = NULL
    ) {

    thisFunctionName <- "reshapeData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(output.file) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));

        DF.output <- readRDS(file = output.file);

        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        list.data <- list();
        for ( temp.cover in names(list.input) ) {
            list.data[[ temp.cover ]] <- reshapeData_long(
                DF.input        = list.input[[ temp.cover ]],
                land.cover      = temp.cover,
                colname.pattern = colname.pattern
                );
            }

        DF.output <- data.frame();
        for ( temp.cover in names(list.data) ) {
            DF.output <- rbind(
                DF.output,
                list.data[[ temp.cover ]]
                );
            }

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.output[,"X_Y_year" ] <- paste(DF.output[,"X"],DF.output[,"Y"],DF.output[,"year"],sep = "_");

        DF.output[,"land_cover"] <- factor(
            x       = as.character(DF.output[,"land_cover"]),
            levels  = land.cover,
            ordered = FALSE
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        # add scaled variables
        target.variables <- grep(
            x       = colnames(DF.output),
            pattern = colname.pattern,
            value   = TRUE
            );

        for ( temp.variable in target.variables ) {
            DF.output <- reshapeData_attachScaledVariable(
                DF.input        = DF.output,
                target.variable = temp.variable,
                by.variable     = "X_Y_year"
                );
            }

        rownames(DF.output) <- paste0("ID_",seq(1,nrow(DF.output)));

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        # add OPC (ordinary principal component) variables
        temp.formula <- paste0("~ ",paste(x = target.variables, collapse = " + "));
        temp.formula <- as.formula( temp.formula );

        results.princomp <- princomp(
            formula = temp.formula,
            data    = DF.output
            );

        DF.scores <- results.princomp[['scores']];
        DF.scores <- as.data.frame(DF.scores);
        colnames(DF.scores) <- gsub(
            x           = colnames(DF.scores),
            pattern     = "Comp\\.",
            replacement = paste0(colname.pattern,"_opc")
            );

        DF.output[,"syntheticID"] <- rownames(DF.output);
        DF.scores[,"syntheticID"] <- rownames(DF.scores);

        DF.output <- dplyr::left_join(
            x  = DF.output,
            y  = DF.scores,
            by = "syntheticID"
            );
        DF.output <- as.data.frame(DF.output);

        DF.output <- DF.output[,setdiff(colnames(DF.output),"syntheticID")];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        # add scaled OPC variables
        target.variables <- grep(
            x       = colnames(DF.output),
            pattern = paste0(colname.pattern,"_opc"),
            value   = TRUE
            );

        for ( temp.variable in target.variables ) {
            DF.output <- reshapeData_attachScaledVariable(
                DF.input        = DF.output,
                target.variable = temp.variable,
                by.variable     = "X_Y_year"
                );
            }

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        if (!is.null(output.file)) {
            saveRDS(object = DF.output, file = output.file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    remove(list = c("list.data","DF.scores"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

###################################################
reshapeData_attachScaledVariable <- function(
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

reshapeData_long <- function(
    DF.input        = NULL,
    land.cover      = NULL,
    colname.pattern = NULL
    ) {

    require(dplyr);
    require(tidyr);

    temp.colnames <- grep(
        x       = colnames(DF.input),
        pattern = colname.pattern,
        value   = TRUE
        );

    DF.temp <- DF.input[,c("X","Y",temp.colnames)];
    #DF.temp       <- DF.input[,temp.colnames];
    #DF.temp[,"X"] <- paste0(land.cover,"_",seq(1,nrow(DF.temp)));
    #DF.temp[,"Y"] <- paste0(land.cover,"_",seq(1,nrow(DF.temp)));

    DF.temp <- DF.temp %>%
        tidyr::gather(column.name,value,-X,-Y);
    DF.temp <- as.data.frame(DF.temp);

    DF.temp[,"date"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
        pattern = "[0-9]{8}"
        );

    DF.temp[,"variable"] <- stringr::str_extract(
        string  = DF.temp[,"column.name"],
        pattern = paste0(colname.pattern,".*")
        );

    DF.temp <- DF.temp[,c("X","Y","date","variable","value")];

    DF.output <- DF.temp %>%
        dplyr::select(X,Y,date,variable,value) %>%
        tidyr::spread(key=variable,value=value);

    DF.output[,"land_cover"] <- as.character(land.cover);
    DF.output[,"date"]       <- as.Date(x = DF.output[,"date"], tryFormats = c("%Y%m%d"));

    temp.colnames <- grep(
        x       = colnames(DF.output),
        pattern = colname.pattern,
        value   = TRUE
        );

    for ( temp.colname in temp.colnames ) {
        DF.output[,temp.colname] <- as.numeric(DF.output[,temp.colname]);
        }

    DF.output[,"year"]         <- format(x = DF.output[,"date"], format = "%Y");
    DF.output[,"new_year_day"] <- as.Date(paste0(DF.output[,"year"],"-01-01"));
    DF.output[,"date_index"]   <- as.integer(DF.output[,"date"]) - as.integer(DF.output[,"new_year_day"]);

    first.colnames     <- c("X","Y","year","land_cover","new_year_day","date","date_index");
    colnames.reordered <- c(first.colnames,setdiff(colnames(DF.output),first.colnames));
    DF.output          <- DF.output[,colnames.reordered];

    remove( list = c("DF.temp") );
    return( DF.output );

    }
