
getData.beam.mode <- function(
    data.directory     = NULL,
    satellites         = NULL,
    beam.mode          = NULL,
    colname.pattern    = NULL,
    exclude.years      = NULL,
    exclude.land.types = NULL,
    land.cover         = NULL,
    RData.output       = paste0("data-",beam.mode,".RData")
    ) {

    thisFunctionName <- "getData.beam.mode";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n"));
    cat(paste0("\nbeam.mode: ",beam.mode,"\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(RData.output) ) {
        DF.output <- readRDS(file = RData.output);
        cat(paste0("\n",thisFunctionName,"() exits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( DF.output );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory <- getwd();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.directory <- file.path(initial.directory,"data");
    if ( !dir.exists(temp.directory) ) {
        dir.create(path = temp.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years <- getData.beam.mode_getYears(
        data.directory = data.directory,
        exclude.years  = exclude.years
        );

    DF.output <- data.frame();
    for ( temp.year in years ) {

        list.data <- getData(
            data.directory     = data.directory,
            satellites         = satellites,
            beam.mode          = beam.mode,
            year               = temp.year,
            exclude.land.types = exclude.land.types,
            output.file        = file.path(temp.directory,paste0("data-",beam.mode,"-",temp.year,"-raw.RData"))
            );

        cat(paste0("\nstr(list.data) -- ",temp.year,"\n"));
        print(        str(list.data) );

        DF.data.reshaped <- reshapeData(
            list.input      = list.data,
            beam.mode       = beam.mode,
            colname.pattern = colname.pattern,
            land.cover      = land.cover,
            output.file     = file.path(temp.directory,paste0("data-",beam.mode,"-",temp.year,"-reshaped.RData"))
            );

        cat(paste0("\nstr(DF.data.reshaped) -- ",beam.mode,", ",temp.year,"\n"));
        print(        str(DF.data.reshaped) );

        DF.output <- rbind(DF.output,DF.data.reshaped);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (!is.null(RData.output)) {
        saveRDS(object = DF.output, file = RData.output);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    remove(list = c("list.data"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

getData.beam.mode_getYears <- function(data.directory = NULL, exclude.years = NULL) {
    require(stringr);
    temp.files <- list.files(path = data.directory);
    if ( !is.null(exclude.years) ) {
        temp.files <- grep(x = temp.files, pattern = exclude.years, value = TRUE, invert = TRUE)
        }
    years <- sort(unique(as.character(
        stringr::str_match(string = temp.files, pattern = "[0-9]{4}")
        )));
    return( years );
    }
