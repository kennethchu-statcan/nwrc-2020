
getData.labelled <- function(
    data.directory     = NULL,
    colname.pattern    = NULL,
    exclude.years      = NULL,
    exclude.land.types = NULL,
    land.cover         = NULL,
    RData.output       = paste0("data-labelled.RData")
    ) {

    thisFunctionName <- "getData.labelled";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

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
    years <- getData.labelled_getYears(
        data.directory = data.directory,
        exclude.years  = exclude.years
        );

    cat(paste0("\n# ",thisFunctionName,"(): years:","\n"));
    print( years );

    DF.output <- data.frame();
    for ( temp.year in years ) {

        list.data <- getData.labelled_helper(
            data.directory     = data.directory,
            year               = temp.year,
            exclude.land.types = exclude.land.types,
            output.file        = file.path(temp.directory,paste0("data-",temp.year,"-raw.RData"))
            );

        cat(paste0("\nstr(list.data) -- ",temp.year,"\n"));
        print(        str(list.data) );

        DF.data.reshaped <- reshapeData(
            list.input      = list.data,
            colname.pattern = colname.pattern,
            land.cover      = land.cover,
            output.file     = file.path(temp.directory,paste0("data-",temp.year,"-reshaped.RData"))
            );

        cat(paste0("\nstr(DF.data.reshaped) -- ",temp.year,"\n"));
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
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

getData.labelled_getYears <- function(data.directory = NULL, exclude.years = NULL) {
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
