
getData <- function(
    data.directory = NULL,
    output.file    = NULL
    ) {

    thisFunctionName <- "getData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(readr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ifelse(is.null(output.file),FALSE,file.exists(output.file)) ) {

        cat(paste0("\n### ",output.file," already exists; loading this file ...\n"));
        DF.output <- readRDS(file = output.file);
        cat(paste0("\n### Finished loading raw data.\n"));

    } else {

        temp.files <- list.files(path = data.directory);

        DF.metadata <- getData_metadata(
            data.files = temp.files
            );

        DF.output <- NULL;
        for ( temp.string in unique(as.character(DF.metadata[,'date'])) ) {
            DF.given.date <- getData_given.date(
                given.date     = as.Date(temp.string),
                DF.metadata    = DF.metadata,
                data.directory = data.directory
                );
            if ( is.null(DF.output) ) {
                DF.output <- DF.given.date;
            } else {
                DF.output <- rbind(DF.output,DF.given.date);
                }
            }

        if (!is.null(output.file)) {
            saveRDS(object = DF.output, file = output.file);
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
getData_given.date <- function(
    given.date     = NULL,
    DF.metadata    = NULL,
    data.directory = NULL
    ) {

    thisFunctionName <- "getData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts: given.date: ",given.date,"\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.metadata <- DF.metadata[DF.metadata[,'date'] == given.date,];

    DF.output <- NULL;
    for ( i in seq(1,nrow(DF.metadata))) {
        # DF.temp <- read.csv(file = file.path(data.directory,DF.metadata[i,'data.file']), header = FALSE);
        DF.temp <- getData_read.csv(
            input.file = file.path(data.directory,DF.metadata[i,'data.file'])
            );
        nrow.DF.temp <- nrow(DF.temp);
        ncol.DF.temp <- ncol(DF.temp);
        cat(paste0("\n# ",thisFunctionName,"(): c(nrow.DF.temp,ncol.DF.temp) = c(",nrow.DF.temp,", ",ncol.DF.temp,")\n"));
        if ( is.null(DF.output) ) {
            DF.output <- data.frame(temp_colname = as.vector(as.matrix(DF.temp)));
        } else {
            DF.output <- cbind(DF.output, temp_colname = as.vector(as.matrix(DF.temp)));
            }
        DF.output <- as.data.frame(DF.output);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = 'temp_colname', replacement = DF.metadata[i,'variable']);
        }

    DF.output[,'date'] <- given.date;

    DF.row.index <- matrix(data = rep(1:nrow.DF.temp,ncol.DF.temp), nrow = nrow.DF.temp, byrow = FALSE);
    DF.col.index <- matrix(data = rep(1:ncol.DF.temp,nrow.DF.temp), ncol = ncol.DF.temp, byrow = TRUE );
    DF.output[,'row_index'] <- as.vector(as.matrix(DF.row.index));
    DF.output[,'col_index'] <- as.vector(as.matrix(DF.col.index));

    leading.colnames <- c('date','row_index','col_index');
    DF.output <- DF.output[,c(leading.colnames,setdiff(colnames(DF.output),leading.colnames))];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

getData_read.csv <- function(
    input.file = NULL
    ) {
    cat(paste0("\n# getData_read.csv(): input.file: ",input.file,"\n"));
    DF.output <- read.csv(file = input.file, header = FALSE);
    # if ( grepl(x = input.file, pattern = getData_dates.xrm) ) {
    #     DF.output <- DF.output[seq(1,nrow(DF.output)-1),];
    # } else if ( grepl(x = input.file, pattern = getData_dates.xc1) ) {
    #     DF.output <- DF.output[,seq(2,ncol(DF.output))];
    # } else if ( grepl(x = input.file, pattern = getData_dates.xcn) ) {
    #     DF.output <- DF.output[,seq(1,ncol(DF.output)-1)];
    # } else if ( grepl(x = input.file, pattern = getData_dates.xrm_xcn) ) {
    #     DF.output <- DF.output[seq(1,nrow(DF.output)-1),seq(1,ncol(DF.output)-1)];
    #     }
    return( DF.output );
    }

getData_metadata <- function(
    data.files = NULL
    ) {

    DF.output <- as.data.frame(t(as.data.frame(strsplit(
        x     = gsub(x = data.files, pattern = "^_", replacement = ""),
        split = "(_|\\.)"
        ))));
    rownames(DF.output) <- NULL;
    colnames(DF.output) <- c('date','beam.mode','variable','file.format');

    DF.output[,'date']      <- as.Date(x = DF.output[,'date'], format = "%Y%m%d");
    DF.output[,'variable']  <- tolower(x = DF.output[,'variable']);
    DF.output[,'data.file'] <- data.files;

    return( DF.output );
    }

### ~~~~~~~~~~~ ####
### ~~~~~~~~~~~ ####
# getData_dates.xrm <- c(
#     "20170404", # dimensions-2017.csv:"20170404",1679,2488
#     "20170416", # dimensions-2017.csv:"20170416",1679,2488
#     "20170709", # dimensions-2017.csv:"20170709",1679,2488
#     "20170826", # dimensions-2017.csv:"20170826",1679,2488
#     "20190406", # dimensions-2019.csv:"20190406",1679,2488
#     "20190629", # dimensions-2019.csv:"20190629",1679,2488
#     "20190711", # dimensions-2019.csv:"20190711",1679,2488
#     "20190804", # dimensions-2019.csv:"20190804",1679,2488
#     "20190816", # dimensions-2019.csv:"20190816",1679,2488
#     "20190921"  # dimensions-2019.csv:"20190921",1679,2488
#     );
# getData_dates.xrm <- paste0("(",paste(getData_dates.xrm,collapse="|"),")");
#
# ### ~~~~~~~~~~~ ####
# getData_dates.xc1 <- c(
#     "20191003", # dimensions-2019.csv:"20191003",1678,2489
#     "20191027", # dimensions-2019.csv:"20191027",1678,2489
#     "20190430", # dimensions-2019.csv:"20190430",1678,2489
#     "20190617"  # dimensions-2019.csv:"20190617",1678,2489
#     );
# getData_dates.xc1 <- paste0("(",paste(getData_dates.xc1,collapse="|"),")");
#
# getData_dates.xcn <- c(
#     "20170615", # dimensions-2017.csv:"20170615",1678,2489
#     "20180423", # dimensions-2018.csv:"20180423",1678,2489
#     "20180517", # dimensions-2018.csv:"20180517",1678,2489
#     "20180610"  # dimensions-2018.csv:"20180610",1678,2489
#     );
# getData_dates.xcn <- paste0("(",paste(getData_dates.xcn,collapse="|"),")");
#
# ### ~~~~~~~~~~~ ####
# getData_dates.xrm_xcn <- c(
#     "20180622", # dimensions-2018.csv:"20180622",1679,2489
#     "20170814", # dimensions-2017.csv:"20170814",1679,2489
#     "20180809", # dimensions-2018.csv:"20180809",1679,2489
#     "20181020", # dimensions-2018.csv:"20181020",1679,2489
#     "20190723"  # dimensions-2019.csv:"20190723",1679,2489
#     );
# getData_dates.xrm_xcn <- paste0("(",paste(getData_dates.xrm_xcn,collapse="|"),")");
