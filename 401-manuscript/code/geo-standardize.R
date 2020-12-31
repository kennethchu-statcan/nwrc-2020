
geo.standardize <- function(
    data.directory = NULL
    ) {

    thisFunctionName <- "geo.standardize";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nlist.files(data.directory)\n");
    print( list.files(data.directory)   );

    years <- list.files(data.directory);
    for ( temp.year in years ) {

        list.data <- geo.standardize_get.data(
            data.directory = file.path(data.directory,temp.year),
            output.file    = paste0("list-data-",temp.year,".RData")
            );

        cat("\npryr::object_size(list.data)\n");
        print( pryr::object_size(list.data)   );

        cat("\nstr(list.data )\n");
        print( str(list.data )   );

        geo.standardize_pairwise.RMSE(
            list.data  = list.data[['Lon']],
            output.csv = paste0("pairwise-RMSE-",temp.year,"-Lon.csv"),
            output.png = paste0("pairwise-RMSE-",temp.year,"-Lon.png")
            );

        geo.standardize_pairwise.RMSE(
            list.data  = list.data[['Lat']],
            output.csv = paste0("pairwise-RMSE-",temp.year,"-Lat.csv"),
            output.png = paste0("pairwise-RMSE-",temp.year,"-Lat.png")
            );

        remove( list = c("list.data") );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
geo.standardize_pairwise.RMSE <- function(
    list.data  = NULL,
    output.csv = NULL,
    output.png = NULL
    ) {

    require(ComplexHeatmap);

    names.list.data <- names(list.data);

    DF.output <- as.data.frame(matrix(
        data = rep(x = NA, times = length(names.list.data)^2),
        nrow = length(names.list.data)
        ));
    rownames(DF.output) <- names.list.data;
    colnames(DF.output) <- names.list.data;

    for ( i in seq(1,  length(names.list.data)-1) ) {
    for ( j in seq(i+1,length(names.list.data)  ) ) {

        temp.rowname <- names.list.data[i];
        temp.colname <- names.list.data[j];

        cat(paste0("\n# (i,j) = (",i,",",j,"): ",temp.rowname,", ",temp.colname,"\n"));

        temp.diff <- as.numeric(as.matrix(list.data[[temp.rowname]] - list.data[[temp.colname]]));
        temp.RMSE <- sqrt(mean(temp.diff^2));

        DF.output[temp.rowname,temp.colname] <- temp.RMSE;
        DF.output[temp.colname,temp.rowname] <- temp.RMSE;

        }}

    write.csv(
        x         = DF.output,
        file      = output.csv,
        row.names = TRUE,
        col.names = TRUE
        );

    selected.rownames <- grep(x = rownames(DF.output), pattern = "_", invert = FALSE, value = TRUE);
    selected.colnames <- grep(x = colnames(DF.output), pattern = "_", invert = TRUE,  value = TRUE);
    my.Heatmap <- Heatmap(matrix = DF.output[selected.rownames,selected.colnames]);

    png(filename = output.png, height = 8, width = 9, units = "in", res = 300);
    print(my.Heatmap)
    dev.off()

    return( DF.output );

    }

geo.standardize_get.data <- function(
    data.directory = NULL,
    output.file    = NULL
    ) {

    if ( file.exists(output.file) ) {
        list.output <- readRDS(file = output.file);
        return( list.output );
        }

    files.lat.lon <- list.files(data.directory, pattern = "(Lat|Lon)\\.csv");

    temp.dates <- unique(gsub(
        x           = files.lat.lon,
        pattern     = "[^0-9]",
        replacement = ""
        ));

    list.output <- list( Lon = list(), Lat = list());
    for ( temp.date in temp.dates ) {

        file.lon <- grep(x = files.lat.lon, pattern = paste0(temp.date,".+Lon"), value = TRUE);
        DF.lon   <- read.csv(
            file   = file.path(data.directory,file.lon),
            header = FALSE
            );

        file.lat <- grep(x = files.lat.lon, pattern = paste0(temp.date,".+Lat"), value = TRUE);
        DF.lat   <- read.csv(
            file   = file.path(data.directory,file.lat),
            header = FALSE
            );

        if ( identical(as.integer(dim(DF.lon)),c(1678L,2488L)) ) {

            list.output[['Lon']][[temp.date]] <- DF.lon;
            list.output[['Lat']][[temp.date]] <- DF.lat;

        } else if ( identical(as.integer(dim(DF.lon)),c(1679L,2488L)) ) {

            list.output[['Lon']][[paste0(temp.date,"_xr1")]] <- DF.lon[seq(2,nrow(DF.lon)  ),];
            list.output[['Lat']][[paste0(temp.date,"_xr1")]] <- DF.lat[seq(2,nrow(DF.lat)  ),];

            list.output[['Lon']][[paste0(temp.date,"_xrm")]] <- DF.lon[seq(1,nrow(DF.lon)-1),];
            list.output[['Lat']][[paste0(temp.date,"_xrm")]] <- DF.lat[seq(1,nrow(DF.lat)-1),];

        } else if ( identical(as.integer(dim(DF.lon)),c(1678L,2489L)) ) {

            list.output[['Lon']][[paste0(temp.date,"_xc1")]] <- DF.lon[,seq(2,ncol(DF.lon)  )];
            list.output[['Lat']][[paste0(temp.date,"_xc1")]] <- DF.lat[,seq(2,ncol(DF.lat)  )];

            list.output[['Lon']][[paste0(temp.date,"_xcn")]] <- DF.lon[,seq(1,ncol(DF.lon)-1)];
            list.output[['Lat']][[paste0(temp.date,"_xcn")]] <- DF.lat[,seq(1,ncol(DF.lat)-1)];

        } else if ( identical(as.integer(dim(DF.lon)),c(1679L,2489L)) ) {

            list.output[['Lon']][[paste0(temp.date,"_xr1_xc1")]] <- DF.lon[seq(2,nrow(DF.lon)  ),seq(2,ncol(DF.lon)  )];
            list.output[['Lat']][[paste0(temp.date,"_xr1_xc1")]] <- DF.lat[seq(2,nrow(DF.lat)  ),seq(2,ncol(DF.lat)  )];

            list.output[['Lon']][[paste0(temp.date,"_xr1_xcn")]] <- DF.lon[seq(2,nrow(DF.lon)  ),seq(1,ncol(DF.lon)-1)];
            list.output[['Lat']][[paste0(temp.date,"_xr1_xcn")]] <- DF.lat[seq(2,nrow(DF.lat)  ),seq(1,ncol(DF.lat)-1)];

            list.output[['Lon']][[paste0(temp.date,"_xrm_xc1")]] <- DF.lon[seq(1,nrow(DF.lon)-1),seq(2,ncol(DF.lon)  )];
            list.output[['Lat']][[paste0(temp.date,"_xrm_xc1")]] <- DF.lat[seq(1,nrow(DF.lat)-1),seq(2,ncol(DF.lat)  )];

            list.output[['Lon']][[paste0(temp.date,"_xrm_xcn")]] <- DF.lon[seq(1,nrow(DF.lon)-1),seq(1,ncol(DF.lon)-1)];
            list.output[['Lat']][[paste0(temp.date,"_xrm_xcn")]] <- DF.lat[seq(1,nrow(DF.lat)-1),seq(1,ncol(DF.lat)-1)];

            }

        }

    saveRDS(file = output.file, object = list.output);
    return( list.output );

    }
