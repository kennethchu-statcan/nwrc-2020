
command.arguments <- commandArgs(trailingOnly = TRUE);
dir.data <- normalizePath( command.arguments[1] );
dir.code <- normalizePath( command.arguments[2] );
dir.pkg  <- normalizePath( command.arguments[3] );
dir.out  <- normalizePath( command.arguments[4] );

# add custom library using .libPaths()
cat("\ndir.data: ", dir.data );
cat("\ndir.code: ", dir.code );
cat("\ndir.pkg:  ", dir.pkg  );
cat("\ndir.out:  ", dir.out  );
cat("\n\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

start.proc.time <- proc.time();
setwd( dir.out );

cat("\n##################################################\n");
require(dplyr);
require(foreach);
require(magrittr);
require(rlang);
require(fpcFeatures);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
code.files <- c(
    "coregisterData.R",
    "geo-standardize.R",
    "getData.R",
    "getData-labelled.R",
    "getData-labelled-helper.R",
    "initializePlot.R",
    "reshapeData.R",
    "visualize-geocoordinates.R",
    "visualizeData-labelled.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# pkg.files <- c(
#     "fpcFeatureEngine.R",
#     "initializePlot.R"
#     );
#
# for ( pkg.file in pkg.files ) {
#     source(file.path(dir.pkg,pkg.file));
#     }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.colour.scheme <- data.frame(
    land_cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
    );
rownames(DF.colour.scheme) <- DF.colour.scheme[,"land_cover"];

# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
colname.pattern <- "V";

data.snapshot  <- "2020-12-18.01";
data.directory <- file.path(dir.data,data.snapshot,"micro-mission-1","Sentinel1","IW","4");

DF.labelled <- getData.labelled(
    data.directory  = data.directory,
    colname.pattern = colname.pattern,
    land.cover      = DF.colour.scheme[,'land_cover'],
    RData.output    = paste0("data-labelled-raw.RData")
    );

cat("\nstr(DF.labelled)\n");
print( str(DF.labelled)   );

# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
visualizeData.labelled(
    DF.input        = DF.labelled,
    colname.pattern = "V"
    );

# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#logger::log_threshold(level = logger::ERROR);
logger::log_threshold(level = logger::TRACE);

n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
polarizations <- c("VV","VH");

for ( temp.polarization in polarizations ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.polarization <- DF.labelled[,c("X","Y","date",temp.polarization)];
    DF.polarization[,"X_Y"] <- apply(
        X      = DF.polarization[,c('X','Y')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse="_")) }
        );

    cat("\nstr(DF.polarization)\n");
    print( str(DF.polarization)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.fpcFeatureEngine <- fpcFeatures::fpcFeatureEngine$new(
        training.data       = DF.polarization,
        location            = 'X_Y',
        date                = 'date',
        variable            = temp.polarization,
        n.partition         = n.partition,
        n.order             = n.order,
        n.basis             = n.basis,
        smoothing.parameter = smoothing.parameter,
        n.harmonics         = n.harmonics
        );

    my.fpcFeatureEngine$fit();

    saveRDS(
        object = my.fpcFeatureEngine,
        file = paste0("fpc-",temp.polarization,"-FeatureEngine.RData")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ggplot2::ggsave(
        file   = paste0("fpc-",temp.polarization,"-harmonics.png"),
        plot   = my.fpcFeatureEngine$plot.harmonics(),
        dpi    = 150,
        height =   4 * n.harmonics,
        width  =  16,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.bspline.fpc <- my.fpcFeatureEngine$transform(
        newdata  = DF.polarization,
        location = 'X_Y',
        date     = 'date',
        variable = temp.polarization
        );

    cat("\nstr(DF.polarization)\n");
    print( str(DF.bspline.fpc)    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

cat("\n##################################################\n");
cat("\n##### warnings():\n");
print(       warnings()    );

cat("\n##### getOption('repos'):\n");
print(       getOption('repos')    );

cat("\n##### .libPaths():\n");
print(       .libPaths()    );

cat("\n##### sessionInfo():\n");
print(       sessionInfo()    );

# print system time to log
cat("\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

# print elapsed time to log
stop.proc.time <- proc.time();
cat("\n##### stop.proc.time - start.proc.time:\n");
print(       stop.proc.time - start.proc.time    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
quit(save = "no");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###


# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#logger::log_threshold(level = logger::ERROR);
logger::log_threshold(level = logger::TRACE);

n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot  <- "2020-10-13.01";
data.directory <- file.path(dir.data,data.snapshot);
n.batches      <- 10;

for ( temp.year in c("2017","2018","2019") ) {

    DF.temp.year <- getData(
        data.directory = file.path(data.directory,temp.year),
        output.file    = paste0("data-unlabelled-",temp.year,".RData")
        );

    DF.temp.year <- coregisterData(
        DF.input    = DF.temp.year,
        output.file = paste0("data-unlabelled-",temp.year,"-coregistered.RData")
        );

    DF.temp.year <- DF.temp.year[,setdiff(colnames(DF.temp.year),c('row_index','col_index','lat','lon'))];

    DF.lat.lon <- unique(DF.temp.year[,c('master_lat','master_lon')]);
    DF.lat.lon[,"batch"] <- sample(
        x       = seq(1,n.batches,1),
        size    = nrow(DF.lat.lon),
        replace = TRUE
        );

    DF.temp.year <- dplyr::left_join(
        x  = DF.temp.year,
        y  = DF.lat.lon,
        by = c('master_lat','master_lon')
        );

    remove( list = c("DF.lat.lon") );

    cat("\nstr(DF.temp.year)\n");
    print( str(DF.temp.year)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.date.range <- range(DF.temp.year[,'date']);
    cat("\nstr(temp.date.range)\n");
    print( str(temp.date.range)   );

    my.fpcFeatureEngine <- fpcFeatureEngine$new(
        training.data       = DF.VV,
        location            = 'x_y',
        date                = 'date',
        variable            = 'VV',
        min.date            = min(temp.date.range),
        max.date            = max(temp.date.range),
        n.partition         = n.partition,
        n.order             = n.order,
        n.basis             = n.basis,
        smoothing.parameter = smoothing.parameter,
        n.harmonics         = n.harmonics
        );

    my.fpcFeatureEngine$fit();

    saveRDS(
        object = my.fpcFeatureEngine,
        file   = paste0("fpc-",temp.year,"-FeatureEngine.RData")
        );

    ggplot2::ggsave(
        file   = paste0("fpc-",temp.year,"-harmonics.png"),
        plot   = my.fpcFeatureEngine$plot.harmonics(),
        dpi    = 150,
        height =   4 * n.harmonics,
        width  =  16,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.batch in seq(1,n.batches,1) ) {

        DF.temp.batch <- DF.temp.year[temp.batch == DF.temp.year[,'batch'],];

        DF.temp.batch[,"master_lat_lon"] <- apply(
            X      = DF.temp.batch[,c('master_lat','master_lon')],
            MARGIN = 1,
            FUN    = function(x) { return(paste(x,collapse="_")) }
            );

        cat(paste0("\nstr(DF.temp.batch): year = ",temp.year,", batch = ",temp.batch,"\n"));
        print( str(DF.temp.batch)   );

        DF.bspline.fpc <- my.fpcFeatureEngine$transform(
            newdata  = DF.temp.batch,
            location = 'master_lat_lon',
            date     = 'date',
            variable = 'vv'
            );

        cat("\nstr(DF.bspline.fpc)\n");
        print( str(DF.bspline.fpc)   );

        selected.colnames <- grep(
            x       = colnames(DF.bspline.fpc),
            pattern = "^[0-9]+$",
            invert  = TRUE,
            value   = TRUE
            );

        DF.fpc <- DF.bspline.fpc[,selected.colnames];

        cat("\nstr(DF.fpc)\n");
        print( str(DF.fpc)   );

        temp.batch.string <- stringr::str_pad(
            string = as.character(temp.batch),
            width  = 1 + base::floor(base::log10(n.batches)),
            pad    = "0"
            );

        saveRDS(
            object = DF.fpc,
            file   = paste0("fpc-",temp.year,"-scores-batch-",temp.batch.string,".RData")
            );

        # png('plot-scatter-fpc1-fpc2.png', height = 8, width = 8, unit = 'in', res = 300);
        # plot(
        #     x    = DF.bspline.fpc[,'fpc_1'],
        #     y    = DF.bspline.fpc[,'fpc_2'],
        #     pch  = 19,
        #     cex  =  0.1,
        #     xlim = 300 * c(-1,1),
        #     ylim = 150 * c(-1,1)
        #     );
        # dev.off()

        remove( list = c("DF.temp.batch","DF.bspline.fpc","DF.fpc") );

        }

    remove( list = c("DF.temp.year","my.fpcFeatureEngine") );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

cat("\n##################################################\n");
cat("\n##### warnings():\n");
print(       warnings()    );

cat("\n##### getOption('repos'):\n");
print(       getOption('repos')    );

cat("\n##### .libPaths():\n");
print(       .libPaths()    );

cat("\n##### sessionInfo():\n");
print(       sessionInfo()    );

# print system time to log
cat("\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

# print elapsed time to log
stop.proc.time <- proc.time();
cat("\n##### stop.proc.time - start.proc.time:\n");
print(       stop.proc.time - start.proc.time    );
