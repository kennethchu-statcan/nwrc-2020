
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
require(foreach);
require(magrittr);
require(rlang);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
code.files <- c(
    "getData.R",
    "getData-beam-mode.R",
    "getData-beam-mode-helper.R",
    "reshapeData.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
pkg.files <- c(
    "fpcFeatureEngine.R",
    "initializePlot.R"
    );

for ( pkg.file in pkg.files ) {
    source(file.path(dir.pkg,pkg.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

DF.colour.scheme <- data.frame(
    land.cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
    );
rownames(DF.colour.scheme) <- DF.colour.scheme[,"land.cover"];

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
satellites      <- "sentinel";
beam.mode       <- "IW4";
colname.pattern <- "V";

data.snapshot       <- "2020-02-24.03";
data.directory      <- file.path(dir.data,data.snapshot,"Sentinal1","relative-orbit-number");
beam.mode.directory <- file.path(data.directory,beam.mode);

DF.IW4 <- getData.beam.mode(
    data.directory  = beam.mode.directory,
    satellites      = satellites,
    beam.mode       = beam.mode,
    colname.pattern = colname.pattern,
    land.cover      = DF.colour.scheme[,'land.cover'],
    RData.output    = paste0("data-labelled-",beam.mode,".RData")
    );

is.not.2016 <- ("2016" != DF.IW4[,'year']);
DF.IW4 <- DF.IW4[is.not.2016,c(c("X","Y","date","type","VV","VH"))];
colnames(DF.IW4) <- gsub(x = colnames(DF.IW4), pattern = "^X$",    replacement = "x");
colnames(DF.IW4) <- gsub(x = colnames(DF.IW4), pattern = "^Y$",    replacement = "y");
colnames(DF.IW4) <- gsub(x = colnames(DF.IW4), pattern = "^type$", replacement = "land_cover");

cat("\nstr(DF.IW4)\n");
print( str(DF.IW4)   );

DF.VV <- DF.IW4[,c("x","y","date","VV")];
DF.VV[,"x_y"] <- apply(
    X      = DF.VV[,c('x','y')],
    MARGIN = 1,
    FUN    = function(x) { return(paste(x,collapse="_")) }
    );

cat("\nstr(DF.VV)\n");
print( str(DF.VV)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#logger::log_threshold(level = logger::ERROR);
logger::log_threshold(level = logger::TRACE);

n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

my.fpcFeatureEngine <- fpcFeatureEngine$new(
    training.data       = DF.VV,
    location            = 'x_y',
    date                = 'date',
    variable            = 'VV',
    n.partition         = n.partition,
    n.order             = n.order,
    n.basis             = n.basis,
    smoothing.parameter = smoothing.parameter,
    n.harmonics         = n.harmonics
    );

my.fpcFeatureEngine$fit();

ggplot2::ggsave(
    file   = "plot-harmonics.png",
    plot   = my.fpcFeatureEngine$plot.harmonics(),
    dpi    = 150,
    height =   4 * n.harmonics,
    width  =  16,
    units  = 'in'
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot  <- "2020-10-13.01";
data.directory <- file.path(dir.data,data.snapshot);
years          <- c("2017","2018","2019");
n.batches      <- 500;

for ( temp.year in years ) {

    DF.temp.year <- getData(
        data.directory = file.path(data.directory,temp.year),
        output.file    = paste0("data-unlabelled-",temp.year,".RData")
        );

    DF.lat.lon <- unique(DF.temp.year[,c('lat','lon')]);

    DF.temp.year[,"batch"] <- sample(
        x       = seq(1,n.batches,1),
        size    = nrow(DF.temp.year),
        replace = TRUE
        );

    for ( temp.batch in seq(1,n.batches,1) ) {

        DF.temp.batch <- DF.temp.year[temp.batch == DF.temp.year[,'batch'],];

        DF.temp.batch[,"lat_lon"] <- apply(
            X      = DF.temp.batch[,c('lat','lon')],
            MARGIN = 1,
            FUN    = function(x) { return(paste(x,collapse="_")) }
            );

        cat(paste0("\nstr(DF.temp.batch) -- ",temp.year," -- ",temp.batch,"\n"));
        print( str(DF.temp.batch)   );

        DF.bspline.fpc <- my.fpcFeatureEngine$transform(
            newdata  = DF.temp.batch,
            location = 'lat_lon',
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
            file   = paste0("fpc-unlabelled-",temp.year,"-batch-",temp.batch.string,".RData")
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

        remove( list = c("DF.bspline.fpc","DF.fpc") );

        }

    remove( list = c("DF.temp.year") );

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
