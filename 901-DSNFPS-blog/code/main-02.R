
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
    "visualize-fpc-scores.R",
    "visualize-geocoordinates.R",
    "visualizeData-labelled.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.colour.scheme <- data.frame(
    land_cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
    );
rownames(DF.colour.scheme) <- DF.colour.scheme[,"land_cover"];

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot  <- "2020-12-30.01";
data.directory <- file.path(dir.data,data.snapshot,"micro-mission-1","Sentinel1","IW","4");

colname.pattern <- "V";

DF.labelled <- getData.labelled(
    data.directory  = data.directory,
    colname.pattern = colname.pattern,
    land.cover      = DF.colour.scheme[,'land_cover'],
    RData.output    = paste0("data-labelled.RData")
    );

cat("\nstr(DF.labelled)\n");
print( str(DF.labelled)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# logger::log_threshold(level = logger::TRACE);
logger::log_threshold(level = logger::ERROR);

n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot  <- "2020-12-30.01";
data.directory <- file.path(dir.data,data.snapshot,"micro-mission-2");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
for ( temp.variable in c("VH","VV") ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.variable <- DF.labelled[,c("X","Y","date","land_cover",temp.variable)];
    DF.variable[,"X_Y"] <- apply(
        X      = DF.variable[,c('X','Y')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse="_")) }
        );

    cat("\nstr(DF.variable)\n");
    print( str(DF.variable)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.year in c("2017","2018","2019") ) {

        DF.temp.year <- getData(
            data.directory = file.path(data.directory,temp.year),
            output.file    = paste0("data-unlabelled-",temp.variable,"-",temp.year,".RData")
            );

        DF.temp.year <- coregisterData(
            DF.input    = DF.temp.year,
            output.file = paste0("data-unlabelled-",temp.variable,"-",temp.year,"-coregistered.RData")
            );

        cat("\nstr(DF.temp.year)\n");
        print( str(DF.temp.year)   );

        }

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

