
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
    "visualize-fpc-approximations.R",
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
land.covers.english <- c("marsh",  "swamp",    "water", "forest", "ag", "shallow"     );
land.covers.french  <- c("marais", "marécage", "eaux",  "forêt",  "ag", "peu.profonde");
Encoding(land.covers.french) <- "UTF-8";

DF.land.covers <- data.frame(
    english = land.covers.english,
    french  = land.covers.french
    );

DF.colour.scheme <- data.frame(
    land_cover = land.covers.english,
  # land_cover = land.covers.french,
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red")
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
cat("\ntable(DF.labelled[,'land_cover'])\n");
print( table(DF.labelled[,'land_cover'])   );

DF.labelled[,'land_cover'] <- as.character(DF.labelled[,'land_cover']);

cat("\nstr(DF.labelled)\n");
print( str(DF.labelled)   );

cat("\nsummary(DF.labelled)\n");
print( summary(DF.labelled)   );

cat("\ntable(DF.labelled[,'land_cover'])\n");
print( table(DF.labelled[,'land_cover'])   );

cat("\nsum(is.na(DF.labelled[,'land_cover']))\n");
print( sum(is.na(DF.labelled[,'land_cover']))   );

for ( i in 1:nrow(DF.land.covers) ) {
    DF.labelled[DF.labelled[,'land_cover'] == DF.land.covers[i,'english'],'land_cover'] <- DF.land.covers[i,'french'];
    }

DF.labelled[,'land_cover'] <- factor(
    x      = as.character(DF.labelled[,'land_cover']),
    levels = DF.land.covers[,'french' ]
    );

cat("\nstr(DF.labelled)\n");
print( str(DF.labelled)   );

DF.colour.scheme <- data.frame(
  # land_cover = land.covers.english,
    land_cover = land.covers.french,
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red")
    );
rownames(DF.colour.scheme) <- DF.colour.scheme[,"land_cover"];

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
diagnostics.directory <- file.path(getwd(),"data-labelled-diagnostics");
if ( !dir.exists(diagnostics.directory) ) {
    dir.create(path = diagnostics.directory, recursive = TRUE);
    }

visualizeData.labelled(
    DF.input         = DF.labelled,
    colname.pattern  = colname.pattern,
    DF.colour.scheme = DF.colour.scheme,
    output.directory = diagnostics.directory
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# logger::log_threshold(level = logger::TRACE);
logger::log_threshold(level = logger::ERROR);

n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
approximations.directory <- file.path(getwd(),"data-labelled-approximations");
if ( !dir.exists(approximations.directory) ) {
    dir.create(path = approximations.directory, recursive = TRUE);
    }

fpc.directory <- file.path(getwd(),"data-labelled-fpc");
if ( !dir.exists(fpc.directory) ) {
    dir.create(path = fpc.directory, recursive = TRUE);
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my.variables <- c(
    "VH",           "VV",
    "VH_scaled",    "VV_scaled",
    "V_opc1",       "V_opc2",
    "V_opc1_scaled","V_opc2_scaled"
    );

for ( temp.variable in my.variables ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.variable <- DF.labelled[,c("X","Y","date","land_cover",temp.variable)];
    DF.variable[,"X_Y"] <- apply(
        X      = DF.variable[,c('X','Y')],
        MARGIN = 1,
        FUN    = function(x) { return( paste(x,collapse="_") ) }
        );

    cat("\nstr(DF.variable)\n");
    print( str(DF.variable)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.fpcFeatureEngine <- fpcFeatures::fpcFeatureEngine$new(
        training.data       = DF.variable,
        location            = 'X_Y',
        date                = 'date',
        variable            = temp.variable,
        n.partition         = n.partition,
        n.order             = n.order,
        n.basis             = n.basis,
        smoothing.parameter = smoothing.parameter,
        n.harmonics         = n.harmonics
        );

    my.fpcFeatureEngine$fit();

    saveRDS(
        object = my.fpcFeatureEngine,
        file   = file.path(fpc.directory,paste0("fpc-",temp.variable,"-FeatureEngine.RData"))
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ggplot2::ggsave(
        file   = file.path(fpc.directory,paste0("fpc-",temp.variable,"-harmonics.png")),
        plot   = my.fpcFeatureEngine$plot.harmonics(),
        dpi    = 150,
        height =   4 * n.harmonics,
        width  =  16,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.bspline.fpc <- my.fpcFeatureEngine$transform(
        newdata  = DF.variable,
        location = 'X_Y',
        date     = 'date',
        variable = temp.variable
        );

    selected.colnames <- grep(x = colnames(DF.bspline.fpc), pattern = "^[0-9]+$", invert = TRUE);
    DF.fpc.scores     <- DF.bspline.fpc[,selected.colnames];

    cat("\nstr(DF.fpc.scores)\n");
    print( str(DF.fpc.scores)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.land.cover <- unique(DF.variable[,c("X_Y","land_cover")]);

    DF.fpc.scores <- merge(
        x  = DF.fpc.scores,
        y  = DF.land.cover,
        by = "X_Y"
        );

    cat("\nstr(DF.fpc.scores)\n");
    print( str(DF.fpc.scores)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualize.fpc.approximations(
        featureEngine    = my.fpcFeatureEngine,
        DF.variable      = DF.variable,
        location         = 'X_Y',
        date             = 'date',
        land.cover       = 'land_cover',
        variable         = temp.variable,
        DF.colour.scheme = DF.colour.scheme,
        output.directory = approximations.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualize.fpc.scores(
        variable         = temp.variable,
        DF.fpc.scores    = DF.fpc.scores,
        DF.colour.scheme = DF.colour.scheme,
        output.directory = fpc.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

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
