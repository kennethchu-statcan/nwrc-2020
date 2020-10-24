
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
    "getData-beam-mode.R",
    "getData.R",
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
satellites     <- "sentinel";
data.snapshot  <- "2020-02-24.03";
data.directory <- file.path(dir.data,data.snapshot,"Sentinal1","relative-orbit-number");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
n.partition         <- 100;
n.order             <-   3;
n.basis             <-   9;
smoothing.parameter <-   0.1;
n.harmonics         <-   7;

DF.colour.scheme <- data.frame(
    land.cover = c("marsh",  "swamp",  "water",  "forest", "ag",     "shallow"),
    colour     = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","red"    )
    );
rownames(DF.colour.scheme) <- DF.colour.scheme[,"land.cover"];

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
beam.mode           <- "IW4";
beam.mode.directory <- file.path(data.directory,beam.mode);
colname.pattern     <- "V";

DF.data <- getData.beam.mode(
    data.directory     = beam.mode.directory,
    satellites         = satellites,
    beam.mode          = beam.mode,
    colname.pattern    = colname.pattern,
    land.cover         = DF.colour.scheme[,'land.cover']
#  ,exclude.years      = exclude.years,
#   exclude.land.types = exclude.land.types
    );

cat("\nstr(DF.data)\n");
print( str(DF.data)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
is.not.2016 <- ("2016" != DF.data[,'year']);
IW4 <- DF.data[is.not.2016,c(c("X","Y","date","type","VV","VH"))];
colnames(IW4) <- gsub(x = colnames(IW4), pattern = "^X$",    replacement = "x");
colnames(IW4) <- gsub(x = colnames(IW4), pattern = "^Y$",    replacement = "y");
colnames(IW4) <- gsub(x = colnames(IW4), pattern = "^type$", replacement = "land_cover");
saveRDS(object = IW4, file = "IW4.RData");

cat("\nstr(IW4)\n");
print( str(IW4)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.VV <- IW4[,c("x","y","date","VV")];
DF.VV[,"x_y"] <- apply(
    X      = DF.VV[,c('x','y')],
    MARGIN = 1,
    FUN    = function(x) { return(paste(x,collapse="_")) }
    );
colnames(DF.VV) <- paste0("my_",colnames(DF.VV));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my.fpcFeatureEngine <- fpcFeatureEngine$new(
    training.data       = DF.VV,
    location            = 'my_x_y',
    date                = 'my_date',
    variable            = 'my_VV',
    n.partition         = n.partition,
    n.order             = n.order,
    n.basis             = n.basis,
    smoothing.parameter = smoothing.parameter,
    n.harmonics         = n.harmonics
    );

my.fpcFeatureEngine$fit();

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
ggplot2::ggsave(
    file   = "plot-harmonics.png",
    plot   = my.fpcFeatureEngine$plot.harmonics(),
    dpi    = 150,
    height =   4 * n.harmonics,
    width  =  16,
    units  = 'in'
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.bspline.fpc <- my.fpcFeatureEngine$transform(newdata = DF.VV);
# cat("\nstr(DF.bspline.fpc)\n");
# print( str(DF.bspline.fpc)   );
#
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.temp <- DF.bspline.fpc[,setdiff(colnames(DF.bspline.fpc),c('my_x_y','year'))];
# DF.temp <- t(DF.temp);
#DF.temp[,'date_index'] <- as.numeric(rownames(DF.temp));
#DF.temp[,'date_index'] <- rep(-999,nrow(DF.temp));
#DF.temp <- DF.temp[,c('date_index',setdiff(colnames(DF.temp),'date_index'))];

# cat("\nstr(DF.temp)\n");
# print( str(DF.temp)   );
#
# print( DF.temp[,c(1,2)] );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.temp              <- DF.data[c("X","Y","year","date","VV")];
is.selected.year     <- ("2017" == DF.temp[,'year']);
DF.temp              <- DF.temp[is.selected.year,];

DF.temp[,"x_y"] <- apply(
    X      = DF.temp[,c('X','Y')],
    MARGIN = 1,
    FUN    = function(x) { return(paste(x,collapse="_")) }
    );

is.selected.location <- ("305296.039198242_4866461.06866592" == DF.temp[,'x_y']);
DF.temp              <- DF.temp[is.selected.location,];

DF.temp <- DF.temp[,c("x_y","date","VV")];

print( str(DF.temp) );

my.ggplot <- my.fpcFeatureEngine$plot.approximations(
    DF.input = DF.temp,
    location = "x_y",
    date     = "date",
    variable = "VV"
    );

ggplot2::ggsave(
    file   = "plot-approximations.png",
    plot   = my.ggplot,
    dpi    = 150,
    height =   6,
    width  =  16,
    units  = 'in'
    );

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
