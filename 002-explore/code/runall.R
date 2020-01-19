
command.arguments <- commandArgs(trailingOnly = TRUE);
data.directory    <- normalizePath(command.arguments[1]);
code.directory    <- normalizePath(command.arguments[2]);
output.directory  <- normalizePath(command.arguments[3]);

# add custom library using .libPaths()
print( data.directory   );
print( code.directory   );
print( output.directory );
print( format(Sys.time(),"%Y-%m-%d %T %Z") );

start.proc.time <- proc.time();

# set working directory to output directory
setwd( output.directory );

##################################################
# source supporting R code
code.files <- c(
    "beam-swath-diagnostics.R",
    "doFPCA.R",
    "doPCA.R",
    "getData.R",
    "initializePlot.R",
    "reshapeData.R",
    "utils-fpca.R",
    "visualize.R"
    );

for ( code.file in code.files ) {
    source(file.path(code.directory,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

beam.swaths <- list.files(path = data.directory);
print( beam.swaths );

for ( beam.swath in beam.swaths ) {
    beam.swath.diagnostics(
        data.directory = data.directory,
        beam.swath     = beam.swath
        );
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
quit(save="no");
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

RData.raw      <- "data-raw.RData";
RData.reshaped <- "data-reshaped.RData";

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# read and convert data to tabular format
list.data_raw <- getData(
    data_folder = file.path(data.directory),
    output_file = RData.raw
    );

cat("\nstr(list.data_raw)\n");
print( str(list.data_raw)   );

##################################################
##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

##################################################
##################################################

list.data_reshaped <- reshapeData(
    list_input  = list.data_raw,
    output_file = RData.reshaped
    );

cat("\nstr(list.data_reshaped)\n");
print( str(list.data_reshaped)   );

visualize(
    list_input = list.data_reshaped
    );

DF.pca <- doPCA(
    list.input = list.data_reshaped,
    make.plots = TRUE
    );

cat("\nstr(DF.pca)\n");
print( str(DF.pca)   );

DF.fpca <- doFPCA(
    DF.input            = DF.pca,
    target.variable     = "scaled_Comp.1",
    spline.grid         = NULL,
    n.order             = 3,
    n.basis             = 9,
    smoothing.parameter = 0.1,
    n.harmonics         = 7,
    FILE.output.RData   = "tmp-FPCA-scaled-Comp1.RData",
    FILE.output.csv     = "tmp-FPCA-scaled-Comp1.csv"
    );

cat("\nstr(DF.fpca)\n");
print( str(DF.fpca)   );

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

