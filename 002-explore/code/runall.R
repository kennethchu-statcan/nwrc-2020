
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
        data.directory  = data.directory,
        beam.swath      = beam.swath,
        colname.pattern = "_cov_matrix_real_comp_",
        FPCA.variable   = "c01_cov_matrix_real_comp_1", #"scaled_Comp2",
        make.heatmaps   = FALSE
        );
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

