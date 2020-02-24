
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
    "getDataStandardizedTimepoints.R",
    "getVariableStems.R",
    "initializePlot.R",
    "reshapeData.R",
    "utils-fpca.R",
    "visualize.R"
    );

for ( code.file in code.files ) {
    source(file.path(code.directory,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(foreach);
require(doParallel);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot  <- "2020-02-24.03";
data.directory <- file.path(data.directory,data.snapshot,"Sentinal1","relative-orbit-number");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

variable.stems <- getVariableStems();
print( variable.stems );

beam.swaths <- list.files(path = data.directory);
print( beam.swaths );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
num.cores <- max(1,detectCores() - 1);
cat("\nnum.cores\n");
print( num.cores   );

registerDoParallel(cores = num.cores);

foreach ( temp.index = 1:length(variable.stems) ) %dopar% {

    base::Sys.sleep(time = 2);
    colname.pattern <- variable.stems[[temp.index]];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.directory     <- getwd();
    temp.output.directory <- file.path(initial.directory,colname.pattern);
    if ( !dir.exists(temp.output.directory) ) {
        dir.create(path = temp.output.directory, recursive = TRUE);
        }
    setwd( temp.output.directory );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    stdout.connection <- "stdout.R.beam-swath-diagnostics";
    stdout.connection <- file(file.path(temp.output.directory,stdout.connection), open = "wt");

    stderr.connection <- "stderr.R.beam-swath-diagnostics";
    stderr.connection <- file(file.path(temp.output.directory,stderr.connection), open = "wt");

    sink(file = stdout.connection, type = "output" );
    sink(file = stderr.connection, type = "message");
    cat("\n");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( beam.swath in beam.swaths ) {
        beam.swath.diagnostics(
            data.directory      = data.directory,
            beam.swath          = beam.swath,
            colname.pattern     = colname.pattern,
            land.types          = c("marsh","swamp","water","forest","ag","shallow"),
	    exclude.years       = "2016",
	    n.partition         = 100,
            n.order             =   3,
            n.basis             =   9,
            smoothing.parameter =   0.1,
            n.harmonics         =   7,
            plot.timeseries     = TRUE,
            plot.heatmaps       = FALSE
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nwarnings()\n");
    print( warnings()   );

    cat("\ngetOption('repos')\n");
    print( getOption('repos')   );

    cat("\n.libPaths()\n");
    print( .libPaths()   );

    cat("\nsessionInfo\n");
    print( sessionInfo() );

    cat("\n");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    sink(file = NULL);
    sink(file = NULL);
    setwd( initial.directory );

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

