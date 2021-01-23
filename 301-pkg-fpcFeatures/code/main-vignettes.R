
command.arguments <- commandArgs(trailingOnly = TRUE);
dir.data <- command.arguments[1];
dir.code <- normalizePath( command.arguments[2] );
dir.out  <- normalizePath( command.arguments[3] );

# add custom library using .libPaths()
cat("\ndir.data: ", dir.data );
cat("\ndir.code: ", dir.code );
cat("\ndir.out:  ", dir.out  );
cat("\n\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

start.proc.time <- proc.time();
setwd( dir.out );

cat("\n##################################################\n");
require(rmarkdown);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
vignette.files <- c(
    "vignette-fpc1-fpc2"
    );

for ( vignette.file in vignette.files ) {
    rmarkdown::render(
        input         = file.path(dir.code,paste0(vignette.file,".Rmd")),
        output_format = "html_document",
        output_file   = file.path(dir.out,paste0(vignette.file,".html"))
        );
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
