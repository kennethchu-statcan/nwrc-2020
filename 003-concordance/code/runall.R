
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
    "initializePlot.R"
    ); 

for ( code.file in code.files ) {
    source(file.path(code.directory,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

DF.concordance <- readr::read_csv(
    file = file.path(data.directory,"QDAD-test-reference-counts.csv")
    );

print( str(DF.concordance) );

print( summary(DF.concordance) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
cor(x = DF.concordance$test,       y = DF.concordance$reference,       method="pearson");

cor(x = DF.concordance$test,       y = DF.concordance$reference,       method="spearman");

cor(x = rank(DF.concordance$test), y = rank(DF.concordance$reference), method="pearson");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.temp <- DF.concordance + matrix(
    rnorm(2 * nrow(DF.concordance), mean = 0, sd = 0.1),
    nrow = nrow(DF.concordance)
    );

print( str(DF.temp) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
PNG.output = 'tmp-pearson-0.png'

my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
my.ggplot <- my.ggplot + theme(legend.position = "none");

my.ggplot <- my.ggplot + geom_point(
    data    = DF.temp,
    mapping = aes(x = reference, y = test, colour = "red"),
    size    = 0.1,
    alpha   = 0.1
    );

ggsave(
    file   = PNG.output,
    plot   = my.ggplot,
    dpi    = 300,
    height =   8,
    width  =   8,
    units  = 'in'
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
PNG.output = 'tmp-spearman-0.png'

my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
my.ggplot <- my.ggplot + theme(legend.position = "none");

my.ggplot <- my.ggplot + geom_point(
    data    = DF.temp,
    mapping = aes(x = rank(reference), y = rank(test), colour = "red"),
    size    = 0.2,
    alpha   = 0.2
    );

ggsave(
    file   = PNG.output,
    plot   = my.ggplot,
    dpi    = 300,
    height =   8,
    width  =   8,
    units  = 'in'
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.concordance <- readr::read_csv(
    file = file.path(data.directory,"ken_fixed_excel.csv")
    );

print( str(DF.concordance) );

print( summary(DF.concordance) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
cor(x = DF.concordance$test,       y = DF.concordance$reference,       method="pearson");

cor(x = DF.concordance$test,       y = DF.concordance$reference,       method="spearman");

cor(x = rank(DF.concordance$test), y = rank(DF.concordance$reference), method="pearson");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.temp <- DF.concordance + matrix(
    rnorm(2 * nrow(DF.concordance), mean = 0, sd = 0.1),
    nrow = nrow(DF.concordance)
    );

print( str(DF.temp) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
PNG.output = 'tmp-pearson-1.png'

my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
my.ggplot <- my.ggplot + theme(legend.position = "none");

my.ggplot <- my.ggplot + geom_point(
    data    = DF.temp,
    mapping = aes(x = reference, y = test, colour = "red"),
    size    = 0.1,
    alpha   = 0.1
    );

ggsave(
    file   = PNG.output,
    plot   = my.ggplot,
    dpi    = 300,
    height =   8,
    width  =   8,
    units  = 'in'
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
PNG.output = 'tmp-spearman-2.png'

my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
my.ggplot <- my.ggplot + theme(legend.position = "none");

my.ggplot <- my.ggplot + geom_point(
    data    = DF.temp,
    mapping = aes(x = rank(reference), y = rank(test), colour = "red"),
    size    = 0.1,
    alpha   = 0.1
    );

ggsave(
    file   = PNG.output,
    plot   = my.ggplot,
    dpi    = 300,
    height =   8,
    width  =   8,
    units  = 'in'
    );

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );

