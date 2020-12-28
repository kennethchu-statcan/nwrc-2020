
build.package <- function(
    write.to.directory = NULL,
    package.path       = NULL,
    log.threshold      = logger::DEBUG
    ) {

    this.function.name <- "build.package";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.wd <- base::normalizePath(base::getwd());

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::dir.exists(write.to.directory) ) {
        base::dir.create(path = write.to.directory, recursive = TRUE);
        }
    write.to.directory <- base::normalizePath(write.to.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.file <- base::file.path(write.to.directory,base::paste0(this.function.name,".log"));
    logger::log_threshold(level = log.threshold);
    logger::log_appender(logger::appender_tee(file = log.file));
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    package.path <- base::normalizePath(package.path);
    logger::log_info('{this.function.name}(): package.path = {package.path}');

    base::setwd( package.path );
    logger::log_info('{this.function.name}(): getwd() = {getwd()}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(devtools);

    logger::log_info('{this.function.name}(): devtools:build(): starts');
    devtools::build(
        vignettes = TRUE,
        manual    = TRUE,
        quiet     = FALSE
        );
    logger::log_info('{this.function.name}(): devtools:build(): done');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::setwd(initial.wd);
    logger::log_info('{this.function.name}(): exits');
    return( NULL );

    }
