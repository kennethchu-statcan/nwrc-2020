
assemble.package <- function(
    write.to.directory = NULL,
    package.name       = NULL,
    copyright.holder   = "Kenneth Chu",
    description.fields = base::list(),
    packages.import    = base::c(),
    packages.depend    = base::c(),
    packages.suggest   = base::c(),
    packages.enhance   = base::c(),
    files.R            = base::c(),
    tests.R            = base::c(),
    list.vignettes.Rmd = base::list(),
    list.vignettes.pdf = base::list(),
    images.png         = base::c(),
    log.threshold      = logger::DEBUG
    ) {

    this.function.name <- "assemble.package";

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
    logger::log_info('{this.function.name}(): .libPaths:\n{paste0(.libPaths(),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    devtools::reload(pkgload::inst("usethis"));
    base::require(devtools);
    base::require(roxygen2);
    base::require(rmarkdown);
    base::require(testthat);
    base::require(R6);
    base::require(dplyr);
    base::require(ggplot2);
    base::require(e1071);
    base::require(stats);

    # ~~~~~~~~~~ #
    path.package <- base::file.path(write.to.directory,package.name);

    testthat::with_mock(
        usethis::create_package(
            path    = path.package,
            fields  = description.fields,
            rstudio = FALSE,
            open    = FALSE
            ),
        check_not_nested = function(path, name) return(),
        .env = "usethis"
        );

    path.package <- base::normalizePath(path.package);
    base::setwd( path.package );

    # ~~~~~~~~~~ #
    usethis::use_mit_license(name = copyright.holder);
    usethis::use_testthat();

    # ~~~~~~~~~~ #
    logger::log_info('{this.function.name}(): packages.import:\n{ paste0(packages.import, collapse="\n")}');
    for ( temp.package in packages.import ) {
        usethis::use_package(package = temp.package, type = "Imports");
        }

    logger::log_info('{this.function.name}(): packages.depend:\n{ paste0(packages.depend, collapse="\n")}');
    for ( temp.package in packages.depend ) {
        usethis::use_package(package = temp.package, type = "Depends");
        }

    logger::log_info('{this.function.name}(): packages.suggest:\n{paste0(packages.suggest,collapse="\n")}');
    for ( temp.package in packages.suggest ) {
        usethis::use_package(package = temp.package, type = "Suggests");
        }

    logger::log_info('{this.function.name}(): packages.enhance:\n{paste0(packages.enhance,collapse="\n")}');
    for ( temp.package in packages.enhance ) {
        usethis::use_package(package = temp.package, type = "Enhances");
        }

    # ~~~~~~~~~~ #
    for ( temp.file.R in files.R ) {
        base::file.copy(
            from = temp.file.R,
            to   = base::file.path(".","R")
            );
        }

    # ~~~~~~~~~~ #
    for ( temp.test.R in tests.R ) {
        base::file.copy(
            from = temp.test.R,
            to   = base::file.path(".","tests","testthat")
            );
        }

    # ~~~~~~~~~~ #
    vignettes.directory <- base::file.path(".","vignettes");
    if ( !dir.exists(vignettes.directory) ) {
        dir.create(
            path      = vignettes.directory,
            recursive = TRUE
            );
        }

    doc.directory <- base::file.path(".","doc");
    if ( !dir.exists(doc.directory) ) {
        dir.create(
            path      = doc.directory,
            recursive = TRUE
            );
        }

    inst.doc.directory <- base::file.path(".","inst","doc");
    if ( !dir.exists(doc.directory) ) {
        dir.create(
            path      = doc.directory,
            recursive = TRUE
            );
        }

    # ~~~~~~~~~~ #
    for ( temp.vignette in list.vignettes.pdf ) {
        logger::log_info('{this.function.name}(): processing PDF vignette: file = {temp.vignette[["file"]]}, asis = {temp.vignette[["asis"]]}');
        base::file.copy(
            from      = temp.vignette[['file']],
            to        = vignettes.directory,
            overwrite = TRUE
            );
        base::file.copy(
            from      = temp.vignette[['asis']],
            to        = vignettes.directory,
            overwrite = TRUE
            );
        }

    # ~~~~~~~~~~ #
    for ( temp.vignette in list.vignettes.Rmd ) {
        logger::log_info('{this.function.name}(): processing HTML vignette: file = {temp.vignette[["file"]]}, asis = {temp.vignette[["asis"]]}');
        rmarkdown::render(
            input         = temp.vignette[['file']],
            output_format = "html_document",
            output_dir    = vignettes.directory,
            output_file   = base::gsub(x = temp.vignette[['file']],pattern="\\.Rmd$",replacement=".html")
            );
        base::file.copy(
            from      = temp.vignette[['asis']],
            to        = vignettes.directory,
            overwrite = TRUE
            );
        }

    # for ( temp.image.png in images.png ) {
    #     base::file.copy(
    #         from = temp.image.png,
    #         to   = vignettes.directory
    #         );
    #     }

    # ~~~~~~~~~~ #
    devtools::document();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::setwd(initial.wd);
    logger::log_info('{this.function.name}(): exits');
    return( path.package );

    }
