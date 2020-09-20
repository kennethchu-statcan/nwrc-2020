
getVariableStems <- function(
    satellites = c("sentinel","radarsat")
    ) {

    thisFunctionName <- "getVariableStems";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( "sentinel" == tolower(satellites) ) {

        list.output <- list(
            polarization = c("V")
            );

    } else if ( "radarsat" == tolower(satellites) ) {

        list.output <- list(
            polarization = c("cov_matrix_real_comp"),
            dBZ          = c("dBZ_cov_comp")
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }
