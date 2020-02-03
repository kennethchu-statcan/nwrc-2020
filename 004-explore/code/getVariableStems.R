
getVariableStems <- function() {

    thisFunctionName <- "getVariableStems";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- list(
        polarization    = c("cov_matrix_real_comp"),
	intensity_ratio = c("intensity_ratio"),
	FreeDur         = c("FreeDur"),
	yamaguchi       = c("Yamaguchi"),
	entropy         = c("entropy_shannon")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

