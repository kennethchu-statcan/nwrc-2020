
getVariableStems <- function() {

    thisFunctionName <- "getVariableStems";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- list(
#       polarization           = c("cov_matrix_real_comp"),
#       intensity_ratio        = c("intensity_ratio"),
#       FreeDur                = c("FreeDur"),
#       yamaguchi              = c("Yamaguchi"),
#       entropy                = c("entropy_shannon"),
#       HH_VV                  = c("HH_VV_"),
#       CloudPott              = c("CloudPott"),
        Touz                   = c("Touz_"),
        TouzDisc               = c("TouzDisc"),
        CircBasisChange        = c("CircBasisChange"),
#       total_power            = c("total_power"),
#       pedestal_height        = c("pedestal_height"),
#       phase_difference_HH_VV = c("phase_difference_HH_VV")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

