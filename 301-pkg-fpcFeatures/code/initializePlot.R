
initializePlot <- function(
    textsize.title = 30,
    textsize.axis  = 20
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.ggplot <- ggplot2::ggplot(data = NULL) + ggplot2::theme_bw();
    output.ggplot <- output.ggplot + ggplot2::theme(
        title            = ggplot2::element_text(size = textsize.title, face = "bold"),
        legend.text      = ggplot2::element_text(size = textsize.axis,  face = "bold"),
        axis.text.x      = ggplot2::element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = ggplot2::element_text(size = textsize.axis,  face = "bold"),
        #axis.title.x    = ggplot2::element_blank(),
        #axis.title.y    = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = ggplot2::element_line(colour="gray", linetype=2, size=0.25),
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::return( output.ggplot );

    }
