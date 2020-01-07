
initializePlot <- function(
    textsize.title = 30,
    textsize.axis  = 20,
    title          = 'my-title',
    subtitle       = 'my-subtitle'
    ) {

    require(ggplot2);

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    my.ggplot <- ggplot(data = NULL) + theme_bw();
    my.ggplot <- my.ggplot + theme(
        title            = element_text(size = textsize.title, face = "bold"),
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
        panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25)
        );

    my.ggplot <- my.ggplot + labs(
        title    = title,
        subtitle = subtitle
        );

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    return( my.ggplot );

    }

