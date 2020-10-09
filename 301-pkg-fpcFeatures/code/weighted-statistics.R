
weighted.mean <- function(x = NULL, weights = NULL) {
    base::return( base::sum( weights * x ) / base::sum(weights) );
    }

weighted.var <- function(x = NULL, weights = NULL) {

    my.weighted.mean <- weighted.mean(x = x, weights = weights);
    pre.weighted.var <- base::sum( weights * ((x - my.weighted.mean)^2) ) / base::sum(weights) ;

    n.nonzero.weights <- base::sum( weights > 0 );
    my.factor <- n.nonzero.weights / (n.nonzero.weights - 1); 

    base::return( my.factor * pre.weighted.var );

    }

weighted.sd <- function(x = NULL, weights = NULL) {
    base::return( base::sqrt( weighted.var(x = x, weights = weights) ) );
    }

