monmlp.initialize <-
function(x, y, hidden1, hidden2, init.weights)
{
    if(is.null(init.weights)){
        calc.range <- TRUE
        init.weights <- c(-0.8/sqrt(ncol(x)+1), 0.8/sqrt(ncol(x)+1))
    } else{
        calc.range <- FALSE
    }
    w.min <- min(init.weights)
    w.max <- max(init.weights)
    W1 <- matrix(runif((ncol(x)+1)*hidden1, w.min, w.max),
                 ncol(x)+1, hidden1)
    if (hidden2==0){
        W2 <- matrix(runif((hidden1+1)*ncol(y), -0.5, 0.5),
                     hidden1+1, ncol(y))
        W.vec <- c(W1, W2)
    } else{
        if(calc.range){
            w.min <- -0.8/sqrt(hidden1+1)
            w.max <- 0.8/sqrt(hidden1+1)
        }
        W2 <- matrix(runif((hidden1+1)*hidden2, w.min, w.max),
                     hidden1+1, hidden2)
        W3 <- matrix(runif((hidden2+1)*ncol(y), -0.5, 0.5),
                     hidden2+1, ncol(y))
        W.vec <- c(W1, W2, W3)
    }
    W.vec
}
