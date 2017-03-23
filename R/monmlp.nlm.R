monmlp.nlm <-
function(x, y, hidden1, hidden2=0, iter.max=5000, n.trials=1,
         Th=tansig, To=linear, Th.prime=tansig.prime, To.prime=linear.prime,
         monotone=NULL, init.weights=NULL, max.exceptions=10,
         silent=FALSE, method='BFGS', control=list(trace=0))
{
    control$follow.on <- TRUE
    cost.best <- Inf
    for (i in seq(n.trials)){
        exception <- TRUE
        n.exceptions <- 0
        while (exception){
            exception <- FALSE
            if ((length(init.weights)==2) || is.null(init.weights)){
                weights <- monmlp.initialize(x=x, y=y, hidden1=hidden1,
                               hidden2=hidden2, init.weights=init.weights)
            } else{
                weights <- init.weights
            }
            output.mlp <- try(suppressWarnings(optimx(par=weights,
                              fn=monmlp.cost, gr=monmlp.grad, method=method,
                              itnmax=iter.max, control=control, xx=x, yy=y,
                              hidden1=hidden1, hidden2=hidden2, Th=Th, To=To,
                              Th.prime=Th.prime, To.prime=To.prime, 
                              monotone=monotone)),
                              silent=FALSE)
            if (class(output.mlp)[[1]]=="try-error"){
                exception <- TRUE
                n.exceptions <- n.exceptions + 1
                if (n.exceptions > max.exceptions)
                    stop("maximum number of exceptions reached")
            }
        }
        output.mlp <- output.mlp[nrow(output.mlp),]
        weights <- unlist(output.mlp[seq_along(weights)])
        code <- output.mlp$conv
        cost <- output.mlp$value
        if(!silent) cat(cost, '\n')
        if (cost < cost.best){
            weights.best <- weights
            cost.best <- cost
            code.best <- code
        }
    }
    list(weights=weights.best, cost=cost.best, code=code.best)
}
