monmlp.cost <-
function(weights, xx, yy, hidden1, hidden2, Th, To, Th.prime, To.prime,
         monotone=NULL)
{
    w <- monmlp.reshape(x=xx, y=yy, weights=weights, hidden1=hidden1,
                        hidden2=hidden2)
    W1 <- w$W1; W2 <- w$W2
    if (hidden2 > 0) W3 <- w$W3
    if (!is.null(monotone)){
        W1[monotone,] <- exp(W1[monotone,])
        W2[1:(nrow(W2)-1),] <- exp(W2[1:(nrow(W2)-1),])
        if(hidden2 > 0) W3[1:(nrow(W3)-1),] <- exp(W3[1:(nrow(W3)-1),])
    }
    xx <- cbind(xx, 1)
    h1 <- xx %*% W1
    y1 <- Th(h1)
    aug.y1 <- cbind(y1, 1)
    h2 <- aug.y1 %*% W2
    if (hidden2==0){
        y2 <- To(h2)
        E <- yy-y2
    } else{
        y2 <- Th(h2)
        aug.y2 <- cbind(y2, 1)
        h3 <- aug.y2 %*% W3
        y3 <- To(h3)
        E <- yy-y3
    }
    cost <- sum(E^2)/length(E)
    cost
}
