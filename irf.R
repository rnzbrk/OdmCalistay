irf <- function(theta = 0, a = 1, b = 0, cc = 0, type = "3pl") {
    n <- length(a)
    prob <- 0
    if (type == "3pl") {
        for (i in 1:n) {
            prob <- prob + cc[i] + (1 - cc[i])/(1 + exp(-a[i] * (theta - b[i])))
        }
    } else {
        for (i in 1:n) {
            prob <- prob + cc[i] + (1 - cc)[i] * pnorm(a[i] * (theta - b[i]))
        }
    }
    return(prob)
}
