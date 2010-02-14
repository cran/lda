slda.em <-
function (documents, K, vocab, num.e.iterations, num.m.iterations, 
    alpha, eta, annotations, params, variance, logistic = FALSE, 
    lambda = 10, method = "sLDA", trace = 0L) 
{
    estimate.params <- function(document.sums, num.topics, logistic) {
        z.bar. <- t(document.sums)/colSums(document.sums)
        if (logistic) {
            model.fit <- glm(annotations ~ z.bar. + 0, family = binomial())
        }
        else {
            model.fit <- lm(annotations ~ z.bar. + 0)
        }
        list(model = model.fit, coefs = coef(model.fit))
    }
    method <- rep(method, length.out = num.m.iterations)
    variance <- rep(variance, length.out = num.m.iterations)
    num.e.iterations <- rep(num.e.iterations, length.out = num.m.iterations)
    result <- .slda.collapsed.gibbs.sampler(documents, K, vocab, 
        num.e.iterations[1], alpha, eta, annotations, params, 
        variance[1], logistic, method[1], lambda = lambda, trace = trace)
    fit <- estimate.params(result$document_sums, K, logistic)
    params <- fit$coefs
    for (ii in seq(length.out = num.m.iterations - 1)) {
        result <- .slda.collapsed.gibbs.sampler(documents, K, 
            vocab, num.e.iterations[ii + 1], alpha, eta, annotations, 
            params, variance[ii + 1], logistic, method[ii + 1], 
            lambda = lambda, initial = list(assignments = result$assignments), 
            trace = trace)
        fit <- estimate.params(result$document_sums, K, logistic)
        params <- fit$coefs
    }
    c(result, fit)
}
