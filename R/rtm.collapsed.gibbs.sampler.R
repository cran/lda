rtm.collapsed.gibbs.sampler <-
function (documents, links, K, vocab, num.iterations, alpha, 
    eta, beta) 
{
    retval <- structure(.Call("rtm", documents, links, as.integer(K), 
        length(vocab), as.integer(num.iterations), as.double(alpha), 
        as.double(eta), rep(as.double(beta), length.out = K)), 
        names = c("assignments", "topics", "topic_sums", "document_sums"))
    colnames(retval$topics) <- vocab
    retval
}
