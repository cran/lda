slda.predict <-
function (documents, topics, model, alpha, eta, num.iterations = 100, 
    average.iterations = 50, trace = 0L) 
{
    topic_sums <- as.matrix(as.integer(rowSums(topics)))
    K <- nrow(topics)
    vocab <- colnames(topics)
    result <- lda.collapsed.gibbs.sampler(documents, K, vocab, 
        num.iterations, alpha, eta, trace = trace, initial = list(topics = topics, 
            topic_sums = topic_sums), freeze.topics = TRUE)
    doc_sums_count <- result$document_sums
    for (ii in 1:average.iterations) {
        result <- lda.collapsed.gibbs.sampler(documents, K, vocab, 
            2, alpha, eta, trace = trace, initial = list(assignments = result$assignments, 
                topics = topics, topic_sums = topic_sums), freeze.topics = TRUE)
        doc_sums_count <- result$document_sums + doc_sums_count
    }
    props <- t(doc_sums_count)/colSums(doc_sums_count)
    props %*% coef(model)
}
