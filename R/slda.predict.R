slda.predict <-
function (documents, topics, model, alpha = 1/nrow(topics), num.iterations = 5) 
{
    word.assignments <- sapply(documents, function(doc) {
        colSums(t(topics[, (doc[1, ] + 1)]) * doc[2, ])
    })
    doc.assignments <- t(word.assignments)/colSums(word.assignments)
    for (ii in 1:num.iterations) {
        doc.assignments <- word.assignments * t(doc.assignments + 
            alpha)
        doc.assignments <- t(doc.assignments)/colSums(doc.assignments)
    }
    doc.assignments %*% coef(model)
}
