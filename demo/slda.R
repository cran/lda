set.seed(8675309)

## Use the political blogs data set.
data(poliblog.documents)
data(poliblog.vocab)
data(poliblog.ratings)

num.topics <- 10

## Initialize the params
params <- sample(c(-1, 1), num.topics, replace=TRUE)

result <- slda.em(documents=poliblog.documents,
                  K=num.topics,
                  vocab=poliblog.vocab,
                  num.e.iterations=10,
                  num.m.iterations=4,
                  alpha=1.0, eta=0.1,
                  poliblog.ratings / 100,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=FALSE,
                  method="sLDA")

## Make a pretty picture.
require("ggplot2")
Topics <- apply(top.topic.words(result$topics, 5, by.score=TRUE),
                2, paste, collapse=" ")
coefs <- data.frame(coef(summary(result$model)))
theme_set(theme_bw())
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
coefs <- coefs[order(coefs$Estimate),]
qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
  geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,
                  ymax=Estimate+Std..Error)) + coord_flip()


slda.predict <- function(documents, topics, model,
                         alpha = 1.0 / nrow(topics),
                         num.iterations = 5) {
  ## Get the document matrix for each document
  word.assignments <- sapply(documents,
                            function(doc) {
                              colSums(t(topics[,(doc[1,] + 1)]) * doc[2,])
                            })
  doc.assignments <- t(word.assignments) / colSums(word.assignments)

  for (ii in 1:num.iterations) {
    doc.assignments <- word.assignments * t(doc.assignments + alpha)
    doc.assignments <- t(doc.assignments) / colSums(doc.assignments)
  }

  doc.assignments %*% coef(model)
}

predictions <- slda.predict(poliblog.documents,
                            result$topics / as.vector(result$topic_sums), 
                            result$model)

qplot(predictions,
      fill=factor(poliblog.ratings),
      xlab = "predicted rating",
      ylab = "density",
      alpha=I(0.5),
      geom="density") +
  geom_vline(aes(xintercept=0)) +
  opts(legend.position = "none")
