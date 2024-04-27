require("ggplot2")
require("lda")

theme_set(theme_bw())

data(cora.documents)
data(cora.vocab)
data(cora.titles)
data(cora.cites)

## Fit an RTM model.
system.time(rtm.model <- rtm.collapsed.gibbs.sampler(cora.documents,
                                         cora.cites,
                                         8,
                                         cora.vocab,
                                         35,
                                         0.1, 0.1, 3))

## Fit an LDA model to the topics
result <- lda.collapsed.gibbs.sampler(cora.documents,
                                      8, 
                                      cora.vocab,
                                      25,
                                      0.1,
                                      0.1,
                                      initial =
                                      list(topics = rtm.model$topics,
                                           topic_sums = matrix(rtm.model$topic_sums)))
                                   
                                   


## Fit an LDA model by setting beta to zero.
lda.model <- rtm.collapsed.gibbs.sampler(cora.documents,
                                         cora.cites,
                                         8,
                                         cora.vocab,
                                         35,
                                         0.1, 0.1, 0)


## Randomly sample 100 edges.
edges <- links.as.edgelist(cora.cites)

sampled.edges <- edges[sample(dim(edges)[1], 100),]

rtm.similarity <- predictive.link.probability(sampled.edges,
                                              rtm.model$document_sums,
                                              0.1, 3)
lda.similarity <- predictive.link.probability(sampled.edges,
                                              lda.model$document_sums,
                                              0.1, 3)

## Compute how many times each document was cited.
cite.counts <- table(factor(edges[,1],
                            levels=1:dim(rtm.model$document_sums)[2]))

## And which topic is most expressed by the cited document.
max.topic <- apply(rtm.model$document_sums, 2, which.max)

res <- data.frame(lda.similarity, 
                  rtm.similarity,
                  cites = c(cite.counts[sampled.edges[,1]]),
                  topic = factor(max.topic[sampled.edges[,2]]))
ggplot(res, 
        aes(x = lda.similarity,
            y = rtm.similarity,
            size = log(cites),
            colour = topic)) +
  geom_point() +
  xlab("LDA predicted link probability") +
  ylab("RTM predicted link probability") +      
  xlim(c(0,1)) + ylim(c(0,1)) +
  scale_size(name="log(Number of citations)") +
  scale_colour_hue(name="Max RTM topic of citing document")


