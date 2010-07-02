\name{slda.predict}
\alias{slda.predict}
\title{
  Predict the response variable of documents using an sLDA model.
}
\description{
    This function takes a fitted sLDA model and predicts the value of
    the response variable for each given document.  
}
\usage{
slda.predict(documents, topics, model, alpha = 1/nrow(topics), num.iterations = 5)
}
\arguments{
  \item{documents}{
    A list of document matrices comprising a corpus, in the format
    described in \code{\link{lda.collapsed.gibbs.sampler}}.
}
\item{topics}{
    A \eqn{K \times V} matrix where each entry is a numeric proportional
    to the probability of seeing the word (column) conditioned on topic
    (row) (this entry is sometimes denoted \eqn{\beta_{w,k}} in the
    literature, see details).  The column names should correspond to the
    words in the vocabulary.  The \var{topics} field from the output of
    \code{\link{slda.em}} can be used.  
}
\item{model}{
  A fitted model relating a document's topic distribution to the
  response variable.  The \var{model} field from the output of
  \code{\link{slda.em}} can be used.
}
\item{alpha}{
  The scalar value of the Dirichlet hyperparameter for
  topic proportions.  See references for details.
}
\item{num.iterations}{
  Number of iterations of inference to perform on the documents.
}
}
\details{
  Inference is first performed on the documents to map them to the topic
  space.  Typically for a well-fit model only a small number of
  iterations are required to obtain good fits for new documents.  These
  topic vectors are then piped through \code{model} to yield numeric
  predictions associated with each document.
}
\value{
  A numeric vector of the same length as \code{documents} giving the predictions.
}
\references{
  \cite{Blei, David M. and McAuliffe, John.  Supervised topic models.
    Advances in Neural Information Processing Systems, 2008.}
}
\author{
  Jonathan Chang (\email{jonchang@facebook.com})
}

\seealso{
  See \code{\link{lda.collapsed.gibbs.sampler}} for a description of the
  format of the input data, as well as more details on the model.
  
  See \code{\link{predictive.distribution}} if you want to make
  predictions about the contents of the documents instead of the
  response variables.
}
\examples{
## The sLDA demo shows an example usage of this function.
\dontrun{demo(slda)}
}
\keyword{ utilities }
