links.as.edgelist <-
function (links) 
{
    cbind(rep(1:length(links), sapply(links, length)), unlist(links) + 
        1)
}
