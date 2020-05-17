
## Play with set overlaps

library(RVenn)

set_sizes <- c(200, 100, 50)

sets1 <- lapply(set_sizes,
                function(n) sample(1:250,
                                   size = n,
                                   replace = FALSE))
sets2 <- lapply(set_sizes,
                function(n) sample(200:750,
                                   size = n,
                                   replace = FALSE))


data <- data.frame(set = unlist(mapply(function(x, y) rep(x, y),
                                       1:(2 * length(set_sizes)), c(set_sizes, set_sizes))),
                   item = c(unlist(sets1), unlist(sets2)))



v <- Venn(c(sets1, sets2))
setmap(v, element_clustering = FALSE, set_clustering = FALSE)

ggvenn(v, slice = 1:3)
ggvenn(v, slice = 4:6)
ggvenn(v, slice = 3:5)
