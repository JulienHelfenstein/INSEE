library(synthpop)


vars = c("sex", "age", "income", "socprof", "ls")
df = SD2011[, vars]

y = df[,"sex"]
x = df[,c("age","income", "socprof", "ls")]


syn.ctree <- function(y, x, xp, smoothing = "", proper = FALSE, minbucket = 5, mincriterion = 0.9, ...)
{ 
  if (proper == TRUE) {
    s <- sample(length(y), replace = truehist())
    y <- y[s]
    x <- x[s, , drop = FALSE]
  }
  
  for (i in which(sapply(x, class) != sapply(xp,class))) xp[,i] <-
      eval(parse(text = paste0("as.", class(x[,i]), "(xp[,i])", sep = "")))

  # Fit a tree
#  datact     <- partykit::ctree(y ~ ., data = as.data.frame(cbind(y, x)), 
#                  control = partykit::ctree_control(minbucket = minbucket, 
#                                                    mincriterion = mincriterion, ...))
  datact <- ctree(y ~ ., data = as.data.frame(cbind(y,x)), controls = ctree_control(minbucket = minbucket, mincriterion = mincriterion, ...))
  
# fit.nodes  <- predict(datact, type = "node")
  fit.nodes  <- where(datact)
  nodes      <- unique(fit.nodes)
  no.nodes   <- length(nodes)
# pred.nodes <- predict(datact, type = "node", newdata = xp)
  pred.nodes <- where(datact, newdata = xp)
  # Get row numbers for predicted by sampling with replacement from existing data
  rowno      <- 1:length(y)
  newrowno   <- vector("integer", nrow(xp))
  
  for (i in nodes) {
    newrowno[pred.nodes == i] <- sample(rowno[fit.nodes == i],
                                        length(newrowno[pred.nodes == i]),
                                        replace = TRUE)
  }
  new <- y[newrowno]
  
  if (!is.factor(y) & smoothing != "") new <- 
    syn.smooth(new, y, smoothing = smoothing )
  
  return(list(res = new, fit = datact))
}