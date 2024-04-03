library(partykit)

vars = c("sex", "age", "income", "socprof", "ls")
df = SD2011[, vars]

y = df[,"sex"]
x = df[1:floor(dim(df)[1]*0.8),c("age","income", "socprof", "ls")]
xp = df[(floor(dim(df)[1]*0.8)+1):dim(df)[1],c("age","income", "socprof", "ls")]

datact = ctree(y ~ ., data = as.data.frame(cbind(y,x)))

fit.nodes = predict(datact, type = "node")
nodes = unique(fit.nodes)
no.nodes = length(nodes)
pred.nodes = predict(datact, type = "node", newdata = xp)
rowno = 1:length(y)
newrowno = vector("integer", nrow(xp))