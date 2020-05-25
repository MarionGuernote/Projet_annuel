equationIFCy <- function(contentEquation, tokenEquation, edges, nodes, typecontent) {
  
  sourceEq = ""
  targetEq = contentEquation[,1]
  attrEq = contentEquation[,2]
  for (i in 3:ncol(tokenEquation)){
    if (isTRUE(tokenEquation[,i] != "0") && isTRUE(tokenEquation[,i] != "COMMENT")){
      sourceEq = paste(sourceEq,contentEquation[,i])
    }
  }
  
  srce = as.character(sourceEq)
  trg = as.character(targetEq)
  inter = data.frame(target = targetEq, attr = attrEq, source = sourceEq, interaction = c("interacts"), weight = 1)
  edges <- rbind(edges, inter)
  
  node = t(data.frame(target = trg, source = srce))
  colnames(node)[1] = "id"
  nodes = rbind(nodes, node)
  nodes =as.vector(nodes)
  nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
  nodes <- subset(nodes, id != "NA")
  nodes <- subset(nodes, id != "=")
  nodes <- subset(nodes, id != "<-")
  
  bindValueToken = c()
  
  if (isTRUE (typecontent == "IF")) {
    tokenValues = c("IF_CONTENT")
  } else {
    tokenValues = c("FOR_CONTENT")
  }
  
  inter <- cbind(tokenValues,targetEq)
  bindValueToken = rbind(bindValueToken, inter)
  
  inter <- cbind(tokenValues,targetEq)
  bindValueToken = rbind(bindValueToken, inter)
  
  eqAssign <- list("edges" = edges, "nodes" = nodes, "valueToken" = bindValueToken)
  return(eqAssign); 
  
}