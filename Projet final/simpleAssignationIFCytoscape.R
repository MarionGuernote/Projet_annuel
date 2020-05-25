SimpleAssignationIFCy <- function(contentAssignation, tokenAssignation, edges, nodes, typecontent) {
  
  tabsimpleAssignationOpe = c()
  tabTokenAssign = c()
  i = 1
  while ( i < length(tokenAssignation) ){
    if((isTRUE((contentAssignation[i+1]) == '+')) || (isTRUE((contentAssignation[i+1]) == '-')) || (isTRUE((contentAssignation[i+1]) == '/')) ||(isTRUE((contentAssignation[i+1]) == '*'))) {
      tabsimpleAssignationOpe[i]=paste(contentAssignation[i],contentAssignation[i+1],contentAssignation[i+2])
      tabTokenAssign[i] = "OPERATION"
      i = i+3
    } else {
      tabsimpleAssignationOpe[i]=contentAssignation[i]
      tabTokenAssign[i] = tokenAssignation[i]
      i = i+1
    }
  }
  tableSimpleAsign = as.data.frame(tabsimpleAssignationOpe)
  tableSimpleAsign = t(na.omit(tableSimpleAsign))  
  tableSimpleTokenAsign = as.data.frame(tabTokenAssign)
  tableSimpleTokenAsign = t(na.omit(tableSimpleTokenAsign))
  
  if (isTRUE (typecontent == "FOR")) {
    tokenValues = c("FOR_CONTENT")
  } else {
    tokenValues = c("IF_CONTENT")
  }
  
  simpleAssignValues = rbind(simpleAssignValues,tableSimpleAsign[1],tableSimpleAsign[3], .id= NULL)
  
  bindValueToken <- cbind(tokenValues,simpleAssignValues)
  
  inter = data.frame(target = tableSimpleAsign[, 1], attr = tableSimpleAsign[, 2], source = tableSimpleAsign[, 3], interaction = c("interacts"), weight = 1)
  
  edges <- rbind(edges, inter)
  
  
  node = as.data.frame(t(tableSimpleAsign))
  colnames(node)[1] = "id"
  nodes = rbind(nodes, node)
  nodes =as.vector(nodes)
  nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
  nodes <- subset(nodes, id != "NA")
  nodes <- subset(nodes, id != "=")
  nodes <- subset(nodes, id != "<-")
  
  
  simpleAssignIF <- list("edges" = edges, "nodes" = nodes, "valueToken" = bindValueToken)
  return(simpleAssignIF); 
  
}