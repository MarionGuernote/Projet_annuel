IfCy <- function(contentIf, tokenIf, edges, nodes, cptif, cptifimb) {
  
  tabcondition = c()
  
  if(isTRUE(tokenIf[,1] == "IF")){
    trg = paste("IF", cptif, cptifimb, sep = '.')
    
    for (i in 1:nrow(contentIf)){
      condition <- ""
      for (n in 2:length(contentIf[i,])){
        condition <- paste(condition,contentIf[i,n])
        if (isTRUE(contentIf[i,n] == ")" ) && isTRUE(contentIf[i,n+1] == "{") ){
          tabcondition = rbind(tabcondition,c(condition))
          break
        }
      }
    }
    
  }else {
    trg = paste("ELSE IF", cptif, cptifimb, sep = '.')
    
    for (i in 1:nrow(contentIf)){
      condition <- ""
      for (n in 4:length(contentIf[i,])){
        condition <- paste(condition,contentIf[i,n])
        if (contentIf[i,n] == ")"){
          tabcondition = rbind(tabcondition,c(condition))
          break
        }
      }
    }
    
  }
  

  
  for (j in 1:length(tokenIf)){
    if (tokenIf[,j] == "SYMBOL"){
        srce =  as.character(contentIf[,j] )       
        break;
    }
  }
 
  

  trg = as.character(trg)
  inter = data.frame(target = trg, attr = tabcondition[1,1], source = srce, interaction = c("interacts"), weight = 1)
  edges = rbind (edges, inter)

 
  node = t(data.frame(target = trg, source = srce))
  colnames(node)[1] = "id"
  nodes = rbind(nodes, node)
  nodes =as.vector(nodes)
  nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
  nodes <- subset(nodes, id != "NA")
  nodes <- subset(nodes, id != "=")
  nodes <- subset(nodes, id != "<-")

  
  inter2 = cbind(token = "CONDITION", value = srce)
  ifAssign <- list("edges" = edges, "nodes" = nodes, "inter" = inter,"ifCondBindValueToken" = inter2)
  return(ifAssign); 
}
