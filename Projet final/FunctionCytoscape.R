FunctionCy <- function(contentF, tokenF, edges, nodes, val, cptimb, funcBindValueToken) {
  
  cptedgeFunction = 0
  cptFOp = 0 #compte les fonctions ouvertes
  cptFCl = 0 #compte les fonctions fermées
  arg = ""
  cptArgs = 1 #compte le nombre d'arguments de la fonction
  tabArgF = c() #contient tous les arguments de la fonction
  inter = c()
  interEdges = c()
  k = val
  srce = c()
  bool = FALSE
  

  
  
  while ( k < length(tokenF)){
    
    if (cptedgeFunction == 0 && cptimb == 0){
      interEdges = rbind(interEdges , data.frame(target = contentF[1,1], attr = "=", source = contentF[1,(val-1)], interaction = c("Argument"), weight = 1))
      edges <- rbind(edges, interEdges)
      
      inter2 = cbind(token = "FUNCTION", value = contentF[1,(val-1)])
      funcBindValueToken = rbind(funcBindValueToken, inter2)
      
      inter2 = cbind(token = "FuncAssign", value = contentF[1,1])
      funcBindValueToken = rbind(funcBindValueToken, inter2)
      
      node = t(data.frame(target = contentF[1,1], source = contentF[1,(val-1)]))
      colnames(node)[1] = "id"
      nodes = rbind(nodes, node)
      
      nodes =as.vector(nodes)
      nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
      nodes <- subset(nodes, id != "NA")
      nodes <- subset(nodes, id != "=")
      nodes <- subset(nodes, id != "<-")
      
      cptedgeFunction = cptedgeFunction + 1
    }
    
     
    if (isTRUE(tokenF[1,k] == "SYMBOL_FUNCTION_CALL") ){
      cptimb = cptimb +1
      
      interEdges = rbind(interEdges , data.frame(target = contentF[1,(val-1)], attr = "called in", source = contentF[1,k], interaction = c("Argument"), weight = 1))
      edges <- rbind(edges, interEdges)
      
      node = t(data.frame(target = contentF[1,(val-1)], source = contentF[1,k]))
      colnames(node)[1] = "id"
      nodes = rbind(nodes, node)
      
      nodes =as.vector(nodes)
      nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
      nodes <- subset(nodes, id != "NA")
      nodes <- subset(nodes, id != "=")
      nodes <- subset(nodes, id != "<-")
      
      inter2 = cbind(token = "FUNCTION", value = contentF[1,k])
      funcBindValueToken = rbind(funcBindValueToken, inter2)
      
      inter2 = cbind(token = "FUNCTION", value = contentF[1,(val-1)])
      funcBindValueToken = rbind(funcBindValueToken, inter2)
      
      
      functionAssign = FunctionCy( contentF , tokenF, edges, nodes, (k+1), (cptimb+1), funcBindValueToken)
      edges = functionAssign$edges
      nodes = functionAssign$nodes
      k = functionAssign$val
      funcBindValueToken = functionAssign$funcBindValueToken
      
    }else if (isTRUE(tokenF[1,k] == "SYMBOL") && isTRUE(contentF[1,k] != "F") && isTRUE(contentF[1,k] != "T")){
      
      # for (j in k:length(tokenF)){
      #   if (isTRUE( tokenF[1,j] == "','" ) || isTRUE( tokenF[1,j] == "')'" )  ){
      #     k = j
      #     break
      #   }
      #   srce = paste(srce,contentF[,j], sep = "")
      # }
      
      bool = FALSE
      for (j in 0:nrow(nodes)){
        if (isTRUE(length(nodes) != 0 ) && isTRUE(as.character(contentF[1,k]) == as.character(nodes[j,]))){
          bool = TRUE
          break
        }
      }

      srce = contentF[1,k]
      if (bool == TRUE){
        interEdges = rbind(interEdges , data.frame(target = contentF[1,(val-1)], attr = "used by", source = srce, interaction = c("Argument"), weight = 1))
        edges <- rbind(edges, interEdges)
        
        inter2 = cbind(token = "FUNCTION", value = contentF[1,(val-1)])
        funcBindValueToken = rbind(funcBindValueToken, inter2)
        
        node = t(data.frame(target = contentF[1,1], source = contentF[1,(val-1)]))
        colnames(node)[1] = "id"
        nodes = rbind(nodes, node)
        
        node = t(data.frame(target = contentF[1,(val-1)], source = srce))
        colnames(node)[1] = "id"
        nodes = rbind(nodes, node)
        
        nodes =as.vector(nodes)
        nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
        nodes <- subset(nodes, id != "NA")
        nodes <- subset(nodes, id != "=")
        nodes <- subset(nodes, id != "<-")
        
        srce = c()
      }
      
      
      
    }else if (isTRUE(contentF[1,k] == "(")){
      cptFOp = cptFOp +1
    }else if (isTRUE(contentF[1,k] == ")")){
      cptFCl = cptFCl + 1
    }
    
   
    
    if (cptFOp == cptFCl){
      break
    }
    
    interEdges = c()
    k = k+1
  }
  
  
  FAssign <- list("edges" = edges, "nodes" = nodes, "val" = k,  "funcBindValueToken" = funcBindValueToken)
  return(FAssign);
  
}

