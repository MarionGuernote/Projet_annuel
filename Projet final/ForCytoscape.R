forCy <- function(contentFor, tokenFor, edges, nodes, cptfor, cptforimb) {
   
   tabcondition = c()
   
   trg = paste("FOR", cptfor, cptforimb, sep = '.')
   
   for (i in 1:nrow(contentFor)){
      condition <- ""
      for (n in 2:length(contentFor[i,])){
         condition <- paste(condition,contentFor[i,n])
         if (isTRUE(contentFor[i,n] == ")" ) && isTRUE(contentFor[i,n+1] == "{") ){
            tabcondition = rbind(tabcondition,c(condition))
            break
         }
      }
   }
   
   
   for (j in 1:length(tokenFor)){
      if (tokenFor[,j] == "SYMBOL"){
         srce =  as.character(contentFor[,j] )       
         break;
      }
   }
   
   
   
   trg = as.character(trg)
   inter = data.frame(target = trg, attr = tabcondition[1,1], source = srce, interaction = c("interacts"), weight = 1)
   edges = rbind (edges, inter)
   
   
   node = t(data.frame(target = trg, source = srce))
   colnames(node)[1] = "id"
   nodes = rbind(nodes, node)
   nodes = as.vector(nodes)
   nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
   nodes <- subset(nodes, id != "NA")
   nodes <- subset(nodes, id != "=")
   nodes <- subset(nodes, id != "<-")
   
   
   inter2 = cbind(token = "FOR_CONDITION", value = srce)
   forAssign <- list("edges" = edges, "nodes" = nodes, "inter" = inter, "forCondBindValueToken" = inter2)
   return(forAssign); 
}
