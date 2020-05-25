#take the access path of the script to treat 
simpleAssignation <- function(script) {
   
   source("D:/Ordi/Documents/GitHub/Projet_annuel/ScriptToGraph.R")
   #source("D:/Ordi/Documents/GitHub/Projet_annuel/SimpleAssignationCy.R")SimpleAssignationCy
   scriptToGraph("test.R")
   
   library(formatR)
   library(RCy3)
   library(stringr) 
   library(tidyverse)
   setwd("D:/Ordi/Documents/GitHub/Projet_annuel")
   rm(list = ls(all = TRUE))  # broom variables
   gc()  # garbage collector
   cat("\f")  #clear console
   
   script = "test.R"
   
   formatR::tidy_file(script)
   p_data = parse(script)
   
   #parse script
   parsed = getParseData(p_data)
   
   #removing unwanted/empty/unknown lines 
   parsed = parsed[-c(which(parsed$token %in% c("expr", "equal_assign", "forcond"))), ]
   xy.list <- split(parsed, f = parsed$line1)
   
   #search line where functions are not ended
   lmax = 0
   for (i in 1:length(xy.list)) {
      l = length(xy.list[[i]]$token)
      if (l > lmax){
         lmax =l
      }
   }
   # 
   # token = c()
   # for (i in 1:length(xy.list)) {
   #    #l = length(xy.list[[i]]$token)
   #    token = rbind(token, c( xy.list[[i]]$token[c(1:lmax)] ) )
   # }   
   # 
   # notok = c()
   # token[is.na(token)] <- 0
   # nbop = 0
   # nbcl = 0
   # for (i in 1:nrow(token)) {
   #    l = data.frame(token[35,])
   #    
   #    for (n in 1:nrow(l)) {
   #       if (l[n,] == "'('"){
   #          nbop = nbop + 1
   #       }
   #       if (l[n,] == "'('"){
   #          nbcl = nbcl + 1
   #       }
   #       
   #       if (nbop != nbcl){
   #          notok = rbind(notok, c(i))   
   #       }
   #       nbop = 0
   #       nbcl = 0
   #    }   
   # }   
   
   
   #extracting comments
   xy.list.comments = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT")]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT") == F]
   
   #extracting packages imports
   xy.list.packages = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library")]
   packages = as.character(lapply(lapply(xy.list.packages, `[[`, 9), `[[`, 3))
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library") == F]
   l.packages=rep("library",length(packages))
   packages=cbind(l.packages,packages)
   
   #excluding system commands 
   to_remove = c("rm", "cat", "gc", "setwd")
   xy.list.sc = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove)]
   xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove) == F]
   
   
   #assignation of variable with a operation or a direct attribution
   ind_eq = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "SYMBOL")] #enl�ve les lignes qui commence pas par un symbol
   ind_eq = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))]
   operationAssign = ind_eq[as.logical(lapply(lapply(ind_eq, `[[`, 7), `[[`, 3) %in% c("NUM_CONST", "STR_CONST"))]
   tmp_ind = match( operationAssign, xy.list)
   xy.list.withoutassi = xy.list
   xy.list.withoutassi[tmp_ind] <- NULL
   
   tmp_ind = which(lengths(lapply(xy.list.withoutassi, `[[`, 7)) == 3)
   ind_eq = which(lapply(lapply(xy.list.withoutassi[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
   simpleAssignation = xy.list.withoutassi[tmp_ind][ind_eq]
   xy.list.withoutassi[tmp_ind[ind_eq]] <- NULL
   
   simpleAssignation = c(simpleAssignation , operationAssign)
   
   tabsimpleAssignation = c()
   tokentabsimpleAssignation = c()
   for (i in 1:length(simpleAssignation)) {
      tabsimpleAssignation = rbind(tabsimpleAssignation, c( simpleAssignation[[i]]$text[c(1:lmax)] ) )
      tokentabsimpleAssignation = rbind(tokentabsimpleAssignation, c( simpleAssignation[[i]]$token[c(1:lmax)] ) )
   }


   
   tokenAssignation = t(as.data.frame((tokentabsimpleAssignation[2,])))
   #tokenAssignation = as.data.frame(tokenAssignation[, colSums(is.na(tokenAssignation)) != nrow(tokenAssignation)])

   contentAssignation = t(as.data.frame(tabsimpleAssignation[2,]))
   #contentAssignation = as.data.frame(contentAssignation[, colSums(is.na(contentAssignation)) != nrow(contentAssignation)])
  
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
   tableSimpleAsign = t(na.omit(tabsimpleAssignationOpe))  
   tabTokenAssign = as.data.frame(tabTokenAssign)
   tabTokenAssign = t(na.omit(tabTokenAssign))  
   
   node = as.vector(tableSimpleAsign)
   node <- data.frame(id = unique(nodes),
                      #col = '#ff0000',
                      stringsAsFactors=TRUE)
   node <- subset(nodes, id != "NA")
   node <- subset(nodes, id != "=")
   node <- subset(nodes, id != "<-")
   
  
   
   #-------------------------------------------------------------------------
   tabsimpleAssignationOpe = c()
   tabTokenAssign = c()
   i = 1
   rowNumber = nrow(tabsimpleAssignation)
   rowNum = nrow(tokentabsimpleAssignation)
   while (i < (3*rowNumber+1)) {
      if ((isTRUE((tabsimpleAssignation[i+rowNumber]) == '+')) || (isTRUE((tabsimpleAssignation[i+rowNumber]) == '-')) || (isTRUE((tabsimpleAssignation[i+rowNumber]) == '/')) ||(isTRUE((tabsimpleAssignation[i+rowNumber]) == '*'))) {
         tabsimpleAssignationOpe[i]=paste(tabsimpleAssignation[i],tabsimpleAssignation[i+rowNumber],tabsimpleAssignation[i+(2*rowNumber)])
         tabTokenAssign[i] = "OPERATION"
         i=i+1
      }
      else {
         tabsimpleAssignationOpe[i]=tabsimpleAssignation[i]
         tabTokenAssign[i] = tokentabsimpleAssignation[i]
         i = i + 1
      }
   }
   tableSimpleAsign <- data.frame((tabsimpleAssignationOpe[1:rowNumber]),(tabsimpleAssignationOpe[(rowNumber+1):(2*rowNumber)]),(tabsimpleAssignationOpe[((2*rowNumber)+1):(3*rowNumber)]))
   names(tableSimpleAsign) <- c('column1','column2','column1')
   
   tableSimpleTokenAsign <- data.frame((tabTokenAssign[1:rowNumber]),(tabTokenAssign[(rowNumber+1):(2*rowNumber)]),(tabTokenAssign[((2*rowNumber)+1):(3*rowNumber)]))
   names(tableSimpleTokenAsign) <- c('column1','column2','column1')
   
   
   
   
   loadTableData(tableSimpleAsign)
   edges <- cbind(tableSimpleAsign,"interacts", weight = 1)
   names(edges) <- c('target', 'attr', 'source', 'interacts', 'weight')
   
   
   
   nodes = as.vector(t(tableSimpleAsign))
   cytoscapePing()
   #----------------------------------
   setNodeShapeDefault ('ELLIPSE')
   setNodeColorDefault ('#AAFF88')
   setNodeSizeDefault  (60)
   setNodeFontSizeDefault (30)
   #----------------------------------
   getNodeShapes ()   # diamond, ellipse, trapezoid, triangle, etc.
   #----------------------------------
   #----------------------------------
   nodes <- data.frame(id = unique(nodes),
                       #col = '#ff0000',
                       stringsAsFactors=TRUE)
   nodes <- subset(nodes, id != "NA")
   nodes <- subset(nodes, id != "=")
   nodes <- subset(nodes, id != "<-")
   
   #----------------------------------
   loadTableData(nodes)
   
   tokenValues = c()
   tokenValues = bind_rows(tableSimpleTokenAsign[1],tableSimpleTokenAsign[3], .id= NULL)
   
   simpleAssignValues = c()
   simpleAssignValues = bind_rows(tableSimpleAsign[1],tableSimpleAsign[3], .id= NULL)
   
   bindValueToken = c()
   bindValueToken = cbind(tokenValues,simpleAssignValues)
   
   values = c ()
   shapes = c ()
   column <- 'id'
   for (i in 1:nrow(nodes)){
      for (j in 1:nrow(bindValueToken)){
         if (nodes[i,] == bindValueToken[j,2]){
            if(bindValueToken[j,1] == 'SYMBOL'||bindValueToken[j,1] == 'STR_CONST'||bindValueToken[j,1] == 'NUM_CONST'){
               values <- rbind(values,bindValueToken[j,2])
               shapes <- rbind(shapes,c ('DIAMOND'))
               break
               #setNodeShapeMapping(column, values, shapes)
            }
            if (bindValueToken[j,1] == 'OPERATION'){
               values <- rbind(values, bindValueToken[j,2])
               shapes <- rbind(shapes,c ('OCTAGON'))
               break
               #setNodeShapeMapping(column, values, shapes)
            }
         }
      }
   }
   setNodeShapeMapping(column, values, shapes)
   #----------------------------------
   
   #----------------------------------
   loadTableData(edges)
   column <- 'attr'
   setEdgeLabelMapping(column, style.name = 'default')
   #----------------------------------
   #setEdgeLabelMapping(table.column = "attr",style.name = 'default')
   
   setEdgeTargetArrowShapeDefault(new.shape = "ARROW")
   
   createNetworkFromDataFrames(nodes, edges, title = "my simple assignation network", collection = "DataFrame Example")
   #setVisualStyle('Marquee')
   for (i in 1:nrow(nodes)){
      for (j in 1:nrow(bindValueToken)){
         if (nodes[i,] == bindValueToken[j,2]){
            if(bindValueToken[j,1] == 'SYMBOL'||bindValueToken[j,1] == 'STR_CONST'||bindValueToken[j,1] == 'NUM_CONST'){
               setNodeColorBypass(node.names = bindValueToken[j,2],new.colors = '#ea0d0d')
               break
               #setNodeShapeMapping(column, values, shapes)
            }
            if (bindValueToken[j,1] == 'OPERATION'){
               setNodeColorBypass(node.names = bindValueToken[j,2],new.colors = '#68091b')
               break
               #setNodeShapeMapping(column, values, shapes)
            }
         }
      }
   }
   
   
   
}

