cat("\f")  #clear console
rm(list = ls(all = TRUE))  # broom variables

#take the access path of the script to treat 
simpleAssignation <- function(script) {
   
   library(formatR)
   library(RCy3)

   script = "test.R"
   
   formatR::tidy_file(script)
   p_data = parse(script)
   
   #parse script
   parsed = getParseData(p_data)
   
   
   #removing unwanted/empty/unknown lines 
   parsed = parsed[-c(which(parsed$token %in% c("expr", "equal_assign", "forcond"))), ]
   xy.list <- split(parsed, f = parsed$line1)
   
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
   xy.list[tmp_ind] <- NULL
   
   
   tmp_ind = which(lengths(lapply(xy.list, `[[`, 7)) == 3)
   ind_eq = which(lapply(lapply(xy.list[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
   simpleAssignation = xy.list[tmp_ind][ind_eq]
   xy.list[tmp_ind[ind_eq]] <- NULL
   
   tabsimpleAssignation = c()
   for (i in 1:length(operationAssign)) {
      l = length(operationAssign[[2]]$text)
      tabsimpleAssignation = rbind(tabsimpleAssignation, c( operationAssign[[i]]$text[c(1:l)] ) )
      
   }
   
   # namesimpleAssignation = c()
   # for (i in 1:length(tabsimpleAssignation)) {
   #    if ((tabsimpleAssignation[i,4]) == '+'){
   #       namesimpleAssignation = ("addition")}
   #    else if ((tabsimpleAssignation[i,4]) == '-'){
   #       namesimpleAssignation = ("soustraction")}
   #    else if ((tabsimpleAssignation[i,4]) == '/'){
   #       namesimpleAssignation = ("division")}
   #    else if ((tabsimpleAssignation[i,4]) == '*'){
   #       namesimpleAssignation = ("multiplication")}
   #    else {
   #       namesimpleAssignation = ("interaction")}
   #    #namesimpleAssignation = rbind(namesimpleAssignation, c( tabsimpleAssignation[[i]]$text[c(1:l)] ) )
   # }
   
   # test = xy.list[lengths((lapply(xy.list, `[[`, 7))) > 2]
   # test2 = which(lapply(lapply(test, `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
   # test = test[test2]
   #tab = lapply(ind_eq, `[[`, 7)
   # tab2 = lapply(test, `[[`, 9) 
   
   table1toBind <- data.frame((tabsimpleAssignation[,1]),(tabsimpleAssignation[,2]),(tabsimpleAssignation[,3]))
   names(table1toBind) <- c('colonne1','colonne2','colonne3')
   
   table2toBind <- data.frame((tabsimpleAssignation[,3]),(tabsimpleAssignation[,4]),(tabsimpleAssignation[,5]))
   names(table2toBind) <- c('colonne1','colonne2','colonne3')
   
   tableBind <- rbind(table1toBind,table2toBind)
   names(tableBind) <- c('Source','Attraction','Cible')
   
   tableBind <- subset(tableBind, Attraction != "NA")
   
   loadTableData(tableBind)
   edges <- cbind(tableBind,"interacts", weight = 1)
   names(edges) <- c('target', 'attr', 'source', 'interacts', 'weight')

   
   pre_nodes1 = data.frame(id = unique(tabsimpleAssignation[,1]))
   pre_nodes2 = data.frame(id = tabsimpleAssignation[,3])
   pre_nodes = rbind(pre_nodes1,pre_nodes2)
   nodes <- subset(pre_nodes, id != "NA")
   
   
   library(RCy3)
   cytoscapePing()
   #----------------------------------
   setNodeShapeDefault ('ELLIPSE')
   setNodeColorDefault ('#AAFF88')
   setNodeSizeDefault  (60)
   setNodeFontSizeDefault (30)
   #----------------------------------
   
   getNodeShapes ()   # diamond, ellipse, trapezoid, triangle, etc.
   
   #----------------------------------
   #nodes = data.frame(id = unique(nodes),col='#ff0000')
   #----------------------------------
   nodes <- data.frame(id = nodes,
                       stringsAsFactors=TRUE)
   #----------------------------------
   #edges = data.frame(target = v[, 1], source = v[, 2], attr = v[, 3], interaction = c("interacts"), weight = 1)
   
   #----------------------------------
   # loadTableData(edges)
   # edges <- data.frame(source = c(tableBind[,1]),
   #                     attr = c(tableBind[,2]),
   #                     target = c(tableBind[,3]),
   #                     interaction = c("interacts"),
   #                     weight = 1,
   #                     stringsAsFactors=TRUE)
   #----------------------------------
   
   #----------------------------------
   #createNetworkFromDataFrames(nodes, edges, title = "my first network", collection = "DataFrame Example")
   #setEdgeLabelBypass(edge.names = paste(edges$source, " (", edges$interaction, ") ", edges$target, sep = ""), new.labels = edges$attr)
   #----------------------------------
   loadTableData(nodes)
   column <- 'id'
   values <- c ('a', 'b', 'gse', 'gpl', 'p', 'lfc', 'fdr', 'click', '1', '3', '5', '/', '+','<-','=')
   shapes <- c ('DIAMOND', 'DIAMOND', 'PARALLELOGRAM','HEXAGON', 'RECTANGLE', 'OCTAGON', 'VEE', 'ROUND_RECTANGLE', 'TRIANGLE', 'TRIANGLE', 'TRIANGLE', 'ROUND_RECTANGLE', 'ROUND_RECTANGLE','ROUND_RECTANGLE','ROUND_RECTANGLE')
   setNodeSelectionColorDefault(new.color = '#2255CC')
   setNodeShapeMapping(column, values, shapes)
   #----------------------------------
   
   #----------------------------------
   loadTableData(edges)
   column <- 'attr'
   setEdgeLabelMapping(column, style.name = 'default')
   #----------------------------------
   
   #setEdgeLabelMapping(table.column = "attr",style.name = 'default')
   
   setEdgeTargetArrowShapeDefault(new.shape = "ARROW")
   # setNodeColorBypass(node.names = paste("input.file",c(1:13),sep='.'),new.colors = '#ff0000')
   
   createNetworkFromDataFrames(nodes, edges, title = "my simple assignation network", collection = "DataFrame Example")
}

