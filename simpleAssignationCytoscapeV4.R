cat("\f")  #clear console
rm(list = ls(all = TRUE))  # broom variables

#take the access path of the script to treat 
#simpleAssignation <- function(script) {

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
ind_eq = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "SYMBOL")] #enlève les lignes qui commence pas par un symbol
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

tabsimpleAssignation[3]
bidule = paste(tabsimpleAssignation[19],tabsimpleAssignation[28],tabsimpleAssignation[37])
show(bidule)

tabsimpleAssignationOpe = c()
i = 1
#j = 1
rowNumber = nrow(tabsimpleAssignation)
while (i < (3*rowNumber+1)) {
  if ((isTRUE((tabsimpleAssignation[i+rowNumber]) == '+')) || (isTRUE((tabsimpleAssignation[i+rowNumber]) == '-')) || (isTRUE((tabsimpleAssignation[i+rowNumber]) == '/')) ||(isTRUE((tabsimpleAssignation[i+rowNumber]) == '*'))) {
    tabsimpleAssignationOpe[i]=paste(tabsimpleAssignation[i],tabsimpleAssignation[i+rowNumber],tabsimpleAssignation[i+(2*rowNumber)])
    show(tabsimpleAssignationOpe[i])
    i=i+1
    #tabsimpleAssignationOpe[i]=paste(tabsimpleAssignationOpe[i-1],tabsimpleAssignation[i+rowNumber])
  }
  else {
    tabsimpleAssignationOpe[i]=tabsimpleAssignation[i]
    #show(tabsimpleAssignationOpe[i])
    i = i + 1
  }
}

tableSimpleAsign <- data.frame((tabsimpleAssignationOpe[1:rowNumber]),(tabsimpleAssignationOpe[(rowNumber+1):(2*rowNumber)]),(tabsimpleAssignationOpe[((2*rowNumber)+1):(3*rowNumber)]))
names(tableSimpleAsign) <- c('column1','column2','column3')


(tabsimpleAssignationOpe[1:(rowNumber)])
(tabsimpleAssignationOpe[(rowNumber+1):(rowNumber+rowNumber)])
show(tableSimpleAsign)
show(tabsimpleAssignation)

loadTableData(tableSimpleAsign)
edges <- cbind(tableSimpleAsign,"interacts", weight = 1)
names(edges) <- c('target', 'attr', 'source', 'interacts', 'weight')



nodes = as.vector(t(tableSimpleAsign))
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
#----------------------------------
nodes <- data.frame(id = unique(nodes),
                    #col = '#ff0000',
                    stringsAsFactors=TRUE)
nodes <- subset(nodes, id != "NA")
nodes <- subset(nodes, id != "=")
nodes <- subset(nodes, id != "<-")

#----------------------------------
loadTableData(nodes)

column <- 'id'
values <- c ('a', 'b', 'gse', 'gpl', 'p', 'lfc', 'fdr', 'click')
shapes <- c ('DIAMOND', 'DIAMOND', 'PARALLELOGRAM','HEXAGON', 'RECTANGLE', 'OCTAGON', 'VEE', 'ROUND_RECTANGLE')

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
setVisualStyle('Marquee')
setNodeColorBypass(node.names = 'a',new.colors = '#ea0d0d')
setNodeColorBypass(node.names = 'b',new.colors = '#68091b')
#}

