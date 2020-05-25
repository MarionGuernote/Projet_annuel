rm(list = ls(all = TRUE))  # broom variables
gc()  # garbage collector
cat("\f")  #clear console

library(formatR)
library(RCy3)
library(stringr) 
library(tidyverse)
setwd("D:/Ordi/Documents/GitHub/Projet_annuel")
source("SimpleAssignationCytoscape.R")
source("SimpleAssignationIFCytoscape.R")
source("IfCytoscapeV1.R")
source("ForCytoscape.R")
source("FunctionCytoscapeV1.R")
source("EquationCytoscapeV1.R")


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


#extracts the size of the longest line (use to create table results (token and content))
lmax = 0
for (i in 1:length(xy.list)) {
   l = length(xy.list[[i]]$token)
   if (l > lmax){
      lmax =l
   }
}

#extract tokens and content for each row
token = c()
content = c()
for (i in 1:length(xy.list)) {
   #l = length(xy.list[[i]]$token)
   token = rbind(token, c( xy.list[[i]]$token[c(1:lmax)] ) )
   content = rbind(content, c( xy.list[[i]]$text[c(1:lmax)] ) )
}   

token[is.na(token)] <- 0

edges =c()
nodes =c()
tokenValues = c()
simpleAssignValues = c()
bindValueToken = c()



tabopen = c() #permet de stocker ce qui est ouvert dans le code 

cptif = 0 #compte le nombre de if total dans le code pour les différencier avec cytoscape
cptifimb = 0 # compte les if imbriqué dans le if acutellement ouvert afin de dif dans cyto 
cpttransi = 0 

cptfor = 0 
cptforimb = 0

cptelseif = 0

edgesif = c()
edgesfor = c()
edgesopen = c()
trgif = c()

inter = c()
inter2 = c()
inter3 = c()
inter4 = c()
inter5 = c()
inter6 = c()
rank = list()
rankbis = list()
#analyze each line and send to the right function to treat
for ( i in 0:nrow(token)){
   
   ############################################### IF
   #search if condition
   if(isTRUE(token[i,1] == "IF")){
      # regarde si il y a déjà un if d'ouvert pour gérer l'imbrication
      if (length(tabopen) != 0) {
         for (n in 0:nrow(tabopen)){
            if (isTRUE(tabopen[n,1] == "IF")){
               cptifimb =  cptifimb +1
            }
         }
         
      }
      
      #si il n'y a pas d'imbri commencé il y a un nouveau if donc on incrémente le cpt if 
      if (cptifimb == 0) {
         cptif = cptif + 1
      }
      
      #on crée le tableau de ce qui est ouvert pour contenir les infos essentielles
      tabinter = t(c("IF", cptif, cptifimb))
      tabopen = rbind(tabopen, tabinter)
      
      #Ajoute les If dans le tableau bindValueToken
      val = paste("IF", cptif, cptifimb, sep = '.')
      val = as.character(val)
      inter3 = cbind(token = "IF", value = val)
      
      
      #Modifie la nature du token par la dernière valeur obtenue
      if (length(bindValueToken) != 0) {
         change3 = FALSE
         for (k in 1:nrow(bindValueToken)) {
            if (bindValueToken[k,2] == inter3[,2]) {
               bindValueToken = bindValueToken[-k,]
               bindValueToken = rbind(bindValueToken,inter3)
               change3 = TRUE
            }
         }
         if (change3 == FALSE) {
            bindValueToken = rbind(bindValueToken,inter3)
         }
      }
      else {
         bindValueToken = rbind(bindValueToken,inter3)
      }
      
      contentIf = t(as.data.frame(content[i,]))
      tokenIf = t(as.data.frame(token[i,]))
      ifAssign = IfCy(contentIf, tokenIf, edges, nodes, cptif, cptifimb)
      #relie l'assignation simple au if dans lequel elle est traitée
      inter = ifAssign$inter
      if (length(edgesopen) != 0 && nrow(edgesopen) != 0) {
         inter2 = data.frame(target = edgesopen[nrow(edgesopen),1], attr = edgesopen[nrow(edgesopen),2], source = inter[nrow(inter),1], interaction = c("interacts"), weight = 1)
      } 
      edges = ifAssign$edges
      nodes = ifAssign$nodes
      if (length(inter2) != 0) {
         edges =rbind(edges, inter2)
      } 
      edgesif = rbind(edgesif,inter)
      edgesopen = rbind(edgesopen,inter)
      
      inter6 = ifAssign$ifCondBindValueToken
      #Modifie la nature du token par la dernière valeur obtenue
      if (length(bindValueToken) != 0) {
         change6 = FALSE
         for (k in 1:nrow(bindValueToken)) {
            if (bindValueToken[k,2] == inter6[,2]) {
               bindValueToken = bindValueToken[-k,]
               bindValueToken = rbind(bindValueToken,inter6)
               change6 = TRUE
            }
         }
         if (change6 == FALSE) {
            bindValueToken = rbind(bindValueToken,inter6)
         }
      }
      else {
         bindValueToken = rbind(bindValueToken,inter6)
      }
      
   }
   
   ############################################### ELSE IF
   #search else if condition
   if(  isTRUE(token[i,2] == "ELSE") && isTRUE(token[i ,3] == "IF") ){
      # regarde si il y a déjà un else if d'ouvert pour sa numérotation
      if (length(tabopen) != 0) {
         for (n in 0:nrow(tabopen)){
            if ( isTRUE(tabopen[n,1] == "ELSE IF") ){
               cptelseif =  cptelseif +1
            }
         }
      }
      
      #on crée le tableau de ce qui est ouvert pour contenir les infos essentielles
      tabinter = t(c("ELSE IF", cptif, cptelseif))
      tabopen = rbind(tabopen, tabinter)
      
      contentIf = t(as.data.frame(content[i,]))
      tokenIf = t(as.data.frame(token[i,]))
      ifAssign = IfCy(contentIf, tokenIf, edges, nodes, cptif, cptelseif)
      #relie l'assignation simple au if dans lequel elle est traitée
      inter = ifAssign$inter
      if (length(edgesopen) != 0 && nrow(edgesopen) != 0) {
         inter2 = data.frame(target = edgesopen[nrow(edgesopen),1], attr = edgesopen[nrow(edgesopen),2], source = inter[nrow(inter),1], interaction = c("interacts"), weight = 1)
      } 
      edges = ifAssign$edges
      nodes = ifAssign$nodes
      if (length(inter2) != 0) {
         edges =rbind(edges, inter2)
      } 
      
      edgesif = rbind(edgesif,inter)
      edgesopen = rbind(edgesopen,inter)
   }
   
   ############################################### ELSE
   #search else  condition
   if(  isTRUE(token[i,2] == "ELSE") && isTRUE(token[i ,3] != "IF")  ){
      
      #on crée le tableau de ce qui est ouvert pour contenir les infos essentielles
      tabinter = t(c("ELSE", cptif, 0))
      tabopen = rbind(tabopen, tabinter)
      
      if (length(edgesif) != 0 && nrow(edgesif) != 0) {
         srce = paste("ELSE", cptif, sep = '.')
         inter = data.frame(target = edgesif[nrow(edgesif),1], attr = "", source = srce, interaction = c("interacts"), weight = 1)
         edges = rbind (edges, inter)
         
         node = t(data.frame(target = edgesif[nrow(edgesif),1], source =as.character( srce) ))
         colnames(node)[1] = "id"
         nodes = rbind(nodes, node)
         nodes =as.vector(nodes)
         nodes <- data.frame(id = unique(nodes), stringsAsFactors=TRUE)
         nodes <- subset(nodes, id != "NA")
         nodes <- subset(nodes, id != "=")
         nodes <- subset(nodes, id != "<-")
         
         edgesif = rbind(edgesif,data.frame(target = srce, attr = "", source =srce, interaction = c("interacts"), weight = 1))
         edgesopen = rbind(edgesopen,data.frame(target = srce, attr = "", source =srce, interaction = c("interacts"), weight = 1))
      }
      
      
   }
   
   ############################################### FOR
   #search for condition
   if(isTRUE(token[i,1] == "FOR")){
      # regarde si il y a déjà un for d'ouvert pour gérer l'imbrication
      if (length(tabopen) != 0) {
         for (n in 0:nrow(tabopen)){
            if (isTRUE(tabopen[n,1] == "FOR")){
               cptforimb =  cptforimb +1
            }
         }
         
      }
      
      #si il n'y a pas d'imbri commencé il y a un nouveau for donc on incrémente le cpt for  
      if (cptforimb == 0) {
         cptfor = cptfor + 1
      }
      
      #on crée le tableau de ce qui est ouvert pour contenir les infos essentielles
      tabinter = t(c("FOR", cptfor, cptforimb))
      tabopen = rbind(tabopen, tabinter)
      
      contentFor = t(as.data.frame(content[i,]))
      tokenFor = t(as.data.frame(token[i,]))
      forAssign = forCy(contentFor, tokenFor, edges, nodes, cptfor, cptforimb)
      #relie l'assignation simple au if dans lequel elle est traitée
      inter = forAssign$inter
      #relie l'assignation simple au if dans lequel elle est traitée
      inter = forAssign$inter
      if (length(edgesopen) != 0 && nrow(edgesopen) != 0) {
         inter2 = data.frame(target = edgesopen[nrow(edgesopen),1], attr = edgesopen[nrow(edgesopen),2], source = inter[nrow(inter),1], interaction = c("interacts"), weight = 1)
      } 
      edges = forAssign$edges
      nodes = forAssign$nodes
      if (length(inter2) != 0) {
         edges =rbind(edges, inter2)
      } 
      edgesfor = rbind(edgesfor,inter)
      edgesopen = rbind(edgesopen,inter)
      
   }
   
   
   ############################################### END IF, ELSE IF, ELSE, FOR
   # vérifie qu'un if, else if, else ou un for se termine
   if (isTRUE(token[i,1] == "'}'")){
      #enlève la dernière ligne du tableau de ce qui est ouvert (if et for) puisque la dernière chose ouverte est focément la première chose fermée
      tabopen = as.data.frame(tabopen)
      l = nrow(tabopen)
      
      if (tabopen[l,1] == "IF" && length(edgesif) != 0 && nrow(edgesif) != 0){
         m = nrow(edgesif)
         edgesif = edgesif[-m,]
      }else if (isTRUE(tabopen[l,1] == "ELSE IF") || isTRUE(tabopen[l,1] == "ELSE") && length(edgesif) != 0 && nrow(edgesif) != 0){
         m = nrow(edgesif) 
         if (m-1 > 0){
            edgesif = edgesif[-(m-1),]
         }else{
            edgesif = edgesif[-m,]
         }
      }else if (tabopen[l,1] == "ELSE" && length(edgesif) != 0 && nrow(edgesif) != 0){
         m = nrow(edgesif) 
         if (m-1 > 0){
            edgesif = edgesif[-(m-1),]
         }else{
            edgesif = edgesif[-m,]
         }
      }
      
      tabopen = tabopen[-l,]
      
      
      #vérifie si il y a encore des if dans le tableau, si ce n'est pas le cas on réinitialise le compteur des if imbriqué, puisque la présence d'un nouveau if va entrainer une nouvelle imbrication (on va passer de 1.3 à 2.0 )
      if (length(tabopen) != 0) {
         for (j in 0:nrow(tabopen)){
            if (isTRUE(tabopen[j,1] == "IF")){
               cpttransi =  cpttransi +1
            }
         }
      }
      if (cpttransi == 0 ){
         cptifimb = 0
      }
      
   }
   
   ############################################### SIMPLE ASSIGNATION
   #search simple assignation (calculation or x = c)
   if (token[i,1] == "SYMBOL" && isTRUE( token[i,2] == "EQ_ASSIGN" || token[i,2] == "LEFT_ASSIGN") && isTRUE( token[i,3] == "NUM_CONST" || token[i,3] == "STR_CONST"|| isTRUE(token[i,3] == "SYMBOL" && token[i,4] == 0) ) ){
      #relie l'assignation simple au if dans lequel elle est traitée
      if (length(edgesopen) != 0 && nrow(edgesopen) != 0) {
         inter = data.frame(target = content[i,1], attr = edgesopen[nrow(edgesopen),2], source = edgesopen[nrow(edgesopen),1], interaction = c("interacts"), weight = 1)
         edges = rbind (edges, inter)
         contentS = t(as.data.frame(content[i,]))
         tokenS = t(as.data.frame(token[i,]))
         simpleAssignIF = SimpleAssignationIFCy( contentS , tokenS, edges, nodes)
         edges = simpleAssignIF$edges
         nodes = simpleAssignIF$nodes
         inter5 = simpleAssignIF$valueToken
         
         #Modifie la nature du token par la dernière valeur obtenue
         if (length(bindValueToken) != 0) {
            for (k in 1:nrow(bindValueToken)) {
               for (l in 1:nrow(inter5)){
                  if (bindValueToken[k,2] == inter5[l,2]) {
                     rank = c(rank,k)
                  }
               }
            }
            if (length(rank) != 0) {
               for (m in 1:length(rank)) {
                  lign = rank[[m]]
                  bindValueToken = bindValueToken[-lign,]
               }
            }
            bindValueToken = rbind(bindValueToken,inter5)
         }
         else {
            bindValueToken = rbind(bindValueToken,inter5)
         }
         
      }else {
         contentS = t(as.data.frame(content[i,]))
         tokenS = t(as.data.frame(token[i,]))
         simpleAssign = SimpleAssignationCy( contentS , tokenS, edges, nodes, tokenValues)
         edges = simpleAssign$edges
         nodes = simpleAssign$nodes
         inter4 = simpleAssign$valueToken
         
         #Modifie la nature du token par la dernière valeur obtenue
         if (length(bindValueToken) != 0) {
            for (k in 1:nrow(bindValueToken)) {
               for (l in 1:nrow(inter4)){
                  if (bindValueToken[k,2] == inter4[l,2]) {
                     rankbis = c(rankbis,k)
                  }
               }
            }
            if (length(rankbis) != 0) {
               for (m in 1:length(rankbis)) {
                  lignbis = rankbis[[m]]
                  bindValueToken = bindValueToken[-lignbis,]
               }
            }
            bindValueToken = rbind(bindValueToken,inter4)
         }
         else {
            bindValueToken = rbind(bindValueToken,inter4)
         }
      }
   }
   
   ############################################### USE FUNCTION
   #search functions
   if (isTRUE(token[i,3] == "SYMBOL_FUNCTION_CALL")){
      contentF = t(as.data.frame(content[i,]))
      tokenF = t(as.data.frame(token[i,]))
      
      functionAssign = FunctionCy( contentF , tokenF, edges, nodes, 4, 0)
      edges = functionAssign$edges
      nodes = functionAssign$nodes
   }
   
   
   ############################################### EQUATION
   #search equation
   if (token[i,1] == "SYMBOL" && isTRUE( token[i,2] == "EQ_ASSIGN" || token[i,2] == "LEFT_ASSIGN") && isTRUE( token[i,3] == "SYMBOL") && isTRUE( token[i,3] != "0")){
      contentEquation = t(as.data.frame(content[i,]))
      tokenEquation = t(as.data.frame(token[i,]))
      
      eqAssign = equationCy(contentEquation , tokenEquation, edges, nodes)
      edges = eqAssign$edges
      nodes = eqAssign$nodes
   }

inter2 = c()
}


node = c()
row.names(nodes) <- 1 : nrow(nodes)
for (i in 0:nrow(nodes)){
   l = data.frame(nodes[i,])
   node = rbind(node, l)
}
colnames(node)[1] = "id"



cytoscapePing()


#=============================
#Modification de la forme du noeud en fonction de la nature du noeud
#=============================

values = c ()
shapes = c ()
column <- 'id'
for (i in 1:nrow(nodes)){
   for (j in 1:nrow(bindValueToken)){
      if (node[i,] == bindValueToken[j,2]){
         if(bindValueToken[j,1] == 'SYMBOL'||bindValueToken[j,1] == 'STR_CONST'||bindValueToken[j,1] == 'NUM_CONST' || bindValueToken[j,1] == 'IF_CONTENT'){
            values <- rbind(values,bindValueToken[j,2])
            shapes <- rbind(shapes,c ('ELLIPSE'))
            break
         }
         if (bindValueToken[j,1] == 'OPERATION'){
            values <- rbind(values, bindValueToken[j,2])
            shapes <- rbind(shapes,c ('OCTAGON'))
            break
         }
         if (bindValueToken[j,1] == 'IF'){
            values <- rbind(values, bindValueToken[j,2])
            shapes <- rbind(shapes,c ('ROUND_RECTANGLE'))
            break
         }
         if (bindValueToken[j,1] == 'CONDITION'){
            values <- rbind(values, bindValueToken[j,2])
            shapes <- rbind(shapes,c ('DIAMOND'))
            break
         }
      }
   }
}
setNodeShapeMapping(column, values, shapes)

setEdgeLabelMapping(table.column = "attr",style.name = 'default')
setEdgeTargetArrowShapeDefault(new.shape = "ARROW")

createNetworkFromDataFrames(node, edges, title = "assignation network", collection = "En cours")

#=============================
#Ajout de la couleur en fonction de la nature du noeud
#=============================

for (i in 1:nrow(nodes)){
   for (j in 1:nrow(bindValueToken)){
      if (node[i,] == bindValueToken[j,2]){
         if(bindValueToken[j,1] == 'SYMBOL'||bindValueToken[j,1] == 'STR_CONST'||bindValueToken[j,1] == 'NUM_CONST'){
            setNodeColorBypass(node.names = bindValueToken[j,2],new.colors = '#ea0d0d')
            break
         }
         if (bindValueToken[j,1] == 'OPERATION'){
            setNodeColorBypass(node.names = bindValueToken[j,2],new.colors = '#68091b')
            break
         }
         if ((bindValueToken[j,1] == 'IF') || (bindValueToken[j,1] == 'CONDITION') || (bindValueToken[j,1] == 'IF_CONTENT')) {
            setNodeColorBypass(node.names = bindValueToken[j,2],new.colors = '#318CE7')
            break
         }
      }
   }
}



