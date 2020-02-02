Script.Name = "test.R"
rm(list = ls(all = TRUE))  # broom variables
gc()  # garbage collector
cat("\f")  #clear console
# LIBRARIES ---------------------------------------------------------------
  setwd("D:/Ordi/Documents/GitHub/Projet_annuel")


#first format the script, here you can define the max length of line.

library(formatR)
library(RCy3)


formatR::tidy_file("./final.R")
p_dat = parse("./final.R")

#parse script
parsed = getParseData(p_dat)

#removing unwanted/empty/unknown lines 
parsed = parsed[-c(which(parsed$token == "expr")), ]
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
xy.list.rm = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove)]
xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) %in% to_remove) == F]


#extracting equal assign lines of length 3 (something = some other thing)
tmp_ind = which(lengths(lapply(xy.list, `[[`, 7)) == 3)
ind_eq = which(lapply(lapply(xy.list[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
eq = xy.list[tmp_ind][ind_eq]

xy.list[tmp_ind[ind_eq]] <- NULL
v = c()
for (i in 1:length(eq)) {
    v = rbind(v, c(eq[[i]]$text[c(1, 3)]))

}
# v[,1]=make.unique(v[,1])
v1 = matrix(v, nrow = length(make.unique(v))/2)
v1=rbind(v1,packages)
v1 = cbind(v1, rep("", dim(v1)[1]))

# write.table(v1, "~/Bureau/connections.txt", sep = ",", quote = F, row.names = F, col.names = F)


##### more complex lines length > 3 
tmp_ind = which(lengths(lapply(xy.list, `[[`, 7)) > 3)
## we still look at lines where there is a = 
ind_eq = which(lapply(lapply(xy.list[tmp_ind], `[[`, 7), `[[`, 2) %in% c("EQ_ASSIGN",'LEFT_ASSIGN'))
eq = xy.list[tmp_ind][ind_eq]
xy.list[tmp_ind[ind_eq]] <- NULL
c=1
v2 = c()
fun = c("read.table", "readRDS")
fun2 =c("data.frame")
for (i in 1:length(eq)) {
  print(eq[[i]])
    if (eq[[i]]$token[3] == "SYMBOL_FUNCTION_CALL") {
        #token 5: because we look after something = something else (
        #                               1         2 3              4
      
        j = which(eq[[i]]$token[5:(dim(eq[[i]])[1] - 1)] == "SYMBOL")
        f = which(eq[[i]]$token[5:(dim(eq[[i]])[1] - 1)] == "SYMBOL_FUNCTION_CALL")
        #specific parsing for well known functions
        if (eq[[i]]$text[3]%in%fun) {
          v2 = rbind(v2, c(eq[[i]]$text[1], paste("input.file.",c,sep=""), eq[[i]]$text[3]))
          c=c+1
        } else if (eq[[i]]$text[3]%in%fun2) {
          if (length(j) > 0 &length(f) > 0 ) {
            v2 = rbind(v2, c(eq[[i]]$text[1], paste(eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)][j],collapse="+"), paste(eq[[i]]$text[3],eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)][f],sep="+")))
          } else {
          v2 = rbind(v2, c(eq[[i]]$text[1], paste(eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)],collapse=""), eq[[i]]$text[3]))}
        } else if (length(j) > 0 &length(f) > 0 ) {
            v2 = rbind(v2, c(eq[[i]]$text[1], paste(eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)][j],collapse="+"), paste(eq[[i]]$text[3],eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)][f],sep="+")))
        } else if (length(j) > 0 ) {
            v2 = rbind(v2, c(eq[[i]]$text[1], paste(eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)][j],collapse="+"), eq[[i]]$text[3]))
        } else {
            v2 = rbind(v2, c(eq[[i]]$text[1], eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)], eq[[i]]$text[3]))}
    } else if (eq[[i]]$text[1] == eq[[i]]$text[3]) {
      newline=c(eq[[i]]$text[1], paste(eq[[i]]$text[5:(dim(eq[[i]])[1] - 1)], collapse = ""), "")
      if (length(grep("^[,]c|^[,][-]c|^c[(]|^[-]c[(]",newline[2],perl = T))>0) {newline=newline[c(1,1,2)]}
      v2 = rbind(v2,newline)
    } else {
      newline=c(eq[[i]]$text[1], paste(eq[[i]]$text[3:(dim(eq[[i]])[1])], collapse = ""), "")
      if (length(grep("^[,]c|^[,][-]c|^c[(]|^[-]c[(]",newline[2],perl = T))>0) {newline=newline[c(1,1,2)]}
        v2 = rbind(v2,newline)

    }
print(v2)
  }
v = rbind(v1, v2)

# write.table(v, "~/Bureau/connections.txt", sep = ",", quote = F, row.names = F, col.names = F)



nodes = as.vector(t(v))
library(RCy3)
cytoscapePing ()
cytoscapeVersionInfo ()

nodes = data.frame(id = unique(nodes),col='#ff0000')

edges = data.frame(target = v[, 1], source = v[, 2], attr = v[, 3], interaction = c("interacts"), weight = 1)
createNetworkFromDataFrames(nodes, edges, title = "my first network", collection = "DataFrame Example")
# setEdgeLabelBypass(edge.names = paste(edges$source, " (", edges$interaction, ") ", edges$target, sep = ""), new.labels = edges$attr)
setEdgeLabelMapping(table.column = "attr",style.name = 'default')
setEdgeTargetArrowShapeDefault(new.shape = "ARROW")
# setNodeColorBypass(node.names = paste("input.file",c(1:13),sep='.'),new.colors = '#ff0000')
