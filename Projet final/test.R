
# Version info: R 3.2.3, Biobase 2.30.0, GEOquery 2.40.0 CLEAR & CLEAN -----------------------------------------------------------
rm(list = ls(all = TRUE))  # broom variables
gc()  # garbage collector
cat("\f")  #clear console
# LIBRARIES ---------------------------------------------------------------
library(limma)
library(ggplot2)
library(oligo)
library(GEOquery)
library(biomaRt)


setwd("D:/Ordi/Documents/GitHub/Projet_annuel")

# a = 1 + 3 b <- 4/5 t = 5 x = 1 y = 2 if (t > 10) { x <- 3 + 2 y <- 10 } else if (5 <= t & t < 10) { x <- 3 + 1 y <- 5 } else { x <- x - 1 y
# <- 3 }


gse = "GSE39591"
gpl = "GPL6246"

# ACCESS GSE -------------------------------------------------
gset <- getGEO(gse, GSEMatrix = TRUE, getGPL = F)  # .- access GSE ----
idx = grep(gpl, attr(gset, "names"))
gset = gset[[1]]  #idx=1
# .- selecting gpl ----

data = exprs(gset)  # .- access gset  ----
pheno_set <- pData(gset)


# .- extract gene name of GPL ----
platf = parseGEO(paste("../", gse, "/platf/", gpl, ".annot", sep = ""))
ncbifd <- data.frame(attr(dataTable(platf), "table"))
probe_and_genes = ncbifd[, c(1, 4, 3)]

# ACCESS GSM ----

# .- patients for gse48558 ---------

lineage = t(pheno_set[8])
subject = colnames(lineage)
lineage = as.vector(lineage)
lineage = gsub("[.].*", "_", lineage)
mat_labels = t(rbind(subject, lineage))


df = as.data.frame(t(data), stringsAsFactors = F)
# LIMMA analysis --- match(mat_labels[,1],colnames(data))
type = mat_labels[, 2]
type = gsub("[ ].*", "", type, perl = TRUE)  # .- format patients names -----

groups = factor(type, levels = c("ST3", "wt"))
# .- create design matrix ----
design = model.matrix(~0 + groups)
colnames(design) <- c("ST3", "WT")
df = sapply(df, as.numeric)

fit <- lmFit(t(df), design)

contrast.matrix <- makeContrasts(ST3 - WT, levels = design)

fit2 <- contrasts.fit(fit, contrast.matrix)

e2 = eBayes(fit2)

# .- results ----
a = topTable(e2, coef = 1, adjust = "BH", number = 1e+05)
p = 0.01
lfc = 0.5
fdr = 0.05
up_n_downs <- decideTests(fit2, p.value = p, lfc = lfc)
adj.p_gr1 <- p.adjust(e2$p.value[, 1], method = "BH")


fdr_1 = which(adj.p_gr1 < fdr)
GR1 = matrix(ncol = 3)
GR1up = matrix(ncol = 3)
GR1down = matrix(ncol = 3)
table_signif = up_n_downs
sum(which(table_signif[, 1] != 0) %in% fdr_1)
for (i in 1:nrow(table_signif)) {
    # lfc,dir,name
    tmp = cbind(e2$coefficients[i], table_signif[i, 1], rownames(table_signif)[i])
    click = 0
    if (tmp[, 3] %in% names(fdr_1)) {
        if (table_signif[i, 1] != 0) {
            GR1 = rbind(GR1, tmp)
            click = 1
        }
    }
}

# make dataframe and match symbols
GR1 = data.frame(GR1[-c(1, which(GR1[, 3] == "")), ])
