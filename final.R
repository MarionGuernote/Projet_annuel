convert_symbols <- function(symbols, HGNC, c = 1) {
    genes <- symbols
    expected <- c("Approved.symbol", "Previous.symbols", "Synonyms", "NCBI.Gene.ID")
    if (dim(HGNC)[2] != 4) {
        stop("HGNC correspondence table was not load properly, please consider updating with HGNC=update_symbols()")
    } else if (!identical(colnames(HGNC), expected)) {
        stop("HGNC correspondence table was not load properly, please consider updating with HGNC=update_symbols()")
    }
    
    message("calling limma::alias2Symbol on provided data")
    genes <- ifelse(is.na(alias2SymbolTable(genes)), genes, alias2SymbolTable(genes))
    
    message(paste("-> matching ", length(genes), " symbols to HGNC Approved symbol", sep = ""))
    
    matched <- HGNC$NCBI.Gene.ID[match(tolower(genes), tolower(HGNC$Approved.symbol))]
    
    
    # loc 2 entrezid
    id <- which(is.na(matched))
    if (length(id) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    
    missing_ids <- genes[id]
    message(paste(length(id), " symbols remaining", sep = ""))
    message(paste("-> matching ", length(id), " symbols to ^LOC symbols", sep = ""))
    id2 <- grep("^LOC", missing_ids, perl = TRUE)
    matched[id][id2] <- as.numeric(gsub("LOC", "", missing_ids[id2]))
    # elements not found
    
    i <- which(is.na(matched))
    if (length(i) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    
    if (length(i) != length(id)) {
        message(paste(length(i), " symbols remaining", sep = ""))
    }
    message(paste("-> matching ", length(i), " symbols to approved symbols (adaptative parsing)", sep = ""))
    
    cl <- makeCluster(c)
    registerDoParallel(cl)
    g <- NULL
    l <- 0
    l <- foreach(g = seq_along(i), .combine = cbind) %dopar% {
        # for each element not found get corresponding gene
        gene_to_find <- tolower(genes[i[g]])
        for (line in seq_along(HGNC$Approved.symbol)) {
        }
        if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("-[0-9].*$", gene_to_find, perl = TRUE)) > 0) {
            gene_to_find <- gsub("-", "", gene_to_find)
            for (line in seq_along(HGNC$Approved.symbol)) {
                # split multiple elements
                res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Approved.symbol[line]), split = ", ")[[1]])
                # if found stop
                if (res) {
                  break
                }
            }
        } else if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("(-1)$", gene_to_find, perl = TRUE)) > 0) {
            gene_to_find <- strsplit(gene_to_find, "-")[[1]][1]
            for (line in seq_along(HGNC$Approved.symbol)) {
                # split multiple elements
                res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Approved.symbol[line]), split = ", ")[[1]])
                # if found stop
                if (res) {
                  break
                }
            }
        }
        if (line != length(HGNC$Approved.symbol)) {
            line
        } else {
            NA
        }
    }
    stopCluster(cl)
    if (sum(is.na(l)) != length(l)) {
        matched[i] <- HGNC$NCBI.Gene.ID[l]
    }
    
    
    i0 <- which(is.na(matched))
    if (length(i0) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    if (length(i0) != length(i)) {
        message(paste(length(i0), " symbols remaining", sep = ""))
    }
    
    
    # match to second column: previous symbols (1 elements in column)
    matched[i0] <- HGNC$NCBI.Gene.ID[match(tolower(genes[i0]), tolower(HGNC$Previous.symbols))]
    # element not found
    message(paste("-> matching ", length(i0), " symbols to HGNC previous symbol", sep = ""))
    
    i2 <- which(is.na(matched))
    if (length(i2) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    
    if (length(i0) != length(i2)) {
        message(paste(length(i2), " symbols remaining", sep = ""))
    }
    
    message(paste("-> matching ", length(i2), " symbols to HGNC previous symbol (adapt. parsing)", sep = ""))
    
    # match to second column: previous symbols (2+ elements in column)
    cl <- makeCluster(c)
    registerDoParallel(cl)
    g <- NULL
    l1 <- 0
    l1 <- foreach(g = seq_along(i2), .combine = cbind) %dopar% {
        # for each element not found get corresponding gene
        gene_to_find <- tolower(genes[i2[g]])
        # check if element not found is present in symbols of columns 2 of HGNC
        for (line in seq_along(HGNC$Previous.symbols)) {
            # split multiple elements
            res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Previous.symbols[line]), split = ", ")[[1]])
            # if found stop
            if (res) {
                break
            }
        }
        # get corresponding line or NA and store in l1
        if (line == length(HGNC$Previous.symbols)) {
            if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("-[0-9].*$", gene_to_find, perl = TRUE)) > 0) {
                gene_to_find <- gsub("-", "", gene_to_find)
                for (line in seq_along(HGNC$Previous.symbols)) {
                  # split multiple elements
                  res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Previous.symbols[line]), split = ", ")[[1]])
                  # if found stop
                  if (res) {
                    break
                  }
                }
            } else if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("(-1)$", gene_to_find, perl = TRUE)) > 0) {
                gene_to_find <- strsplit(gene_to_find, "-")[[1]][1]
                for (line in seq_along(HGNC$Previous.symbols)) {
                  # split multiple elements
                  res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Previous.symbols[line]), split = ", ")[[1]])
                  # if found stop
                  if (res) {
                    break
                  }
                }
            }
        }
        if (line != length(HGNC$Previous.symbols)) {
            line
        } else {
            NA
        }
    }
    stopCluster(cl)
    if (sum(is.na(l1)) != length(l1)) {
        matched[i2] <- HGNC$NCBI.Gene.ID[l1]
    }
    
    
    # elements not found
    j <- which(is.na(matched))
    if (length(j) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    
    if (length(i2) != length(j)) {
        message(paste(length(j), " symbols remaining", sep = ""))
    }
    
    # match to third column: Synonyms (1 elements in column)
    matched[j] <- HGNC$NCBI.Gene.ID[match(tolower(genes[j]), tolower(HGNC$Synonyms))]
    # elements not found:
    j2 <- which(is.na(matched))
    if (length(j2) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    message(paste("-> matching ", length(j), " symbols to HGNC synonyms", sep = ""))
    
    if (length(j) != length(j2)) {
        message(paste(length(j2), " symbols remaining", sep = ""))
    }
    
    message(paste("-> matching ", length(j2), " symbols to HGNC synonyms (adapt. parsing)", sep = ""))
    # match to third column: Synonyms (2+ elements in column)
    cl <- makeCluster(c)
    registerDoParallel(cl)
    g <- NULL
    l2 <- 0
    # same as above parse each line (3rd column) look for correspondence
    l2 <- foreach(g = seq_along(j2), .combine = cbind) %dopar% {
        # for each element not found get corresponding gene
        gene_to_find <- tolower(genes[j2[g]])
        # check if element not found is present in symbols of columns 2 of HGNC
        for (line in seq_along(HGNC$Synonyms)) {
            # split multiple elements
            res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Synonyms[line]), split = ", ")[[1]])
            # if found stop
            if (res) {
                break
            }
        }
        # get corresponding line or NA and store in l2
        if (line == length(HGNC$Synonyms)) {
            if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("-[0-9].*$", gene_to_find, perl = TRUE)) > 0) {
                gene_to_find <- gsub("-", "", gene_to_find)
                for (line in seq_along(HGNC$Synonyms)) {
                  # split multiple elements
                  res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Synonyms[line]), split = ", ")[[1]])
                  # if found stop
                  if (res) {
                    break
                  }
                }
            } else if (length(grep("-", gene_to_find, perl = TRUE)) > 0 & length(grep("(-1)$", gene_to_find, perl = TRUE)) > 0) {
                gene_to_find <- strsplit(gene_to_find, "-")[[1]][1]
                for (line in seq_along(HGNC$Synonyms)) {
                  # split multiple elements
                  res <- gene_to_find %in% tolower(strsplit(as.character(HGNC$Synonyms[line]), split = ", ")[[1]])
                  # if found stop
                  if (res) {
                    break
                  }
                }
            }
        }
        if (line != length(HGNC$Synonyms)) {
            line
        } else {
            NA
        }
    }
    stopCluster(cl)
    if (sum(is.na(l2)) != length(l2)) {
        matched[j2] <- HGNC$NCBI.Gene.ID[l2]
    }
    
    # elements not found:
    k <- which(is.na(matched))
    if (length(k) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    
    if (length(j2) != length(k)) {
        message(paste(length(k), " symbols remaining", sep = ""))
    }
    
    # other.
    
    symbols <- genes[which(is.na(matched))]
    
    other_ids <- suppressMessages(mapIds(org.Hs.eg.db, symbols, "ENTREZID", "SYMBOL"))
    if (sum(!(names(other_ids) == symbols)) != 0) {
        warning("org.Hs.eg.db correspondence error")
    }
    
    message(paste("-> matching ", length(k), " symbols to org.Hs.eg.db", sep = ""))
    
    matched[k] <- as.numeric(other_ids)
    
    
    m <- which(is.na(matched))
    if (length(m) == 0) {
        message("all symbols found")
        aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
        return(aliases)
    }
    if (length(k) != length(m)) {
        message(paste(length(m), " symbols remaining", sep = ""))
    }
    
    symbols_not_found <- genes[which(is.na(matched))]
    
    message(paste("-> matching ", length(m), " symbols to ~withdrawn gene symbols", sep = ""))
    
    withd <- match(tolower(paste(symbols_not_found, "~withdrawn", sep = "")), tolower(HGNC$Approved.symbol))
    
    x <- which(!is.na(withd))
    if (length(l) != 0) {
        found_withd <- withd[!is.na(withd)]
        message(paste(length(found_withd), " symbols withdrawn from databases", sep = ""))
        message(as.character(HGNC$Approved.symbol[withd]))
        message("\n")
    } else {
        message("No withdrawn symbols found\n", sep = "")
        
    }
    
    message(paste(length(m), " symbols not found", sep = ""))
    message(paste(round((length(genes) - length(m))/length(genes), 5), "% of genes were found:", sep = ""))
    message("Genes not found:\n", as.character(genes[m]))
    
    aliases <- data.frame(Symbols = genes, entrezID = as.numeric(matched))
    
    return(aliases)
}
