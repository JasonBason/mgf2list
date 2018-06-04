#' Extract Feats function
#'
#' Converts a data.frame to a list of features
#' @param df data.frame to be extracted
#' @export
extractFeats <- function(df){
  outp <- list()
  header <- grep("=",df[,1])

  headerx <- df[header,]
  headerxv <- apply(headerx,1,paste, collapse = " ")
  headerxvnw <- trimws(headerxv)
  # mi <- grep("mass", headerxvnw, ignore.case = TRUE, value = TRUE)
  # mii <- grep("mass", headerxvnw, ignore.case = TRUE)
  #
  # mi2 <- regmatches(mi, regexpr(" ", mi), invert = TRUE)
  # int <- paste0("INT=", (mi2[[1]][2]), collapse = " ")
  # pepm <- strsplit(headerxvnw[mii], " ")[[1]][1]
  # headerxvnw2 <- c(headerxvnw[-mii], int, pepm)
  headersplit <- regmatches(headerxvnw, regexpr("=", headerxvnw), invert = TRUE)
  #header2 <- c(header,5)

  for(i in seq(length(headersplit))){
    outp[[headersplit[[i]][1]]] <- paste(headersplit[[i]][-1])

    tryCatch({
      outp[[headersplit[[i]][1]]] <- as.numeric(outp[[headersplit[[i]][1]]])
    },
    warning = function(w) {}
    )
  }


  if(!is.numeric(outp[["PEPMASS"]])){
    pepsplit <- strsplit(outp[["PEPMASS"]], " ")
    outp[["PEPMASS"]] <- as.numeric(pepsplit[[1]][1])
    outp[["INT"]] <- as.numeric(pepsplit[[1]][2])
  }

  outp[["spectrum"]] <- df[-header,1:2]
  outp[["spectrum"]] <- apply(outp[["spectrum"]],2,as.numeric)

  colnames(outp[["spectrum"]]) <- c("mz", "intensity")
  return(outp)
}

importMGF <- function(filepath){
  mydata = read.table(filepath, fill =TRUE, stringsAsFactors = F)



  #make a dataframe with a column for beginning of chunk and a column for the end of chunk
  ranges <- data.frame(beginnings = which(mydata[,1]=="BEGIN")+1,
                       ends = which(mydata[,1]=="END")-1)

  #result is the application of the chunks function on each row in the ranges of mydata
  #to get a list of data
  res <- apply(ranges,1,chunks, x = mydata)

  res2 <- lapply(res,extractFeats)
  return(res2)
}
