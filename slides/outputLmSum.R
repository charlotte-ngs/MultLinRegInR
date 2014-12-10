###
###
###
###   Purpose:   Output lm summary by parts
###   started:   2014/11/18 (pvr)
###
### ################################################### ###
###
###   Helper functions
###

expandCharVec <- function(psCharVec, align = "right") {
  ###
  ###   expendCharVec(psCharVec): expand 
  ###     psCharVec to fixed width
  ### ############################### ###
  ### # constant, separator string
  sFillChar <- " "
  ### # init result vector
  vecResult <- vector(mode = "character", length = length(psCharVec))
  ### # count number of characters in vector
  vecNrChar <- nchar(psCharVec)
  ### # number of fill characters
  vecNrFillChar <- max(vecNrChar) - vecNrChar
  ### # add fill characters
  for (idx in 1:length(psCharVec)) {
    sCurFill <- paste(rep(sFillChar, vecNrFillChar[idx]), sep = "", collapse = "")
    if (align == "left") {
      vecResult[idx] <- paste(psCharVec[idx], sCurFill, sep = "")      
    } else {
      vecResult[idx] <- paste(sCurFill, psCharVec[idx], sep = "")
    }
  }
  return(vecResult)
}

outputMat <- function(aMatrix){
  ###
  ###   outputMat(aMatrix): writes a matrix in fixed
  ###      width format
  ### ############################################### ###
  sColSep <- " "
  for (idx in 1:nrow(aMatrix)) {
    sCurRow <- paste(aMatrix[idx,], sep = "", collapse = sColSep)
    cat(sCurRow, "\n")
  }
}
  
### ################################################### ###
###
###   Functions writing parts of summary to output
###

writeLmCallSummary <- function(psASummary) {
  ### 
  ###   writeLmCallSummary(psASummary): write Call 
  ###      section of psASummary
  ### ############################################ ###
  cat("\nCall:\n", sep = "")
  print(psASummary$call)
}

### ################################################### ###

writeLmResidualsSummary <- function(psASummary) {
  # psASummary <- sumFm1
  ### 
  ###   writeLmResidualsSummary(psASummary): write 
  ###      Residuals section of psASummary
  ### ############################################ ###
  ### # constant, output title
  vecResTitle <- c("Min", "1Q", "Median", "3Q", "Max")
  ### # compute quantities in result
  vecRes <- psASummary$residuals
  minRes <- round(min(vecRes), digits = 4)
  maxRes <- round(max(vecRes), digits = 4)
  medRes <- round(median(vecRes), digits = 4)
  vecQuant <- round(as.vector(quantile(vecRes, probs = c(0.25, 0.75))), digits = 4)
  ### # conversion into character matrix
  vecResOut <- c(vecResTitle, 
                 as.character(minRes), 
                 as.character(vecQuant[1]),
                 as.character(medRes),
                 as.character(vecQuant[2]),
                 as.character(maxRes))
  ### # expand vector elements with fill characters
  vecResOut <- expandCharVec(vecResOut)
  ### # convert to matrix for easier printing
  matResOut <- matrix(data = vecResOut, nrow = 2, byrow = TRUE)
  
  ### # output
  cat("\nResiduals:\n", sep = "")
  outputMat(matResOut)

}

### ################################################### ###

writeLmCoefSummary <- function(psASummary) {
  # psASummary <- sumFm1
  ###
  ###   writeLmCoefSummary(psASummary): write 
  ###      coefficients part of lm summary
  ### ####################################### ###
  ### # extract and round coefficients
  matCoef <- round(psASummary$coefficients, digits = 4)
  ### # column-wise expansion, define expanded matrix
  matExp <- matrix(nrow=nrow(matCoef)+1, ncol=ncol(matCoef)+1)
  matExp[1,1] <- ""
  ### # start with rownames of matCoef
  matExp[,1] <- expandCharVec(c(matExp[1,1], dimnames(matCoef)[[1]]), align = "left")
  for (nColIdx in 1:ncol(matCoef)) {
    sCurCol <- c(dimnames(matCoef)[[2]][nColIdx], as.character(matCoef[,nColIdx]))
    matExp[,(nColIdx+1)] <- expandCharVec(sCurCol)
  }
  ### # output
  cat("\nCoefficients:\n")
  outputMat(matExp)
}

### ################################################### ###

writeLmRSESummary <- function(psASummary) {
  ###
  ###   writeLmRSESummary(psASummary): residual 
  ###      standard errors from a lm summary
  ### ######################################### ###
  cat("\nResidual standard error: ", 
      round(psASummary$sigma, digits = 2),
      " on ", psASummary$df[2], " degrees of freedom\n")
}

### ################################################### ###

writeLmRSQSummary <- function(psASummary) {
  ###
  ###   writeLmRSQSummary(psASummary): 
  ###      R-square and adjusted R-Square
  ### #################################### ###
  cat("Multiple R-squared:  ", round(psASummary$r.squared, digits = 3), 
      ", Adjusted R-squared:  ", round(psASummary$adj.r.squared, digits = 3), "\n")
}

### ################################################### ###

writeLmFStatSummary <- function(psASummary) {
  ###
  ###   writeLmFStatSummary(psASummary): extract and 
  ###      output F-Statistic from lm summary
  ### ############################################### ###
  cat("F-statistic: ", round(as.numeric(psASummary$fstatistic["value"]), digits = 1), 
      " on ", round(as.numeric(psASummary$fstatistic["numdf"]), digits = 0),
      " and ", round(as.numeric(psASummary$fstatistic["dendf"]), digits = 0),
      " DF, p-value: ", round(pf(as.numeric(psASummary$fstatistic["value"]), 
                                 as.numeric(psASummary$fstatistic["numdf"]), 
                                 as.numeric(psASummary$fstatistic["dendf"]), 
                                 lower.tail = FALSE), digits = 7),
      "\n")
}

### ################################################### ###
###
###   Debug and develop, should be deleted once we are done

# fm1 <- lm(rating ~ ., data = attitude)
# sumFm1 <- summary(fm1)

