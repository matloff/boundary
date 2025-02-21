
# data,yName,qeFtnName as usual in qeML; cLevel is the boundary value c;
# opts are options for qeFtnName

# generates a cdparcoord graph, sameGraphGrpVar being an indicator
# column for the fitted regression function being above or below c

aboveAndBelowC <- function(dataName,yName,qeFtnName,cLevel,opts=NULL) 
{
   library(cdparcoord)

   data <- get(dataName)
   qeFtn <- get(qeFtnName)
   dcArgs <- list(data=data,yName=yName,holdout=NULL)
   if (!is.null(opts)) {
      nms <- names(opts)
      for (nm in nms) {
         dcArgs[[nm]] <- opts[[nm]]
      }
   }
   qeOut <- do.call(qeFtn,dcArgs)
   if (qeFtnName == 'qeLin') {
      fitVals <- qeOut$fitted.values
   } else if (qeFtnName == 'qeKNN') {
      fitVals <- qeOut$regests
   } else if (qeFtnName == 'qeRF') {
      fitVals <- qeOut$predicted
   } else {
      stop('this qeFtn is not supported yet')
   }
   data$bdry <- as.numeric(fitVals > cLevel)
   discparcoord(data,k=25,sameGraphGrpVar='bdry',colorPalette='Magma',
      jitterVal=0.2)
}

## aboveAndBelowC('svcensus','wageinc',65000)

