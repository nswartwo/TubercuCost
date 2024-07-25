#'Function that will return a dataframe of the same size and shape
#'based on a random sample
#'
#'@name sampleDF
#'@param meanDataFrame
#'@param sdDataFrame
#'@return
#'@export

sampleDF <- function(meanDataFrame,
                     sdDataFrame){
  ### define the sample dataFrame
  sampleDataFrame <- meanDataFrame
  sampleDataFrame[,] <- 0
  for(i in 1:length(meanDataFrame)){
    sampleDataFrame[i] <- rnorm(n = 1,
                                mean = meanDataFrame[i],
                                sd = sdDataFrame[i])
  }
  return(sampleDataFrame)
}
