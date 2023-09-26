#'Function that will return different plots based on the arguments given
#'
#'@name TbVisuals
#'@param TbCasesAverted a matrix of tuberculosis cases by year and age group
#'@param TbDeathsAverted a matrix of tuberculosis deaths by year and age group
#'@param LtbiTxInitsInc a matrix of ltbi treatment initations cases by year and age group
#'@return
#'@export


TbVisuals <- function(){
  ###create a plot of the ICER over the different age groups
  ### this is only useful if you are looking a productivity estimates

  ### create the dataframe
  ICER <- cost <- benefit <- TbCasesAverted

  for (i in length(TbCasesAverted)){
    benefit[i] <- calculateQALYs(TbCases = TbCasesAverted[i],
                                TbDeaths = TbDeathsAverted[i],
                                LtbiTxInits = 0)

    cost[i] <- (casesAvertedTest[i] * HealthServiceUnitCosts()["TBtx"])

    ICER[i] <- benefit[i] / cost[i]
  }

  ###
  ICERbyAge <- data.frame("Age" = as.factor(colnames(ICER)),
                          "ICER"= colSums(ICER))

  ggplot2::ggplot(data = ICERbyAge, aes(x=Age, y=ICER)) + geom_point() + theme_minimal() +
    ggtitle("ICER by Age") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
