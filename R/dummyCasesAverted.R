#'Function that will return a matrix of dummy values for cases averted
#'
#'@name dummyCases
#'@param startyear
#'@param endyear
#'@param startage
#'@param endage
#'@return
#'@export
dummyCases <- function(
startyear = 2020,
endyear = 2025,
startage = 0,
endage = 100
) {
dummyTbCasesAverted <- matrix(runif(length(startyear:endyear)*
                              length(startage:endage)),
                              nrow = length(startyear:endyear),
                              ncol = length(startage:endage))


colnames(dummyTbCasesAverted) <- startage:endage

rownames(dummyTbCasesAverted) <- startyear:endyear

return(dummyTbCasesAverted)
}
