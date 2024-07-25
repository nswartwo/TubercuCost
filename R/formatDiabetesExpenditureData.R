format_exp_data <- function(healthExpConvFact = 1.39,
                            USDyear = 2022,
                            StartYear = 2024){

  lifeExpReductPerc <- .12

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### read in the discount file
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[,2]

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### read in life expectancy data
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  lifeExpectancySingleAge <- readRDS("~/TubercuCost/inst/LifeExpectancySingleYearAge.rds")
  discLifeExpectancySingleAge <- readRDS("~/TubercuCost/inst/DiscountedLifeExpectancySingleYearAge.rds")

  lifeExpectancyAgeGroups <- readRDS("~/TubercuCost/inst/LifeExpectancyAgeGroups.rds")
  discLifeExpectancyAgeGroups <- readRDS("~/TubercuCost/inst/DiscountedLifeExpectancyAgeGroups.rds")

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### HEALTHCARE EXPENDITURE DATA
  ##### Healthcare expenditure data are in 2019 USD from
  ##### https://uwchoice.shinyapps.io/futuremedicalcosts/
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ##### read in the annual healthcare expenditure data in 2019
  annualHealthExpenditure <- read.csv("~/TubercuCost/inst/extdata/AnnualHealthExpenses_DiabetesSingleAge2019.csv",
                                      check.names = FALSE)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### SINGLE YEAR OF AGE
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### UNDISCOUNTED ANNUAL HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  annualHealthFit <- lm(data = annualHealthExpenditure, formula = log(`Mean Costs ($)`) ~ Age)

  newData <- data.frame("Age" = 0:100)
  annualHealthPred <- predict.lm(object = annualHealthFit, newdata = newData, se.fit = TRUE)

  annualHealthExpenditureSingleAge <- t(matrix(exp(annualHealthPred$fit) * healthExpConvFact,
                                               ncol = length(StartYear:2180),
                                               nrow = length(0:100),
                                               byrow = FALSE))
  rownames(annualHealthExpenditureSingleAge) <- StartYear:2180
  colnames(annualHealthExpenditureSingleAge) <- 0:100

  # saveRDS(annualHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/HealthExpenditureSingleYearAgeDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### DISCOUNTED ANNUAL HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  DiscountAnnualHealthExpenditureSingleAge <- matrix(0, nrow = nrow(annualHealthExpenditureSingleAge),
                                                     ncol = ncol(annualHealthExpenditureSingleAge))
  colnames(DiscountAnnualHealthExpenditureSingleAge) <- colnames(annualHealthExpenditureSingleAge)
  rownames(DiscountAnnualHealthExpenditureSingleAge) <- rownames(annualHealthExpenditureSingleAge)
  for (i in 1:nrow(DiscountAnnualHealthExpenditureSingleAge)){
    for (j in 1:ncol(DiscountAnnualHealthExpenditureSingleAge)){
      DiscountAnnualHealthExpenditureSingleAge[i,j] <- annualHealthExpenditureSingleAge[i,j] * discountVec[i]
    } }

  saveRDS(DiscountAnnualHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedHealthExpenditureSingleYearAgeDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### UNDISCOUNTED LIFETIME HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  LifetimeHealthExpenditureSingleAge <- annualHealthExpenditureSingleAge[1:77,]

  for (i in 1:nrow(LifetimeHealthExpenditureSingleAge)){
    for(j in 1:ncol(LifetimeHealthExpenditureSingleAge)){
      # years of lost life expectancy
      yearsLost <- round(lifeExpectancySingleAge[i,j] * (1-lifeExpReductPerc))
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- i + yearsLost
      lastCol <- min(j + yearsLost, ncol(LifetimeHealthExpenditureSingleAge))
      LifetimeHealthExpenditureSingleAge[i,j] <- sum(diag(annualHealthExpenditureSingleAge[i:lastRow,i:lastCol]))
    }
  }
  saveRDS(LifetimeHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeHealthExpenditureSingleYearAgeDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### DISCOUNTED LIFETIME HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  discountLifetimeHealthExpenditureSingleAge <- LifetimeHealthExpenditureSingleAge[1:77,]

  for (i in 1:nrow(discountLifetimeHealthExpenditureSingleAge)){
    for(j in 1:ncol(discountLifetimeHealthExpenditureSingleAge)){
      # years of lost life expectancy
      yearsLost <- round(discLifeExpectancySingleAge[i,j] * (1-lifeExpReductPerc))
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- i + yearsLost
      lastCol <- min(j + yearsLost, ncol(discountLifetimeHealthExpenditureSingleAge))
      discountLifetimeHealthExpenditureSingleAge[i,j] <- sum(diag(DiscountAnnualHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    }
  }

  saveRDS(discountLifetimeHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeHealthExpenditureSingleYearAgeDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### AGE GROUPS
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create undiscounted health expenditure estimates for age group estimates - TB mortality
  annualHealthExpenditureAgeGroup <- lifeExpectancyAgeGroups[1:77,-1]
  rownames(annualHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(annualHealthExpenditureAgeGroup)){
    annualHealthExpenditureAgeGroup[i,1] <- mean(annualHealthExpenditureSingleAge[i,1:5])
    annualHealthExpenditureAgeGroup[i,2] <- mean(annualHealthExpenditureSingleAge[i,6:15])
    annualHealthExpenditureAgeGroup[i,3] <- mean(annualHealthExpenditureSingleAge[i,16:25])
    annualHealthExpenditureAgeGroup[i,4] <- mean(annualHealthExpenditureSingleAge[i,26:35])
    annualHealthExpenditureAgeGroup[i,5] <- mean(annualHealthExpenditureSingleAge[i,36:45])
    annualHealthExpenditureAgeGroup[i,6] <- mean(annualHealthExpenditureSingleAge[i,46:55])
    annualHealthExpenditureAgeGroup[i,7] <- mean(annualHealthExpenditureSingleAge[i,56:65])
    annualHealthExpenditureAgeGroup[i,8] <- mean(annualHealthExpenditureSingleAge[i,66:75])
    annualHealthExpenditureAgeGroup[i,9] <- mean(annualHealthExpenditureSingleAge[i,76:85])
    annualHealthExpenditureAgeGroup[i,10] <- mean(annualHealthExpenditureSingleAge[i,86:95])
    annualHealthExpenditureAgeGroup[i,11] <- mean(annualHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(annualHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/HealthExpenditureAgeGroupsDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create discounted health expenditure estimates for age group estimates - TB mortality
  discountAnnualHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup
  rownames(discountAnnualHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(discountAnnualHealthExpenditureAgeGroup)){
    discountAnnualHealthExpenditureAgeGroup[i,1] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,1:5])
    discountAnnualHealthExpenditureAgeGroup[i,2] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,6:15])
    discountAnnualHealthExpenditureAgeGroup[i,3] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,16:25])
    discountAnnualHealthExpenditureAgeGroup[i,4] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,26:35])
    discountAnnualHealthExpenditureAgeGroup[i,5] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,36:45])
    discountAnnualHealthExpenditureAgeGroup[i,6] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,46:55])
    discountAnnualHealthExpenditureAgeGroup[i,7] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,56:65])
    discountAnnualHealthExpenditureAgeGroup[i,8] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,66:75])
    discountAnnualHealthExpenditureAgeGroup[i,9] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,76:85])
    discountAnnualHealthExpenditureAgeGroup[i,10] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,86:95])
    discountAnnualHealthExpenditureAgeGroup[i,11] <- mean(DiscountAnnualHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(discountAnnualHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedHealthExpenditureAgeGroupsDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create undiscounted health expenditure estimates for age group estimates - TB mortality
  LifetimeHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup
  rownames(LifetimeHealthExpenditureAgeGroup) <- 2024:2100
  for(i in 1:nrow(LifetimeHealthExpenditureAgeGroup)){
    LifetimeHealthExpenditureAgeGroup[i,1] <- mean(LifetimeHealthExpenditureSingleAge[i,1:5])
    LifetimeHealthExpenditureAgeGroup[i,2] <- mean(LifetimeHealthExpenditureSingleAge[i,6:15])
    LifetimeHealthExpenditureAgeGroup[i,3] <- mean(LifetimeHealthExpenditureSingleAge[i,16:25])
    LifetimeHealthExpenditureAgeGroup[i,4] <- mean(LifetimeHealthExpenditureSingleAge[i,26:35])
    LifetimeHealthExpenditureAgeGroup[i,5] <- mean(LifetimeHealthExpenditureSingleAge[i,36:45])
    LifetimeHealthExpenditureAgeGroup[i,6] <- mean(LifetimeHealthExpenditureSingleAge[i,46:55])
    LifetimeHealthExpenditureAgeGroup[i,7] <- mean(LifetimeHealthExpenditureSingleAge[i,56:65])
    LifetimeHealthExpenditureAgeGroup[i,8] <- mean(LifetimeHealthExpenditureSingleAge[i,66:75])
    LifetimeHealthExpenditureAgeGroup[i,9] <- mean(LifetimeHealthExpenditureSingleAge[i,76:85])
    LifetimeHealthExpenditureAgeGroup[i,10] <- mean(LifetimeHealthExpenditureSingleAge[i,86:95])
    LifetimeHealthExpenditureAgeGroup[i,11] <- mean(LifetimeHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(LifetimeHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeHealthExpenditureAgeGroupsDiab.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create discounted health expenditure estimates for age group estimates - TB mortality
  discountLifetimeHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup
  rownames(discountLifetimeHealthExpenditureAgeGroup) <- 2024:2100
  for(i in 1:nrow(discountLifetimeHealthExpenditureAgeGroup)){
    discountLifetimeHealthExpenditureAgeGroup[i,1] <- mean(discountLifetimeHealthExpenditureSingleAge[i,1:5])
    discountLifetimeHealthExpenditureAgeGroup[i,2] <- mean(discountLifetimeHealthExpenditureSingleAge[i,6:15])
    discountLifetimeHealthExpenditureAgeGroup[i,3] <- mean(discountLifetimeHealthExpenditureSingleAge[i,16:25])
    discountLifetimeHealthExpenditureAgeGroup[i,4] <- mean(discountLifetimeHealthExpenditureSingleAge[i,26:35])
    discountLifetimeHealthExpenditureAgeGroup[i,5] <- mean(discountLifetimeHealthExpenditureSingleAge[i,36:45])
    discountLifetimeHealthExpenditureAgeGroup[i,6] <- mean(discountLifetimeHealthExpenditureSingleAge[i,46:55])
    discountLifetimeHealthExpenditureAgeGroup[i,7] <- mean(discountLifetimeHealthExpenditureSingleAge[i,56:65])
    discountLifetimeHealthExpenditureAgeGroup[i,8] <- mean(discountLifetimeHealthExpenditureSingleAge[i,66:75])
    discountLifetimeHealthExpenditureAgeGroup[i,9] <- mean(discountLifetimeHealthExpenditureSingleAge[i,76:85])
    discountLifetimeHealthExpenditureAgeGroup[i,10] <- mean(discountLifetimeHealthExpenditureSingleAge[i,86:95])
    discountLifetimeHealthExpenditureAgeGroup[i,11] <- mean(discountLifetimeHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(discountLifetimeHealthExpenditureAgeGroup,
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeHealthExpenditureAgeGroupsDiab.rds"), version=2)

}
