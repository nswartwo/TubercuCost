format_exp_data <- function(healthExpConvFact = 1.39,
                            USDyear = 2022,
                            StartYear = 2024){
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
  annualHealthExpenditure <- read.csv("~/TubercuCost/inst/extdata/AnnualHealthExpenses_SingleAge2019.csv",
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

  saveRDS(annualHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/HealthExpenditureSingleYearAge.rds"), version=2)

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

  saveRDS(DiscountAnnualHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedHealthExpenditureSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### UNDISCOUNTED LIFETIME HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  LifetimeHealthExpenditureSingleAge <- annualHealthExpenditureSingleAge[1:77,]

  for (i in 1:nrow(LifetimeHealthExpenditureSingleAge)){
    for(j in 1:ncol(LifetimeHealthExpenditureSingleAge)){
      # years of lost life expectancy
      yearsLost <- round(lifeExpectancySingleAge[i,j])
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- i + yearsLost
      lastCol <- min(j + yearsLost, ncol(LifetimeHealthExpenditureSingleAge))
      LifetimeHealthExpenditureSingleAge[i,j] <- sum(diag(annualHealthExpenditureSingleAge[i:lastRow,i:lastCol]))
    }
  }
  saveRDS(LifetimeHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeHealthExpenditureSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### DISCOUNTED LIFETIME HEALTH EXPENSES FOR SINGLE YEAR OF AGE
  discountLifetimeHealthExpenditureSingleAge <- LifetimeHealthExpenditureSingleAge[1:77,]

  for (i in 1:nrow(discountLifetimeHealthExpenditureSingleAge)){
    for(j in 1:ncol(discountLifetimeHealthExpenditureSingleAge)){
      # years of lost life expectancy
      yearsLost <- round(discLifeExpectancySingleAge[i,j])
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- i + yearsLost
      lastCol <- min(j + yearsLost, ncol(discountLifetimeHealthExpenditureSingleAge))
      discountLifetimeHealthExpenditureSingleAge[i,j] <- sum(diag(DiscountAnnualHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    }
  }

  saveRDS(discountLifetimeHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeHealthExpenditureSingleYearAge.rds"), version=2)

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

  saveRDS(annualHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/HealthExpenditureAgeGroups.rds"), version=2)

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

  saveRDS(discountAnnualHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedHealthExpenditureAgeGroups.rds"), version=2)

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

  saveRDS(LifetimeHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeHealthExpenditureAgeGroups.rds"), version=2)

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
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeHealthExpenditureAgeGroups.rds"), version=2)


  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### NON HEALTH CARE EXPENSES ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### read in the annual totalcare expenditure data in 2019
  annualTotalExpenditure <- read.csv("~/TubercuCost/inst/extdata/TotalExpenditureDataAgeGrps2022.csv",
                                      check.names = FALSE)[,1:2]

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### Single year of age
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### UNDISCOUNTED ANNUAL NON TOTAL EXPENSES FOR SINGLE YEAR OF AGE
  pc_y <- annualTotalExpenditure[-c(1,2,10,11),2]
  pc_x  <- c(20,30,40,50,60,70,80)
  est <- spline(pc_x,log(pc_y),xmin=0,xmax=100,n=101)

  annualTotalExpenditureSingleAge <- annualHealthExpenditureSingleAge
  for (i in 1:nrow(annualHealthExpenditureSingleAge)){
    annualTotalExpenditureSingleAge[i,] <- exp(est$y)
  }

  #### calculate the non health expenses
  annualNonHealthExpenditureSingleAge <- annualTotalExpenditureSingleAge - annualHealthExpenditureSingleAge

  saveRDS(annualNonHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/nonHealthExpenditureSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### DISCOUNTED ANNUAL NON TOTAL EXPENSES FOR SINGLE YEAR OF AGE
  DiscountAnnualNonHealthExpenditureSingleAge <- DiscountAnnualHealthExpenditureSingleAge

  for (i in 1:ncol(DiscountAnnualNonHealthExpenditureSingleAge)){
    DiscountAnnualNonHealthExpenditureSingleAge[,i] <- annualNonHealthExpenditureSingleAge[,i] * discountVec[1:nrow(DiscountAnnualNonHealthExpenditureSingleAge)]
  }

  saveRDS(DiscountAnnualNonHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountednonHealthExpenditureSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### UNDISCOUNTED LIFETIME TOTAL EXPENSES FOR SINGLE YEAR OF AGE
  LifetimeNonHealthExpenditureSingleAge <- annualTotalExpenditureSingleAge[1:77,]

  for (i in 1:nrow(LifetimeNonHealthExpenditureSingleAge)){
    for(j in 1:ncol(LifetimeNonHealthExpenditureSingleAge)){
      # years of lost life expectancy
      yearsLost <- round(lifeExpectancySingleAge[i,j])
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- i + yearsLost
      lastCol <- min(j + yearsLost, ncol(LifetimeNonHealthExpenditureSingleAge))
      LifetimeNonHealthExpenditureSingleAge[i,j] <- sum(diag(annualNonHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    }
  }
  saveRDS(LifetimeNonHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeNonHealthExpenditureSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### DISCOUNTED LIFETIME TOTAL EXPENSES FOR SINGLE YEAR OF AGE
  discountLifetimeNonHealthExpenditureSingleAge <- LifetimeNonHealthExpenditureSingleAge
  for (i in 1:ncol(discountLifetimeNonHealthExpenditureSingleAge)){
        discountLifetimeNonHealthExpenditureSingleAge[,i] <- LifetimeNonHealthExpenditureSingleAge[,i] * discountVec[1:nrow(discountLifetimeNonHealthExpenditureSingleAge)]
  }
  saveRDS(discountLifetimeNonHealthExpenditureSingleAge, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeNonHealthExpenditureSingleYearAge.rds"), version=2)


  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### AGE GROUPS
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create undiscounted total expenditure estimates for age group estimates - TB mortality
  annualNonHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup
  rownames(annualNonHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(annualNonHealthExpenditureAgeGroup)){
    annualNonHealthExpenditureAgeGroup[i,1] <- mean(annualNonHealthExpenditureSingleAge[i,1:5])
    annualNonHealthExpenditureAgeGroup[i,2] <- mean(annualNonHealthExpenditureSingleAge[i,6:15])
    annualNonHealthExpenditureAgeGroup[i,3] <- mean(annualNonHealthExpenditureSingleAge[i,16:25])
    annualNonHealthExpenditureAgeGroup[i,4] <- mean(annualNonHealthExpenditureSingleAge[i,26:35])
    annualNonHealthExpenditureAgeGroup[i,5] <- mean(annualNonHealthExpenditureSingleAge[i,36:45])
    annualNonHealthExpenditureAgeGroup[i,6] <- mean(annualNonHealthExpenditureSingleAge[i,46:55])
    annualNonHealthExpenditureAgeGroup[i,7] <- mean(annualNonHealthExpenditureSingleAge[i,56:65])
    annualNonHealthExpenditureAgeGroup[i,8] <- mean(annualNonHealthExpenditureSingleAge[i,66:75])
    annualNonHealthExpenditureAgeGroup[i,9] <- mean(annualNonHealthExpenditureSingleAge[i,76:85])
    annualNonHealthExpenditureAgeGroup[i,10] <- mean(annualNonHealthExpenditureSingleAge[i,86:95])
    annualNonHealthExpenditureAgeGroup[i,11] <- mean(annualNonHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(annualNonHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/NonHealthExpenditureAgeGroups.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create discounted total expenditure estimates for age group estimates - TB mortality
  discountAnnualNonHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup[,-1]
  rownames(discountAnnualNonHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(discountAnnualNonHealthExpenditureAgeGroup)){
    discountAnnualNonHealthExpenditureAgeGroup[i,1] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,1:5])
    discountAnnualNonHealthExpenditureAgeGroup[i,2] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,6:15])
    discountAnnualNonHealthExpenditureAgeGroup[i,3] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,16:25])
    discountAnnualNonHealthExpenditureAgeGroup[i,4] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,26:35])
    discountAnnualNonHealthExpenditureAgeGroup[i,5] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,36:45])
    discountAnnualNonHealthExpenditureAgeGroup[i,6] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,46:55])
    discountAnnualNonHealthExpenditureAgeGroup[i,7] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,56:65])
    discountAnnualNonHealthExpenditureAgeGroup[i,8] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,66:75])
    discountAnnualNonHealthExpenditureAgeGroup[i,9] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,76:85])
    discountAnnualNonHealthExpenditureAgeGroup[i,10] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,86:95])
    discountAnnualNonHealthExpenditureAgeGroup[,11] <- mean(DiscountAnnualNonHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(discountAnnualNonHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedNonHealthExpenditureAgeGroups.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create undiscounted total expenditure estimates for age group estimates - TB mortality
  LifetimeNonHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup[,-1]
  rownames(LifetimeNonHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(LifetimeNonHealthExpenditureAgeGroup)){
    LifetimeNonHealthExpenditureAgeGroup[i,1] <- mean(LifetimeNonHealthExpenditureSingleAge[i,1:5])
    LifetimeNonHealthExpenditureAgeGroup[i,2] <- mean(LifetimeNonHealthExpenditureSingleAge[i,6:15])
    LifetimeNonHealthExpenditureAgeGroup[i,3] <- mean(LifetimeNonHealthExpenditureSingleAge[i,16:25])
    LifetimeNonHealthExpenditureAgeGroup[i,4] <- mean(LifetimeNonHealthExpenditureSingleAge[i,26:35])
    LifetimeNonHealthExpenditureAgeGroup[i,5] <- mean(LifetimeNonHealthExpenditureSingleAge[i,36:45])
    LifetimeNonHealthExpenditureAgeGroup[i,6] <- mean(LifetimeNonHealthExpenditureSingleAge[i,46:55])
    LifetimeNonHealthExpenditureAgeGroup[i,7] <- mean(LifetimeNonHealthExpenditureSingleAge[i,56:65])
    LifetimeNonHealthExpenditureAgeGroup[i,8] <- mean(LifetimeNonHealthExpenditureSingleAge[i,66:75])
    LifetimeNonHealthExpenditureAgeGroup[i,9] <- mean(LifetimeNonHealthExpenditureSingleAge[i,76:85])
    LifetimeNonHealthExpenditureAgeGroup[i,10] <- mean(LifetimeNonHealthExpenditureSingleAge[i,86:95])
    LifetimeNonHealthExpenditureAgeGroup[i,11] <- mean(LifetimeNonHealthExpenditureSingleAge[i,96:101])
  }
  saveRDS(LifetimeNonHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/LifetimeNonHealthExpenditureAgeGroups.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### Create discounted total expenditure estimates for age group estimates - TB mortality
  discountLifetimeNonHealthExpenditureAgeGroup <- annualHealthExpenditureAgeGroup[,-1]
  rownames(discountLifetimeNonHealthExpenditureAgeGroup) <- StartYear:2100
  for(i in 1:nrow(discountLifetimeNonHealthExpenditureAgeGroup)){
    discountLifetimeNonHealthExpenditureAgeGroup[i,1] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,1:5])
    discountLifetimeNonHealthExpenditureAgeGroup[i,2] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,6:15])
    discountLifetimeNonHealthExpenditureAgeGroup[i,3] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,16:25])
    discountLifetimeNonHealthExpenditureAgeGroup[i,4] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,26:35])
    discountLifetimeNonHealthExpenditureAgeGroup[i,5] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,36:45])
    discountLifetimeNonHealthExpenditureAgeGroup[i,6] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,46:55])
    discountLifetimeNonHealthExpenditureAgeGroup[i,7] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,56:65])
    discountLifetimeNonHealthExpenditureAgeGroup[i,8] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,66:75])
    discountLifetimeNonHealthExpenditureAgeGroup[i,9] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,76:85])
    discountLifetimeNonHealthExpenditureAgeGroup[i,10] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,86:95])
    discountLifetimeNonHealthExpenditureAgeGroup[i,11] <- mean(discountLifetimeNonHealthExpenditureSingleAge[i,96:101])
  }

  saveRDS(discountLifetimeNonHealthExpenditureAgeGroup, paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeNonHealthExpenditureAgeGroups.rds"), version=2)

}
