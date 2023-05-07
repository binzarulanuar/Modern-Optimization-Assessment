getBenchmark <- function(){
  #if the "UsingR" has not been installed on your system, install it.
  #data("fat", package = "UsingR")

  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html.
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset

  library(readxl)
  file_path <- "C:/Users/nizar/Downloads/Restaurant_Inventory_Log.xlsx"
  sheet_name <- "Sheet1"

  # Read data from Excel sheet
  data <- read_excel(file_path, sheet = sheet_name)

  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- lm(Inventory_Policies ~  Selling_Price + Demand + Unit_Cost_Of_Holding, data = data)

  return (mod)
}



getData <- function(){
  #if the "UsingR" has not been installed on your system, install it.
  #data("fat", package = "UsingR")

  library(readxl)
  # Read data from Excel sheet
  file_path <- "C:/Users/nizar/Downloads/Restaurant_Inventory_Log.xlsx"
  sheet_name <- "Sheet1"
  data <- read_excel(file_path, sheet = sheet_name)

  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html.
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset

  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- getBenchmark()

  #Extract the input data from the fitted model. You can extract the data directly from the variable 'fat' but you
  #will have to explicitly mention all the variables used in the fitting above.
  xx <- model.matrix(mod)[, -1]
  yy <- data$Inventory_Policies         #the response variable
  data <- cbind(xx,yy)
  return (data)
}

featureFitness <- function(string,xx,yy) {
  #print(string)                         #uncomment this line if you want to print every single solution
  inc <- which(string == 1)              #'inc' includes those features/variables for which 'string' contains 1
  if (length(inc)==0) return (-10E20)    #if  no feature is selected then give a terrible fitness to this solution
  X <- cbind(1, xx[,inc])                #create a matrix of values for all the variables contained in 'inc'

  mod <- lm.fit(X, yy)                  #lm.fit computes faster than the 'lm'; because we have to fit thousands of models, use something efficient.
  class(mod) <- "lm"
  -AIC(mod)                  #AIC should be minimised. But the GA package maximises. So let's turn it into a
  #maximisation problem. However, negative values will be a problem with roulette wheel
  #selection which requires positive values to build a roulette wheel. Therefore, consider
  #other ways of inverting the minimisation problem such as 1/(1+AIC); this normalises
  #the values between 0 (the worst) and 1 (the best).
}


