# IS607 Week 3 Quiz Questions - Neil Acampa

# 1) Write a function that takes a numeric vector and calculate the mean of the observation

nvec <- c(1,4,7,8,9,11,22)
nmean <- function(nvec) {
  result <- mean(nvec)
  return(result)
}
nmean(nvec)


# 2) Write a function that takes a numeric vector and calculate the mean of the observation

nvec <- c(1,NA,2,NA,9,10,NA)

nmean <- function(nvec) {
  result <- mean(nvec, na.rm = TRUE)
  return(result)
}
nmean(nvec)

# 3) Write a function that takes two numeric input values and calculates the greatest common 
#    divisor.


gcd <- function(a,b) {
  if ((is.numeric(a) == FALSE) || (is.numeric(b) == FALSE)) {
    return(gcd = "Error  - input is not numeric")
  } 
  if ((a == 0 ) || (b ==0)) {
    return(gcd = "Error - Input cannot be zero")
  }
  v <- c(a,b)
  max = max(v)
  min = min(v)
  i = 0
  result = 0
  while (i <= min) {
    i = i+1
    if (((a %% i) == 0) && ((b %% i) == 0)) {
      result = i
    }
  }
  return(result)
}
gcd(8,12)
gcd(21,7)






# 4) Write a function implements Euclid's alogorithm and calculates the greatest common 
#    divisor.

euclidgcd <- function(a,b) {
  if ((is.numeric(a) == FALSE) || (is.numeric(b) == FALSE)) {
    return(euclidgcd = "Error  - input is not numeric")
  } 
  if ((a == 0 ) || (b ==0)) {
    return(euclidgcd = "Error - Input cannot be zero")
  }
  ppi <- c(a,b)
  imax = max(ppi)
  imin = min(ppi)
  ppi <- c(imin, imax-imin)
  while (imax !=imin) {
    imax = max(ppi)
    imin = min(ppi)
    ppi <- c(imin,imax-imin)
  }
  euclidgcd = imax
  return(euclidgcd)
}

a = 10
b = 25
result = euclidgcd(a,b)
cat("The Greatest Common Denominator for ",a," ", b," using Euclids Algorithm is  = ", result[1], "\n")

a = 27
b = 42
result = euclidgcd(a,b)
cat("The Greatest Common Denominator for ",a," ", b,"\n")
cat("Using Euclids Algorithm is  = ", result[1], "\n")

# 5) Write a funtion that takes two numeric inputs x and y and calculates x^2*y + 2xy -x^y



result <- function(x,y) {
  result <- ((((x^2)*y) - (2*x*y)) - (x*(y^2)))
  return(result)
}
result(2,4)


# 6) Read in the week-3-price-data.csv and week-3-make-model-data.csv files as data frames
#    then merge them by the model key.


nw = setwd("C:/Users/nacampa/Documents")

cwdfle= paste(nw,"/week-3-price-data.csv",sep = "")
wk3price = read.csv(file=cwdfle, head=TRUE)
wk3price
cwdfle= paste(nw,"/week-3-make-model-data.csv",sep = "")
wk3makemodel = read.csv(file=cwdfle, head=TRUE)
wk3makemodel
wk3merge <- merge(x = wk3price, y=wk3makemodel, by.x = c("ModelNumber"), by.y = c("ModelNumber"))

wk3merge
cat("Number or Obervations = ", nrow(wk3merge), "\n")
cat("Yes this is what I would expect because we are merging both files on the ModelNumber field\n")
cat("In this case Model 23120 in row 12 of the price file does not have a match in the\n")
cat("Make-model file and therefore it is it is not included in the merged output\n")

# 7) Use the data sets from the previoius question, but merge them so that the rows from the
#   price-data table appear, even if there is no match in the make-model table.

wk3merge <- merge(x = wk3price, y=wk3makemodel, by.x = c("ModelNumber"), by.y = c("ModelNumber"), all = TRUE)
wk3merge
cat("Number or Obervations = ", nrow(wk3merge), "\n")

# 8) Take the result from question 7 and subset so only the 2010 vehicles are include

require(data.table)
wkdt <- data.table(wk3merge)
setkey(wkdt,Year)
wk32010 <- wkdt[wkdt$Year == "2010"]
wk32010


# 9) Take the result from question 7 and subset so only red cars and cars that cost more than 10,000 are included


require(data.table)
wkdt <- data.table(wk3merge)
setkey(wkdt,Color,Price)

#Two steps

wk3color <- wkdt[wkdt$Color == "Red"]
wk3pc <- wk3color[wk3color$Price > 10000]
wk3pc

#10) Take the subset from question 9 and subset it so that ModelNumber and Color are removed
wkout <- wk3pc[,list(ID, Mileage, Price, Make, Model, Year)]
wkout

# 11) Write a function that takes as input a character vector and returns a numeric vector
#     with the number of characters in each of the elements in the original vector


cvec <- c("apple","boy","cat","dog","elephant")

convec <-  function(cvec) {
  return(nchar(cvec)) 
}


# 12) Write a function that takes two character vectors of equal length and concatentes them element
#     by element with a space as the seperator, 


concatvec <- function(cvec, cvec1) {
  L1 = length(cvec)
  L2 = length(cvec1)
  if (L1 != L2) {
    return(cvec="Error - Vectors are not the same length") 
  } else {
    if ((is.character(cvec) == TRUE) && (is.character(cvec1) == TRUE)) {
      return(paste(cvec,cvec1, spec = ""))
    } else {
      return(cvec="Error - Vectors not character Vectors ")
    }
  }
}
cvec <- c("a","b","c","d","e") 
cvec1 <- c("apple","boy","cat","dog","elephant")
concatvec(cvec,cvec1)

# 13) Write a function that takes a character vector and returns the substring of 3 characters
#     that begins with the first vowel of the string.

require(stringr)


convec <- function(cvec) {
  require(stringr) 
  if (is.character(cvec)) {
    # Initialize array
    L = length(cvec)
    convec = character(length = L)
    
    # Use str commands to detect pattern and Locate first position of pattern
    posvec = str_detect(string = cvec, pattern = ignore.case("[aeiou]"))
    lposvec <- str_locate(string = cvec, pattern = ignore.case("[aeiou]"))
    
    # Return strings that both have a vowel and at least 3 characters after the 
    # occurance of the vowel 
    # otherwise return NA
    for (i in 1:L) {
      if (posvec[i] == TRUE) {
        # First test passed
        fin = lposvec[i] + 2
        
        if (fin <= nchar(cvec[i])) {
          # second test passed
          temp = str_sub(cvec[i], start = lposvec[i], fin)
          convec[i] = temp
        } else {
          convec[i]="NA"
        }
        
      } else {
        convec[i] = "NA"
      }
    }
    return(convec)
  } else {
    return(convec = "Error - Vector is not a character vector")
  }
}

cvec  <- c("apple","boys","LMNOP","Cats","dreamers")
convec(cvec)
cvec  <- c("apple","boys","LMNOP","Cats","dreamers","ap")
convec(cvec)


# 14) Suppose you have a data frame where one column is gives the month
#     the next gives the day, the next gives the year. Use R to create a data frame
#     then add the forth column with the date in date format

d1 <- c(01,02,03,04,05)
d2 <- c(01,02,16,17,07)
d3 <- c(2010,2011,2015,2012,2011)

df <- data.frame(d1,d2,d3)
df

d4 <- paste(df[,3], df[,1], df[,2],sep = "/")
d4 = as.Date(d4)
d4

df <- data.frame(d1,d2,d3,d4)
df

#15) Illustrate what it takes to take a string MM-DD-YYYY and convert it to date

str = "09/07/2014"
mm = as.numeric(str_sub(str, start = 1, 2))
dd = as.numeric(str_sub(str, start = 4, 5))
yy = as.numeric(str_sub(str, start = 7, 11))
if (length(mm) < 2) {
  mm <- paste(0,mm,sep="")
}
if (length(dd) <2) {
  dd <- paste(0,dd,sep="")
}
dt <- paste(yy, mm, dd,sep = "/")
dt


# 16) Illustrate wthe code necessary to take a date and extract the month of the date.

str = "2014-09-07"
mm = as.numeric(str_sub(str, start = 6, 7))
mm 


# 17) Create a sequence of all the dates from Jan 1 2005 to December 31, 2014
startdt <- as.Date("2005/1/1")
enddt  <-  as.Date("2014/12/31")
d      <- seq(startdt,enddt,"day")
d
