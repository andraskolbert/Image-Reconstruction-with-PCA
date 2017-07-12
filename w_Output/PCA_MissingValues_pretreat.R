
############################################################
#	IMAGE RECONSTRUCTION WITH PRINCIPAL COMPONENTS ANALYSIS  #
# Shiny dashboard                                          #
############################################################ 
#   Final Project                                          #
#   Descriptive Analytics                                  #
#   Fall semester 2015/2016                                #
#   Master in Advanced Analytics                           #
############################################################
#   Beata Babiakova                                        #
#   Carolina Duarte                                        #
#   Andras Kolbert                                         #
############################################################

# Set Working Directory# ###SET TO YOUR  OWN GITHUB DIRECTORY!!!###
#setwd("C:\\Users\\-Andris\\Documents\\GitHub\\DA-project\\ToDeliver\\pics")
#setwd("C:\\Users\\closer\\Documents\\GitHub\\DA-project\\ToDeliver\\pics")
setwd("C:\\Users\\Carolina\\Documents\\GitHub\\DA-project\\ToDeliver\\pics")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
originalGOOD = readJPEG("MonaLisaInputOriginal.jpg")
original = readJPEG("MonaLisaMissingValues.jpg")


##################################################
#MISSING DATA CHALLENGE - image compared with original
##################################################

# set damaged pixels to NA (=actual missing value. When a picture is saved,
# those pixels become simple black!)

for (d in 1:3){
  for (i in 1:nrow(originalGOOD)){
    for (j in 1:ncol(originalGOOD-20)){
      if(abs(originalGOOD[i, j, d] - original[i,j, d]) > 0.1){
        original[i,j, d] <- NA
      }
    }
  }  
}



#sum(is.na(original))



R=original[,,1]
G=original[,,2]
B=original[,,3]


# Multivariate Imputations by Chained Equations (MICE)
library(mice)

#treating missing values R
if(sum(is.na(R))>0) {
  # imputing process (dataset containing the incomplete data, 
  # m=number of imputed datasets, maxit=number of iterations, 
  # printFlag=print history on the console)
  # default method: Predictive Mean Matching (pmm)
  imp <- mice(R, m=1, maxit=1, printFlag=TRUE)
  
  # back to the completed dataset, missing values are replaced with the imputed values
  Datimp <- complete(imp, "long", include=TRUE) 
  
  # the function complete() adds the imputed values to the original dataset,
  # for the reconstructed picture we only use the whole dataset AFTER the imputation
  R <- sapply(Datimp[(nrow(original)+1):(nrow(original)*2), 3:(ncol(original)+2)], as.numeric)
}

#nrow(original)
#ncol(original)



#treating missing values G
if(sum(is.na(G))>0) {
  imp <- mice(G, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  G <- sapply(Datimp[(nrow(original)+1):(nrow(original)*2), 3:(ncol(original)+2)], as.numeric)
}

#treating missing values B
if(sum(is.na(B))>0) {
  imp <- mice(B, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  B <- sapply(Datimp[(nrow(original)+1):(nrow(original)*2), 3:(ncol(original)+2)], as.numeric)
}

img=original

img[,,1]=R
img[,,2]=G
img[,,3]=B
writeJPEG(img,"MissingValuesOutput.jpg")



##################################################
#end of MISSING DATA CHALLENGE
##################################################


# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
k=10

### Red

#r=cov(R)
r=cor(R)
g=eigen(r)
Rv=g$vectors[,1:k]

###Green

#r=cov(G)
r=cor(G)
g=eigen(r)
Gv=g$vectors[,1:k]


###Blue

#r=cov(B)
r=cor(B)
g=eigen(r)
Bv=g$vectors[,1:k]

# Reconstruct the original image

# just an easy way to initialize it to a 3D matrix of the correct size
#img=original 

img[,,1]=R%*%Rv%*%t(Rv)
img[,,2]=G%*%Gv%*%t(Gv)
img[,,3]=B%*%Bv%*%t(Bv)
name=paste(k,"PCsRGBm.jpeg")
writeJPEG(img,name)

