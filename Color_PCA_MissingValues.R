# Set Working Directory
setwd("C:\\Users\\closer\\Documents\\Projecto\\DA-project\\MissingValuesTreatmentColor")

# Load library and read the file
library(jpeg)
originalGOOD = readJPEG("MonaLisaInput.jpg")
original = readJPEG("MonaLisaInputDes.jpg")

### Missing data (compared with original image)

# set missing values (NA) in the pixels that are different from the original image 
for (d in 1:3){
  for (i in 1:nrow(originalGOOD)){
    for (j in 1:ncol(originalGOOD)){
      if(abs(originalGOOD[i, j, d] - original[i,j, d]) > 0.1){
        original[i,j, d] <- NA
      }
    }
  }  
}

# How many missing values do we actually have?
sum(is.na(original))

# Save the image with missing values
writeJPEG(original,"MonaLisaMissingValues.jpg")

R=original[,,1]
G=original[,,2]
B=original[,,3]

library(mice)

#treating missing values R
if(sum(is.na(R))>0) {
  imp <- mice(R, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  R <- sapply(Datimp[145:288, 3:120], as.numeric)
}

#treating missing values G
if(sum(is.na(G))>0) {
  imp <- mice(G, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  G <- sapply(Datimp[145:288, 3:120], as.numeric)
}

#treating missing values B
if(sum(is.na(B))>0) {
  imp <- mice(B, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  B <- sapply(Datimp[145:288, 3:120], as.numeric)
}

img=original

img[,,1]=R
img[,,2]=G
img[,,3]=B
writeJPEG(img,"MonaLisaOutputTreated.jpg")

### PCA

# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
k=100

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

# just an easy way to initialize it to a 3D matrix of the correct size
#img=original 
img[,,1]=R%*%Rv%*%t(Rv)
img[,,2]=G%*%Gv%*%t(Gv)
img[,,3]=B%*%Bv%*%t(Bv)
name=paste(k,"PCsRGBMissing.jpeg")
writeJPEG(img,name)

