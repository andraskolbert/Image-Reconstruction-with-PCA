
############################################################
#	IMAGE RECONSTRUCTION WITH PRINCIPAL COMPONENTS ANALYSIS  #
# Shiny dashboard                                          #
############################################################ 
#   Final Project                                          #
#   Descriptive Analytics                                  #
#   Fall semester 2015/2016                                #
#   Master in Advanced Analytics                           #
############################################################
#   Carolina Almeida                                       #
#   Beata Babiakova                                        #
#   Andras Kolbert                                         #
############################################################

# Set Working Directory
setwd("C:\\Users\\closer\\Documents\\GitHub\\DA-project\\MissingValuesTreatmentColor")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
originalGOOD = readJPEG("MonaLisaInput.jpg")
original = readJPEG("MonaLisaInputDes.jpg")

##################################################
#MISSING DATA CHALLENGE - image compared with original
##################################################

for (d in 1:3){
  for (i in 1:nrow(originalGOOD)){
    for (j in 1:ncol(originalGOOD)){
      if(abs(originalGOOD[i, j, d] - original[i,j, d]) > 0.3){
        original[i,j, d] <- NA
      }
    }
  }  
}



sum(is.na(original))


writeJPEG(original,"MissingValuesOutput1.jpg")



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
writeJPEG(img,"MissingValuesOutput2.jpg")



##################################################
#end of MISSING DATA CHALLENGE
##################################################


# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
k=60

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
name=paste(k,"PCsRGB.jpeg")
writeJPEG(img,name)

