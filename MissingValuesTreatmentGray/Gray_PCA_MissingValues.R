
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
setwd("C:\\Users\\closer\\Documents\\Projecto\\DA-project\\MissingValuesTreatmentGray")

# Load library and read the file
library(jpeg)
original = readJPEG("MonaLisaInput.jpg")

# The image is viewed as a 3D array of dimensions:
dim(original)


### Missing Values (inserted)

#insert missing values (NA) 
original[60:75, 60:65, 1:3] <- NA
writeJPEG(original,"MonaLisaMissingValues.jpg")

gray = (original[,,1]+original[,,2]+original[,,3])/3


# Check whether there are missing values and treat them
if(sum(is.na(gray))>0) {
  library(mice)
  imp <- mice(gray, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  gray2 <- Datimp[145:288, 1:120]
  gray <- sapply(gray2[1:144, 3:120], as.numeric)
}

# Save the image after missing values treatment
writeJPEG(gray,"MonaLisaOutputTreated.jpg")

### PCA

# Do the analysis using the correlation or the covariance
# matrix. Just change the definition of r
#r=cov(gray)   
r=cor(gray) 

g=eigen(r)
v=g$vectors

# Do the scree plot (using log scale just to see it better)
lambda=g$values
plot(y=lambda,
     x=1:length(lambda),
     log="x",
     type="b",
     las=1,ylab="Eigenvalues",xlab="Index (log scale)")

# Reconstruct the image with just k pcs
k=60
v=g$vectors[,1:k]
img=gray%*%v%*%t(v)
name=paste(k,"PCs.jpeg")
writeJPEG(img,name)

# If you want to see how a particular component looks like
#k=1
#v=g$vectors[,k]
#img=gray%*%v%*%t(v)
#name=paste(k,"PC.jpeg")
#writeJPEG(img,name)

