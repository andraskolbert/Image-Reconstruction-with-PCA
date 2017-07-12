# Set Working Directory
setwd("C:/Users/-Andris/Documents/GitHub/DA-project/SHINY/pics")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
original = readJPEG("MonaLisaInput.jpg")


# The image is viewed as a 3D array of dimensions:
dim(original)


# That means to each pixel corresponds 3 values, that
# determine the color (RGB)
# RGB channels can have values between 0 and 255 or 
# between 0 and 1 (which is the case)

# To do the analysis we need a matrix with the same number
# of pixels but with just one value per pixel
# To do that we can use just one channel or combine the 3

# Lets start with gray images.A possible way of converting it:

gray = (original[,,1]+original[,,2]+original[,,3])/3



# Do the analysis using the correlation or the covariance
# matrix. Just change the definition of r

#r=cov(gray)   
r=cor(gray) 

g=eigen(r)
v=g$vectors
ncol(g)

# Do the scree plot (using log scale just to see it better)

lambda=g$values

plot(y=lambda,
     x=1:length(lambda),
     log="x",
     type="b",
     las=1,ylab="Eigenvalues",xlab="Index (log scale)")


# Reconstruct the original image

img=gray%*%v%*%t(v)
writeJPEG(img,"original.jpeg")


# Reconstruct the image with just k pcs
k=1
v=g$vectors[,1:k]
img=gray%*%v%*%t(v)
name=paste(k,"PCs.jpeg")
writeJPEG(img,name)

# If you want to see how a particular component looks like
k=1
v=g$vectors[,k]
img=gray%*%v%*%t(v)
name=paste(k,"PC.jpeg")
writeJPEG(img,name)

rm(perc_exp)
NROW(g)
g
dim(original)[2]
head(g)
perc_exp<-g$values/dim(original)[2]
perc_exp
cum_exp<-c(perc_exp[1], sum(perc_exp[1:2]), sum(perc_exp[1:3]), 
           sum(perc_exp[1:4]),sum(perc_exp[1:5]),sum(perc_exp[1:6]),
           sum(perc_exp[1:7]),sum(perc_exp[1:8]),sum(perc_exp[1:9]),
           sum(perc_exp[1:10]),sum(perc_exp[1:11]),sum(perc_exp[1:12]),
           sum(perc_exp[1:13]),sum(perc_exp[1:14]),sum(perc_exp[1:15]))

table_exp<-cbind(values=g$values[1:15], variance_explained=perc_exp[1:15], 
                 cummulated_variance_explained=cum_exp)
table_exp
Kaiser <- sum(table_exp[,1] > 1)
