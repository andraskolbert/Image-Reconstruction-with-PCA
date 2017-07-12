# Set Working Directory
setwd('C:\\Users\\Carolina\\Documents\\GitHub\\DA-project\\ChallengePCsMissingValues')

# (Install jpeg library previously)
# Load library and read the file
library(jpeg)

original = readJPEG("putin.jpeg")
badlisa =  readJPEG("MustachePutin.jpeg")

# Combine RGB channels to get a gray picture
gray = t((original[,,1]+original[,,2]+original[,,3])/3)
badgray = t((badlisa[,,1]+badlisa[,,2]+badlisa[,,3])/3)

# Or do it with color
R=t(badlisa[,,1])
G=t(badlisa[,,2])
B=t(badlisa[,,3])

#With the original picture and the scratched one we
#are able to detect where the scratch is

#Count the number of rows where pixels have been affected
count=0;
for (z in 1:dim(gray)[1]){
  if (sum(abs(gray[z,]-badgray[z,])>0.1)){
    count=count+1;
  }
}

#Initialize a matrix with the same columns as the previous,
#but fewer rows (just the ones which were not affected)
m=matrix(0,dim(gray)[1]-count,dim(gray)[2])
dim(m)

#Populate the matrix with the correct values (gray)
j=0
for (z in 1:dim(gray)[1]){
  if (sum(abs(gray[z,]-badgray[z,])>0.1)==FALSE){
    j=j+1
    m[j,]=badgray[z,]
    c=c+1
  }
}


###Color

#Initialize a matrices with the same columns as the previous,
#but fewer rows (just the ones which were not affected)
mR=matrix(0,dim(gray)[1]-count,dim(gray)[2])
mG=matrix(0,dim(gray)[1]-count,dim(gray)[2])
mB=matrix(0,dim(gray)[1]-count,dim(gray)[2])
dim(m)


#Populate the matrix with the correct values (color)
j=0
for (z in 1:dim(gray)[1]){
  if (sum(abs(gray[z,]-badgray[z,])>0.1)==FALSE){
    j=j+1
    mR[j,]=R[z,]
    mG[j,]=G[z,]
    mB[j,]=B[z,]
  }
}


#######
#Number of principal components to use:
k=10

### Red



#r=cov(R)
r=cor(mR)
g=eigen(r)
Rv=g$vectors[,1:k]


###Green

#r=cov(G)
r=cor(mG)
g=eigen(r)
Gv=g$vectors[,1:k]


###Blue

#r=cov(B)
r=cor(mB)
g=eigen(r)
Bv=g$vectors[,1:k]

# just an easy way to initialize it to a 3D matrix of the correct size
img=original 

ig=original
ig[,,1]=t(R%*%Rv%*%t(Rv))
ig[,,2]=t(G%*%Gv%*%t(Gv))
ig[,,3]=t(B%*%Bv%*%t(Bv))

dim(ig)
dim(gray)[2]
dim(img)

#Populate 3 matrices where the pixels which were not
#affected are the originals and the ones which were affected
#have been replaced by an estimate using PCs
j=0
for (z in 1:dim(gray)[2]){
  for (zz in 1:dim(gray)[1]){
    if (abs(gray[zz,z]-badgray[zz,z])>0.1){
      img[z,zz,1]=ig[z,zz,1]
      img[z,zz,2]=ig[z,zz,2]
      img[z,zz,3]=ig[z,zz,3]
    }
  }
}

name=paste(k,"PCsRGB.jpeg")
writeJPEG(img,name)


