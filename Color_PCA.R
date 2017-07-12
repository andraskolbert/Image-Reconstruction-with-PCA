# Set Working Directory
setwd("C:\\Users\\closer\\Documents\\Projecto\\DA-project")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
original = readJPEG("MonaLisaInput.jpg")

# Do the same process separately for each channel

R=original[,,1]
G=original[,,2]
B=original[,,3]


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

############
############
#we could have a possibility whether to see analysis of R, G or B
A<-R
#A<-G
#A<-B

############
g=eigen(cor(A))

#depending on the cov/cor input
#g=eigen(cov(A))


#perentage of total variation explained by the components
perc_exp<-g$values/NCOL(A)

cum_exp<-c(perc_exp[1], sum(perc_exp[1:2]), sum(perc_exp[1:3]), 
           sum(perc_exp[1:4]),sum(perc_exp[1:5]),sum(perc_exp[1:6]),
           sum(perc_exp[1:7]),sum(perc_exp[1:8]),sum(perc_exp[1:9]),
           sum(perc_exp[1:10]),sum(perc_exp[1:11]),sum(perc_exp[1:12]),
           sum(perc_exp[1:13]),sum(perc_exp[1:14]),sum(perc_exp[1:15]))

table_exp<-cbind(values=g$values[1:15], variance_explained=perc_exp[1:15], 
                 cummulated_variance_explained=cum_exp)

#output for the app:
table_exp

############
#screeplot to see the elbow
plot(g$values, type='b')

#output for the app:
plot(g[1:15]$values, type='b')

############
#by Kaiser criterion, the number of Principal Components that should be considered
#factors with eigenvalues greater than 1

#output for the app:
sum(table_exp[,1] > 1)

############
#by Person criterion (!!I have not found this anywhere but in my notes!!), 
#accept PC until when cumulated variance explained is above 0.8, included

#output for the app:
sum(table_exp[,3] <= 0.8)+1

############
#15 rows, 10 columns, just a sample...explain this in the text!
A.std<-scale(A, center=TRUE, scale=TRUE)
scores<-A.std%*%g$vectors

#15 rows, 10 columns, just a sample...explain this in the text!
loadings<-cor(scores, A.std)

#output for the app:
loadings[1:15,1:10]

############
#scatterplot of the scores PC1 versus PC2

#output for the app:
plot(scores[,1:2])
abline(h=0); abline(v=0)


############
############

# Reconstruct the original image

# just an easy way to initialize it to a 3D matrix of the correct size
img=original 

img[,,1]=R%*%Rv%*%t(Rv)
img[,,2]=G%*%Gv%*%t(Gv)
img[,,3]=B%*%Bv%*%t(Bv)
name=paste(k,"PCsRGB.jpeg")
writeJPEG(img,name)
