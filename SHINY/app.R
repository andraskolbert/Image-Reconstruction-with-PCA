############################################################
#	IMPLEMENTING PROJECT IN SHINY				                     #
#	last: bea, 20151110                       					     #
############################################################

#-------------------IMPORTANT------------------#
#after finishing the project we will deploy it again:
#shinyapps::deployApp('C:/Users/beyka/Desktop/MAA projects/DA-Project/DA-project/SHINY')

############
library(shiny)

# Set Working Directory# ###SET TO YOUR  OWN GITHUB DIRECTORY!!!###
setwd("C:\\Users\\-Andris\\Documents\\GitHub\\DA-project\\SHINY")

############################################

ui <- fluidPage(
  
  titlePanel("Image reconstruction with Principal Component Analysis"),
  
  
  sidebarLayout(
###
#INPUTS
    sidebarPanel( 
      #a slider to choose number of principal components
      sliderInput(
        inputId = "numPCs", 
        label = "Choose number of PCs", 
        value = 40, min = 1, max = 100), #MAX updated later
  
      #a select list to choose a pic
      selectInput(
        "image",
        label = "Choose image",
        choices = c("Lisbon" = "lisbon", "Mona Lisa" = "monalisa", "User Input" = "user"),
        multiple = FALSE),
      
      #a select list to choose between correlation and covariance
      selectInput(
      "CorCov",
      label = "Choose correlation or covariance",
      choices = c("Correlation" = "cor", "Covariance" = "cov"),
      multiple = FALSE),
      
      #a select list to choose whether to see theory for R, G or B
      selectInput(
        "RGB",
        label = "Choose the component of RGB you want to see a theoretical 
                explication for",
        choices = c("Red" = "r", "Green" = "g", "Blue" = "b"),
        multiple = FALSE),
      
      #upload user's picture
      fileInput("upload", 
                label = 'Select an Image',
                multiple = FALSE,
                accept=c('image/jpeg')),
      
      h4(textOutput("testing"))),

###    
    
#OUTPUTS
    mainPanel(
      tabsetPanel(
        tabPanel("Image decomposition", imageOutput("result"),
                 "The above picture..... BULLLSSSSS"), 
        tabPanel("Screeplot", plotOutput("screeplot"), "The scree plot is a useful visual aid for determining an 
                 appropriate number of principal components. The scree plot graphs the eigenvalue against the 
                 component number. To determine the appropriate number of components, we look for an elbow
                 in the scree plot. The component number is taken to be the point at which the remaining eigenvalues
                 are relatively small and all about the same size."),
        tabPanel("Loadings", tableOutput("loadings"),"
                 The Loading Plot is a plot of the relationship between original variables and subspace dimensions. PC loadings measure the importance of each variable in accounting for the
                  variability in the PC. It is possible to interpret the first few PCs in terms of 'overall' effect or a 'contrast' between groups of variables based on the
                 structures of PC loadings. 
                 "),
        tabPanel("PC12plot", plotOutput("PC12plot"), "BiPlot:
                  The bi-plot shows both the loadings and the scores for two selected components in parallel. Bi-plot display is a visualization technique for investigating
                  the inter-relationships between the observations and
                  variables in multivariate data. In PCA, relationships between PC scores and PCA loadings
                  associated with any two PCs can be illustrated in a bi-plot
                  display. 
                  ")
      
        
      #tableOutput('files'),
      #uiOutput('images'),
      #imageOutput("result"),
      #tableOutput("table_exp"),
      #plotOutput("screeplot"),
      #tableOutput("loadings"),
      #plotOutput("PC12plot")
    ))
  )
)

###
# install and load library 
install.packages(c("jpeg"))
library(jpeg)


############################################

server <- function(input, output, session) {
# set file limit to 5mb
  options(shiny.maxRequestSize = 5*1024^2)
  
#copy the file into the pic's folder, save as user.jpg
#currently this is hard coded, we could make it dynamic if we define a list for choices, add in this observe,
#we add the new filename etc. Plus for the var<- switch, we need a list too
  observe({
    if (is.null(input$upload)) return()
    file.remove(paste(getwd(),"pics","UserInput.jpg", sep="/"))
    file.copy(input$upload$datapath, paste(getwd(),"pics","UserInput.jpg", sep="/"))
  })




output$result <- renderImage({ 
#Dropdown menu decides which input to load

  
#Reads the filenames into a variable
picfiles<- grep('.jpg', list.files(path = paste(getwd(),"pics", sep="/")), value=TRUE)

#Rtrail - eliminating .jpg
picnames<-gsub(".jpg", "", picfiles)

#Creating named list for the dropdown menu
ls_choices.list <- as.list(picfiles) 
names(ls_choices.list) <- picnames
ls_choices.list
#Updateing the dropdown list with the created named list - not working yet
### !!problem is the condition, that's not following the values - links yet !!###
#updateSelectInput(session, "image", choices = ls_choices.list)

# Change values for input$inSelect

var <-switch(input$image,
           "lisbon" = "pics/LisbonInput.jpg",
           "monalisa" = "pics/MonaLisaInput.jpg",
           "user" = "pics/UserInput.jpg"
)
# loading picture
original=readJPEG(var)

#Updating the maximum number of PCas to use
updateSliderInput(session, "numPCs", max =dim(original)[2])

#output$testing <- renderDataTable(picnames)



# Do the same process separatly for each channel
R=original[,,1]
G=original[,,2]
B=original[,,3]


# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
		k = input$numPCs

### Red
		r <-switch(input$CorCov,
		             "cor" = cor(R),
		             "cov" = cov(R)
		)
		g=eigen(r)
		Rv=g$vectors[,1:k]


###Green
		r <-switch(input$CorCov,
		           "cor" = cor(G),
		           "cov" = cov(G)
		)
		g=eigen(r)
		Gv=g$vectors[,1:k]


###Blue
		r <-switch(input$CorCov,
		           "cor" = cor(B),
		           "cov" = cov(B)
		)
		g=eigen(r)
		Bv=g$vectors[,1:k]

###########################
#output theory for R,G or B
  A <- switch(input$RGB,
              "r" = R,
              "g" = G,
              "b" = B)
		
  g <- switch(input$CorCov,
              "cor" = eigen(cor(A)),
              "cov" = eigen(cov(A)))
  
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
  output$table_exp <- renderTable({
    table_exp
  })
 
  
  #just for testing the variable, can be deleted in the end
  #writing location
  
  output$testing <- renderPlot({
    plot(g$values, type='b')
    })
  
  ############
  #screeplot to see the elbow
  
  #output for the app:
  lambda=g$values
  
  output$screeplot <- renderPlot(
    plot(y=lambda,
         x=1:length(lambda),
         log="x",
         type="b",
         las=1,ylab="Eigenvalues",xlab="Index (log scale)")
    
  )

  
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
  output$loadings <- renderTable({
  loadings[1:15,1:10]
  })
    
  ############
  #scatterplot of the scores PC1 versus PC2
  
  #output for the app:
  output$PC12plot <- renderPlot({
  plot(scores[,1:2])
  abline(h=0); abline(v=0)
  })
  
########################################  
		
#a temp file to save the output
		outfile <- tempfile(fileext = ".jpg")

# just an easy way to initialize it to a 3D matrix of the correct size
		img=original 

		img[,,1]=R%*%Rv%*%t(Rv)
		img[,,2]=G%*%Gv%*%t(Gv)
		img[,,3]=B%*%Bv%*%t(Bv)

		writeJPEG(img, target = outfile)

#return a list containing information about the image

		list(src = outfile,
			contentType = "image/jpeg",
			alt = "This is alternate text") #I dont know yet what this does but might be useful later
	})



#output$files <- renderTable(input$files)


}

shinyApp(ui = ui, server = server)

