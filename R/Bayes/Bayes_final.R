######################## 
#### Bayes Final 1 ##### 
########################
setwd("/Users/imac/Desktop")
graphics.off()
source("/Users/imac/Downloads/DBDA2Eprograms/DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("/Users/imac/Desktop/PoisGamma.R")          # Load the definition of the BernBeta function

# Case 1: Mode = 1, SD = 1
# Specify the prior:
t = 1            	 # Specify the prior MODE.
sd= 1	             # Specify the effective prior sample size.
a = gammaShRaFromModeSD(t, sd)[1][[1]]  		 # Convert to beta shape parameter a.
b = gammaShRaFromModeSD(t, sd)[2][[1]]		     # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
Data = c(6, 3, 2, 15, 2, 4, 5)      # Data follow the Poisson distribution

openGraph(width=5,height=7)
posterior = PoisGamma( priorGamma=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="PoisGammaExample",type="png")



# Case 2: Mode = 5, SD = 1
# Specify the prior:
t = 1            	 # Specify the prior MODE.
sd= 5	             # Specify the effective prior sample size.
a = gammaShRaFromModeSD(t, sd)[1][[1]]  		 # Convert to beta shape parameter a.
b = gammaShRaFromModeSD(t, sd)[2][[1]]		     # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
Data = c(6, 3, 2, 15, 2, 4, 5)      # Data follow the Poisson distribution

openGraph(width=5,height=7)
posterior = PoisGamma( priorGamma=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="PoisGammaExample",type="png")



# Case 3: Mode = 1, SD = 5
# Specify the prior:
t = 1            	 # Specify the prior MODE.
sd= 10	             # Specify the effective prior sample size.
a = gammaShRaFromModeSD(t, sd)[1][[1]]  		 # Convert to beta shape parameter a.
b = gammaShRaFromModeSD(t, sd)[2][[1]]		     # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
Data = c(6, 3, 2, 15, 2, 4, 5)      # Data follow the Poisson distribution

openGraph(width=5,height=7)
posterior = PoisGamma( priorGamma=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="PoisGammaExample",type="png")



# Case 4: Mode = 5, SD = 5
# Specify the prior:
t = 1            	 # Specify the prior MODE.
sd= 20	             # Specify the effective prior sample size.
a = gammaShRaFromModeSD(t, sd)[1][[1]]  		 # Convert to beta shape parameter a.
b = gammaShRaFromModeSD(t, sd)[2][[1]]		     # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
Data = c(6, 3, 2, 15, 2, 4, 5)      # Data follow the Poisson distribution

openGraph(width=5,height=7)
posterior = PoisGamma( priorGamma=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="PoisGammaExample",type="png")


######################## 
#### Bayes Final 2 ##### 
########################

setwd("/Users/imac/Downloads/DBDA2Eprograms/")
# Example for Jags-Ymet-XmetSsubj-MrobustHierQuadWt.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load data file and specity column names of x (predictor) and y (predicted):

myData = read.csv( file="HierLinRegressData.csv" )
xName = "X" ; yName = "Y" ; sName="Subj" ; wName=NULL
fileNameRoot = "HierLinRegressData-Quad-Jags-" 

# myData = read.csv( file="BugsRatsData.csv" )
# xName = "Day" ; yName = "Weight" ; sName="Subj" ; wName=NULL
# fileNameRoot = "BugsRatsData-Quad-Jags-" 

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-XmetSsubj-MrobustHierQuadWt.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=myData , 
                    xName=xName , yName=yName , sName=sName , wName=wName ,
                    numSavedSteps=20000 , thinSteps=15 , saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)
# #------------------------------------------------------------------------------- 
# # Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in c("beta0mu","beta1mu","beta2mu","nu","sigma",
                   "beta0[1]","beta1[1]","beta2[1]") ) {
 diagMCMC( codaObject=mcmcCoda , parName=parName , 
           saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , 
          xName=xName , yName=yName , sName=sName , wName=wName ,
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 


# histogram of betas

hist(beta0)
hist(beta1)
hist(beta2)

mean(beta1)
# scatter plot
mcmcMat = as.matrix(mcmcCoda,chains=TRUE)
colnames(mcmcMat)

beta0 <- mcmcMat[,c(2:26)]
beta1 <- mcmcMat[,c(27:51)]
beta2 <- mcmcMat[,c(52:76)]

zbeta0 <- mcmcMat[,c(80:104)]
zbeta1 <- mcmcMat[,c(105:129)]
zbeta2 <- mcmcMat[,c(130:154)]

plot(beta0, beta1)
plot(beta1, beta2)
plot(beta0, beta2)
plot(zbeta0, zbeta1)
plot(zbeta1, zbeta2)
plot(zbeta0, zbeta2)


# Create a correlation matrix

beta <- cbind(beta0, beta1, beta2)

cor_beta <- cor(beta)
cor_zbetas <- cor(zbetas)

cor <- rbind(cor_betas, cor_zbetas)
write.csv(cor_beta, "/Users/imac/Downloads/cor.csv")




# histogram of betas (linear case)
mcmcMat = as.matrix(mcmcCoda,chains=TRUE)
colnames(mcmcMat)

beta0 <- mcmcMat[,c(2:26)]
beta1 <- mcmcMat[,c(27:51)]

zbeta0 <- mcmcMat[,c(54:78)]
zbeta1 <- mcmcMat[,c(79:103)]

hist(beta1)

######################## 
#### Bayes Final 3 ##### 
########################
setwd("/Users/imac/Downloads/DBDA2Eprograms/")
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load The data file 

myDataFrame = singer[singer$voice.part%in%c("Soprano 1", "Soprano 2", "Alto 1", "Alto 2"),]
myDataFrame$voice.part2 <- as.numeric(myDataFrame$voice.part%in%c("Soprano 1", "Soprano 2")) 
myDataFrame$voice.part2[myDataFrame$voice.part2==1] <- "Soprano"
myDataFrame$voice.part2[myDataFrame$voice.part2==0] <- "Alto"
myData <- myDataFrame[, c(1, 3)]

yName="height"
xName="voice.part2"
fileNameRoot = "TwoGroupHeightrobustHet-" 
RopeMuDiff=c(-0.5,0.5) ; RopeSdDiff=c(-0.5,0.5) ; RopeEff=c(-0.1,0.1)

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myData , yName=yName , xName=xName ,
                    numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=RopeMuDiff , 
                        RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=myData , yName=yName , xName=xName , 
          RopeMuDiff=RopeMuDiff , RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

myDataFrame <- singer
myData <- myDataFrame[myDataFrame$voice.part%in%c("Soprano 1", "Soprano 2"),]

yName="height"
xName="voice.part"
fileNameRoot = "TwoGroupHeightrobustHet-" 
RopeMuDiff=c(-0.5,0.5) ; RopeSdDiff=c(-0.5,0.5) ; RopeEff=c(-0.1,0.1)

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myData , yName=yName , xName=xName ,
                    numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=RopeMuDiff , 
                        RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=myData , yName=yName , xName=xName , 
          RopeMuDiff=RopeMuDiff , RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 


myDataFrame <- singer
myData <- myDataFrame[myDataFrame$voice.part%in%c("Alto 1", "Alto 2"),]

yName="height"
xName="voice.part"
fileNameRoot = "TwoGroupHeightrobustHet-" 
RopeMuDiff=c(-0.5,0.5) ; RopeSdDiff=c(-0.5,0.5) ; RopeEff=c(-0.1,0.1)

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myData , yName=yName , xName=xName ,
                    numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=RopeMuDiff , 
                        RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=myData , yName=yName , xName=xName , 
          RopeMuDiff=RopeMuDiff , RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
# Example for Jags-Ymet-Xnom1fac-MnormalHom.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load The data file 

# Comment out or uncomment one of the data sections below. 
# In RStudio, select section then press Cntrl-Shift-C
#singer <- read.csv("/Users/imac/Downloads/singer.csv")
#singer$X <- NULL
myDataFrame = singer
# Specify the column names in the data file relevant to the analysis:
yName="height" 
xName="voice.part" 
# Specify desired contrasts.
# Each main-effect contrast is a list of 2 vectors of level names, 
# a comparison value (typically 0.0), and a ROPE (which could be NULL):
contrasts = list( 
  list( c("Soprano 1"), c("Soprano 2"), compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
  list( c("Soprano 1") , c("Alto 1") , 
        compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
  list( c("Soprano 2") , c("Alto 2") , 
        compVal=0.0 , ROPE=c(-1.5,1.5) ) ,       
  list( c("Soprano 1","Soprano 2") , c("Alto 1","Alto 2") , 
        compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
  list( c("Alto 1") , c("Alto 2") , compVal=0.0 , ROPE=c(-1.5,1.5) ) 
)
# Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
fileNameRoot = "HeightData-NormalHom-" 
graphFileType = "eps" 

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom1fac-MnormalHom.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myDataFrame , yName=yName , xName=xName ,
                    numSavedSteps=11000 , thinSteps=10 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) 
show( parameterNames ) # show all parameter names, for reference
for ( parName in c("ySigma","b0","b[1]","aSigma") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:

summaryInfo = smryMCMC( mcmcCoda , 
                        datFrm=myDataFrame , xName=xName ,
                        contrasts=contrasts , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , 
          datFrm=myDataFrame , yName=yName , xName=xName ,
          contrasts=contrasts , 
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
PoisGamma = function( priorGammaAB , Data , plotType=c("Points","Bars")[2] ,
                     showCentTend=c("Mean","Mode","None")[3] ,
                     showHDI=c(TRUE,FALSE)[2] , HDImass=0.95 ,
                     showpD=c(TRUE,FALSE)[2] , ROPE=NULL ) {
  # priorBetaAB is two-element vector of beta a,b shape parameters
  # Data is vector of 0's and 1's.
  # source("DBDA2E-utilities.R") # for HDIofICDF()
  # For notational convenience, rename components of priorBetaAB:
  a = priorGammaAB[1]
  b = priorGammaAB[2]
  
  # Create summary values of Data:
  z = sum( Data ) 
  N = length( Data ) 
  
  Lambda = seq(0.001,30,by=0.001) # points for plotting
  pLambda = dgamma( Lambda , a , b ) # prior for plotting
  pLambdaGivenData = dgamma( Lambda , a+z , b+N ) # posterior for plotting
  pDataGivenLambda = dpois(z, N*Lambda) # likelihood for plotting

  # Plot the results.
  layout( matrix( c( 1,2,3 ) ,nrow=3 ,ncol=1 ,byrow=FALSE ) ) # 3x1 panels
  par( mar=c(3,3,1,0) , mgp=c(2,0.7,0) , mai=c(0.5,0.5,0.3,0.1) ) # margins
  cexAxis = 1.33
  cexLab = 1.75
  # convert plotType to notation used by plot:
  if ( plotType=="Points" ) { plotType="p" }
  if ( plotType=="Bars" ) { plotType="h" }
  dotsize = 5 # how big to make the plotted dots
  barsize = 5 # how wide to make the bar lines    
  # y limits for prior and posterior:
  yLim = c(0,1.1*max(c(pLambda,pLambdaGivenData)))
  
  # Plot the prior.
  plot( Lambda , pLambda , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0.01,30) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(lambda) , ylab=bquote(dgamma(lambda*"|"*.(a),.(b))) , 
        cex.lab=cexLab ,
        main="Prior (lambda)" , cex.main=1.5 , col="skyblue" )
  
  if ( showCentTend != "None" ) {
    if ( showCentTend == "Mean" ) {
      meanLambda = a/b 
      if ( meanLambda > .5 ) {
        textx = 0 ; textadj = c(0,1)
      } else {
        textx = 1 ; textadj = c(1,1)
      }
      text( textx , yLim[2] ,
            bquote( "mean=" * .(signif(meanLambda,3)) ) ,
            cex=2.0 , adj=textadj )
    }
    if ( showCentTend == "Mode" ) {
      if ( a+b-2 > 0 ) {
        modeLambda = (a-1)/b
        if ( modeLambda > .5 ) {
          textx = 0 ; textadj = c(0,1)
        } else {
          textx = 1 ; textadj = c(1,1)
        }
        text( textx , yLim[2] ,
              bquote( "mode=" * .(signif(modeLambda,3)) ) ,
              cex=2.0 , adj=textadj )
      }
    }
  }
  
  # Mark the highest density interval. HDI points are not thinned in the plot.
  if ( showHDI ) {
    if ( a+b-2 > 0 ) {
      HDIinfo = HDIofICDF( qbeta , shape1=a , shape2=b , credMass=HDImass )
      HDIheight = mean( dbeta(HDIinfo,shape1=a,shape2=b) )
      lines( HDIinfo , rep(HDIheight,length(HDIinfo)) )
      text( mean(HDIinfo) , HDIheight ,
            bquote( .(100*signif(HDImass,3)) * "% HDI" ) ,
            adj=c(0.5,-1.5) , cex=1.5 )
      # Mark the left and right ends of the waterline. 
      for ( i in 1:2 ) {
        lines( c(HDIinfo[i],HDIinfo[i]) , c(-0.5,HDIheight) , type="l" , lty=2 , 
               lwd=1.5 )
        text( HDIinfo[i] , HDIheight , bquote(.(round(HDIinfo[i],3))) ,
              adj=c(0.5,-0.15) , cex=1.2 )
      }
    }
  }
  # Mark the ROPE
  if ( !is.null(ROPE) ) {
    #pInRope = ( pbeta( ROPE[2] , shape1=a+z , shape2=b+N-z ) 
    #            - pbeta( ROPE[1] , shape1=a+z , shape2=b+N-z ) )
    pInRope = ( pbeta( ROPE[2] , shape1=a , shape2=b ) 
                - pbeta( ROPE[1] , shape1=a , shape2=b ) )
    ropeTextHt = 0.7*yLim[2]
    ropeCol = "darkred"
    lines( c(ROPE[1],ROPE[1]) , c(-0.5,ropeTextHt) , type="l" , lty=2 , 
           lwd=1.5 , col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(-0.5,ropeTextHt) , type="l" , lty=2 , 
           lwd=1.5 , col=ropeCol )
    text( mean(ROPE) , ropeTextHt ,
          paste0(ROPE[1],"<",round(pInRope,4)*100,"%<",ROPE[2]) ,
          adj=c(0.5,-0.15) , cex=1.2 , col=ropeCol )    
  }
  
  
  
  # Plot the likelihood: p(Data|Lambda)
  plot( Lambda , pDataGivenLambda , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0.01,30) , ylim=c(0,1.1*max(pDataGivenLambda)) , cex.axis=cexAxis ,
        xlab=bquote(lambda) , ylab=bquote( "p(D|" * lambda * ")" ) , 
        cex.lab=cexLab ,
        main="Likelihood (Poisson)" , cex.main=1.5 , col="skyblue" )
  if ( z > .5*N ) { textx = 0 ; textadj = c(0,1) }
  else { textx = 1 ; textadj = c(1,1) }
  text( textx ,1.0*max(pDataGivenLambda) ,cex=2.0
        ,bquote( "Data: z=" * .(z) * ",N=" * .(N) ) ,adj=textadj )
  if ( showCentTend != "None" ) {
    if ( showCentTend == "Mean" ) {
      meanLambda = z/N
      if ( meanLambda > .5 ) {
        textx = 0 ; textadj = c(0,1)
      } else {
        textx = 1 ; textadj = c(1,1)
      }
      text( textx , 0.7*max(pDataGivenLambda) ,
            bquote( "z/N = " * .(signif(meanLambda,3)) ) ,
            cex=2.0 , adj=textadj )
    }
    if ( showCentTend == "Mode" ) {
      modeLambda = Lambda[ which.max( pDataGivenLambda ) ]
      if ( modeLambda > .5 ) {
        textx = 0 ; textadj = c(0,1)
      } else {
        textx = 1 ; textadj = c(1,1)
      }
      text( textx , 0.7*max(pDataGivenLambda) ,
            bquote( "max at " * .(signif(modeLambda,3)) ) ,
            cex=2.0 , adj=textadj )
    }
  }
  
  # Plot the posterior.
  
  plot( Lambda , pLambdaGivenData , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0.01,30) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(lambda) , ylab=bquote(dgamma(lambda*"|"*.(a+z),.(b+N))) , 
        cex.lab=cexLab ,
        main="Posterior (gamma)" , cex.main=1.5 , col="skyblue" )
  
  if ( showCentTend != "None" ) {
    if ( showCentTend == "Mean" ) {
      meanLambda = (a+z)/(b+N) 
      if ( meanLambda > .5 ) {
        textx = 0 ; textadj = c(0,1)
      } else {
        textx = 1 ; textadj = c(1,1)
      }
      text( textx , yLim[2] ,
            bquote( "mean=" * .(signif(meanLambda,3)) ) ,
            cex=2.0 , adj=textadj )
    }
    if ( showCentTend == "Mode" ) {
      if ( a+z > 1 & b+N > 1 ) {
        modeLambda = (a+z-1)/(b+N)
        if ( modeLambda > .5 ) {
          textx = 0 ; textadj = c(0,1)
        } else {
          textx = 1 ; textadj = c(1,1)
        }
        text( textx , yLim[2] ,
              bquote( "mode=" * .(signif(modeLambda,3)) ) ,
              cex=2.0 , adj=textadj )
      }
    }
  }
  
  
  # Plot marginal likelihood pData:
  if ( showpD ) {
    meanLambda = (a+z)/(b+N)
    if ( meanLambda > .5 ) {
      textx = 0 ; textadj = c(0,1)
    } else {
      textx = 1 ; textadj = c(1,1)
    }
    text( textx , 0.55*yLim[2] , cex=2.0 ,
          bquote( "p(D)=" * .(signif(pData,3)) ) ,adj=textadj )
  }
  
  # Mark the highest density interval.
  if ( showHDI ) {
    if ( a+b+N-2  > 0 ) {
      HDIinfo = HDIofICDF( qgamma , shape1=a+z , shape2=b+N , credMass=HDImass)
      HDIheight = mean( dgamma(HDIinfo,shape1=a+z,shape2=b+N) )
      lines( HDIinfo , rep(HDIheight,length(HDIinfo)) )
      text( mean(HDIinfo) , HDIheight ,
            bquote( .(100*signif(HDImass,3)) * "% HDI" ) ,
            adj=c(0.5,-1.5) , cex=1.5 )
      # Mark the left and right ends of the waterline. 
      for ( i in 1:2 ) {
        lines( c(HDIinfo[i],HDIinfo[i]) , c(-0.5,HDIheight) , type="l" , lty=2 , 
               lwd=1.5 )
        text( HDIinfo[i] , HDIheight , bquote(.(round(HDIinfo[i],3))) ,
              adj=c(0.5,-0.15) , cex=1.2 )
      }
    }
  }
  # Mark the ROPE
  if ( !is.null(ROPE) ) {
    pInRope = ( pgamma( ROPE[2] , shape1=a+z , shape2=b+N ) 
                - pgama( ROPE[1] , shape1=a+z , shape2=b+N ) )
    ropeTextHt = 0.7*yLim[2]
    ropeCol = "darkred"
    lines( c(ROPE[1],ROPE[1]) , c(-0.5,ropeTextHt) , type="l" , lty=2 , 
           lwd=1.5 , col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(-0.5,ropeTextHt) , type="l" , lty=2 , 
           lwd=1.5 , col=ropeCol )
    text( mean(ROPE) , ropeTextHt ,
          paste0(ROPE[1],"<",round(pInRope,4)*100,"%<",ROPE[2]) ,
          adj=c(0.5,-0.15) , cex=1.2 , col=ropeCol )    
  }
  
  return( c(a+z,b+N) )
} # end of function