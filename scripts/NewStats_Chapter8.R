# 23/6/2015

##### The New Statistics with R, Chapter 8: Maximum Likelihood & GLMs

### Metadata
 # Data from Williams 1959; Analysis by Bill (William) Venables with input from John Nelder
 # R package: SemiPar
 # 36 paired values of wood density and timber hardness ('Janka' scale)
 # Aim: Prediction of hardness from density
 # dens (Density): Wood density in pounds per cubic foot
 # Hardness: Janka scale (pounds force required to push a ball bearing into wood sample)

### Set up
# Clear workspace
rm(list=ls(all=TRUE)) 
# Turn off significance stars
options(show.signif.stars= FALSE) 
# set working directory (customise to your own settings)
# setwd(...)

### Load all packages used:
library(SemiPar) # data
library(ggplot2) # graphics
library(MASS)    # Box-Cox
library(arm)     # display() etc.
library(grid)    # grid graphics

### Load data (preamble: reload data from Chapter 4...)
library(SemiPar) # load package
data(janka) # load data
names(janka) <- c("Density", "Hardness") # assign full names
janka # Box 4.1

###########
# Box 8.1 #
###########

###########
# Box 8.2 #
###########

### Fig 8.1: The Box-Cox transformations to the least squares model.
# refit least squares analysis using lm function taken from chapter 4
janka.ls1 <- lm(Hardness~Density, data= janka )
library(MASS)
setEPS()
postscript("Fig8-3.eps")
boxcox(janka.ls1) 
dev.off()
#########

###########
# Box 8.3 #
###########

# refit least squares analysis using lm function taken from chapter 4
janka.ls1 <- lm(Hardness~Density, data= janka )

# GLM equivalent using more general maximum likelihood methods
janka.ml1 <- glm(Hardness~Density, data= janka, family= gaussian(link="identity") )

# the glm() function performs the 'same' analysis as the lm() function...
anova(janka.ls1)
anova(janka.ml1)
# The deviances from the glm match the SS from the least squares model 

# Gamma GLM
janka.gamma <- glm(Hardness~Density, data= janka, family= Gamma(link= "sqrt"))

### Fig. 8.2
(Fig8.2 <- qplot(Density, Hardness, data= janka, 
                 geom= c("point","smooth"), 
                 method= "glm",formula= y~x, family= Gamma(link= "sqrt"),
                 main= "GLM, square-root link, Gamma variance")+theme_bw() )
# ggsave(file= "Fig8-2.tiff")
dev.off()
        
### Supplementary R code: Plotting a confidence interval for a glm curve
 # Where do the predictions for the lines and interval come from?
 # ggplot2 does a lot of work for you behind the scenes...
 # Drawing the graph using base graphics plot() functions needs a lot of code...
 # First, plot the basic graph
   plot(Hardness~Density, data= janka ) 
 # Generate a sequence of plenty of (100) densities to predict points for a smooth curve:
  (xseq <- seq( min(janka$Density), max(janka$Density), length= 100 ) )
 # Substitute this sequence for the observed densities
  (predictions <- predict(janka.gamma, list(Density= xseq), type= "response", se= TRUE ) )
 # type=response back-transforms estimates
 # with the 'se.fit' (or just 'se') argument set to true to get line plus interval
 # predict then returns an object with three components: 
 # predictions$fit gives the 'mean' regression line
 # predictions$se.fit gives the standard error for the regression 
 # predictions$residual.scale gives the ...TO DO 
 # upper and lower halves of a 95% confidence interval using t for P<0.05 & N=34: 
  (tau <- qt( 0.975, 34 ) # t for P<0.05 & N=34)
  (upper <- predictions$fit + tau*predictions$se.fit) 
  (lower <- predictions$fit - tau*predictions$se.fit)
 # Extract and plot the fitted curve: 
   lines(predictions$fit ~ xseq ) 
 # Add upper and lower bounds of a 95% confidence interval: 
   lines(upper ~ xseq, lty = 2)
   lines(lower ~ xseq, lty = 2 )
### End of supplementary R code ###########################

# intercept and slope:
coef(janka.gamma)

# Profile intervals:
confint(janka.gamma)

##### Fig 8.3 #####
# Prepare individual panels
(Fig8.3a <- qplot(Density, Hardness, data= janka, 
                  geom= c("point","smooth"), se= FALSE,
                  method= "glm",formula= y~x, family= Gamma(link= "sqrt"),
                  main= "GLM with untransformed data")+theme_bw() )
dev.off()
(Fig8.3b <- qplot(Density, sqrt(Hardness), data= janka, 
                   geom= c("point","smooth"), se= FALSE,
                   method= "lm",formula= y~x,
                   main= "Linear model with transformed data")+theme_bw() )
 dev.off()
# Save combined figure
setEPS()
postscript("Fig8-3.eps")
 grid.newpage()
 pushViewport(viewport(layout=grid.layout(1,2) ) )
 vplayout <- function(x, y)
 viewport(layout.pos.row=x, layout.pos.col=y)
 print(Fig8.3a, vp= vplayout(1,1)) 
 print(Fig8.3b, vp= vplayout(1,2))
dev.off()
#########

##### Supplementary R code #####
# qplot of residuals vs fitted (predicted) values:
qplot(fitted(janka.gamma), resid(janka.gamma), 
      ylab= "Residuals", xlab= "Predicted values")+theme_bw()+
  geom_hline(yintercept = 0, colour = "grey50") )
# Final inspection of all default residual plots:
par(mfrow= c(2,2))
plot(janka.gamma)
## End of supplementary R code
