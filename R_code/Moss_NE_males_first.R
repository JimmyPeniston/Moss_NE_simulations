##### Code for measuring differences in effective population sizes between moss sexes ######

## Packages needed for plotting
require(reshape2)
require(ggplot2)

# Making colorblind friendly palette
colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


##### Program ####
aRows <- 100 # Number of rows in spatial grid
aCols <- 100 # Number of columns in spatial grid
  
# Create empty spatial grid called "arena"
arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 

N <- 9000 # population size, must be even  number because I assume an equal sex ratio

# Randomly selecting locations of individuals on grid
locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)

males <- matrix(data = 0, nrow = 2, ncol = N/2) # matrix of males
females <- matrix(data = NA, nrow = 1, ncol = N/2) # matrix of males

m <- 1 # counter for males added 
f <- 1 # counter for males added 
for(i in 1:N){
  # alternate between placing of males and females on grid
  if(i%%2 == 0){
    males[1,m] <- locs[i] # Storing location
    arena[locs[i]] <- "M" # placing on grid
    m = m + 1
  }
  else{
    females[1,m] <- locs[i] # Storing location
    arena[locs[i]] <- "F"  # placing on grid
    f = f + 1
  }
}

# arena # shows grid

males[1,] <- sample(males[1,],(N/2))#shuffle order of males

fM <- 0 # store number of mated females

for(i in 1:length(males[1,])){
  mRow <-(males[1,i]%% aRows) # convert index to row
  if(mRow == 0){mRow = aRows}
  mCol <- floor(((males[1,i])/aCols)+.99) # convert index to col
  
  if(mRow-1>0){ #check if there is a female above
    if(arena[(mRow-1),(mCol)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow-1),(mCol)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mRow+1<=aRows){ #check if there is a female below
    if(arena[(mRow+1),(mCol)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow+1),(mCol)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mCol-1>0){ #check if there is a female to the left
    if(arena[(mRow),(mCol-1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow),(mCol-1)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mCol+1<=aCols){ #check if there is a female to the right
    if(arena[(mRow),(mCol+1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow),(mCol+1)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mRow-1>0 && mCol-1>0){ #check diagonal up left
    if(arena[(mRow-1),(mCol-1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow-1),(mCol-1)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mRow-1>0 && mCol+1<=aCols){ #check diagonal up right
    if(arena[(mRow-1),(mCol+1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow-1),(mCol+1)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mRow+1<=aRows && mCol+1<=aCols){ #check diagonal down right
    if(arena[(mRow+1),(mCol+1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow+1),(mCol+1)] = "Mated"
      fM <- fM + 1
    }
  }
  
  if(mRow+1<=aRows && mCol-1>0){ #check diagonal down left
    if(arena[(mRow+1),(mCol-1)] == "F"){
      males[2,i] <- males[2,i] + 1
      arena[(mRow+1),(mCol-1)] = "Mated"
      fM <- fM + 1
    }
  }
}

#males # outputs males. Row1 = location, Row2 = number of mates
#fM # outputs number of females mated


#### Plotting final arena ####

x1=melt(arena)
names(x1)=c("x","y","value")
levels(x1$value)=c("Empty","Unmated Female","Male","Mated Female")
qplot(x, y, fill=value, data=x1,geom='tile')+scale_fill_manual(values=c("gray70",colors[2],colors[3],colors[1]))


## Calculating effective population size
## I don't think I have this correct yet
s1 <- (2/(mean(males[2,]))) # scale to keep pop size equal

males[2,]<- sapply(males[2,],(function(x){x*s1}))
mean(males[2,])
var(males[2,])

Ne <- ((2*N) - 2) / (2+var(males[2,]))

fList <- c(rep(s1,fM),rep(0,(N/2)-fM))

mean(fList)
var(fList)
NeF <-  ((2*N) - 2) / (2+var(fList))

Ne / NeF
hist(males[2,])



####### Running Multiple Reps ########
reps <- 100

RunsMales <- matrix(data = 0, nrow = reps, ncol = N/2) # matrix of males

RunsNeM <- rep(0,reps)
RunsNeF <- rep(0,reps)

for(k in 1:reps){
  ##### Program ####
  aRows <- 100 # Number of rows in spatial grid
  aCols <- 100 # Number of columns in spatial grid
  
  # Create empty spatial grid called "arena"
  arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 
  
  N <- 10000 # population size, must be even  number because I assume an equal sex ratio
  
  # Randomly selecting locations of individuals on grid
  locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)
  
  males <- matrix(data = 0, nrow = 2, ncol = N/2) # matrix of males
  females <- matrix(data = NA, nrow = 1, ncol = N/2) # matrix of males
  
  m <- 1 # counter for males added 
  f <- 1 # counter for males added 
  for(i in 1:N){
    # alternate between placing of males and females on grid
    if(i%%2 == 0){
      males[1,m] <- locs[i] # Storing location
      arena[locs[i]] <- "M" # placing on grid
      m = m + 1
    }
    else{
      females[1,m] <- locs[i] # Storing location
      arena[locs[i]] <- "F"  # placing on grid
      f = f + 1
    }
  }
  
  # arena # shows grid
  
  males[1,] <- sample(males[1,],(N/2))#shuffle order of males
  
  fM <- 0 # store number of mated females
  
  for(i in 1:length(males[1,])){
    mRow <-(males[1,i]%% aRows) # convert index to row
    if(mRow == 0){mRow = aRows}
    mCol <- floor(((males[1,i])/aCols)+.99) # convert index to col
    
    if(mRow-1>0){ #check if there is a female above
      if(arena[(mRow-1),(mCol)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow-1),(mCol)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mRow+1<=aRows){ #check if there is a female below
      if(arena[(mRow+1),(mCol)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow+1),(mCol)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mCol-1>0){ #check if there is a female to the left
      if(arena[(mRow),(mCol-1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow),(mCol-1)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mCol+1<=aCols){ #check if there is a female to the right
      if(arena[(mRow),(mCol+1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow),(mCol+1)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mRow-1>0 && mCol-1>0){ #check diagonal up left
      if(arena[(mRow-1),(mCol-1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow-1),(mCol-1)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mRow-1>0 && mCol+1<=aCols){ #check diagonal up right
      if(arena[(mRow-1),(mCol+1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow-1),(mCol+1)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mRow+1<=aRows && mCol+1<=aCols){ #check diagonal down right
      if(arena[(mRow+1),(mCol+1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow+1),(mCol+1)] = "Mated"
        fM <- fM + 1
      }
    }
    
    if(mRow+1<=aRows && mCol-1>0){ #check diagonal down left
      if(arena[(mRow+1),(mCol-1)] == "F"){
        males[2,i] <- males[2,i] + 1
        arena[(mRow+1),(mCol-1)] = "Mated"
        fM <- fM + 1
      }
    }
  }
  s1 <- (2/(mean(males[2,]))) # scale to keep pop size equal
  
  males[2,]<- sapply(males[2,],(function(x){x*s1}))
  
  NeM <- ((2*N) - 2) / (2+var(males[2,]))
  
  fList <- c(rep(s1,fM),rep(0,(N/2)-fM))
  
  NeF <-  ((2*N) - 2) / (2+var(fList))
  
  RunsMales[k,] <- males[2,]
  RunsNeM[k] <- NeM
  RunsNeF[k] <- NeF
}

NeM_results[10] =  mean(RunsNeM)
NeF_results[10] =  mean(RunsNeF)

plot(x=seq(1000,10000,by=1000),y=NeF_results,pch=16,xlab="Realized population size",ylab="Effective population size",cex.axis=.8,xlim=c(0,10000))
lines(x=seq(1000,10000,by=1000),y=NeF_results)

points(x=seq(1000,10000,by=1000),y=NeM_results)
lines(x=seq(1000,10000,by=1000),y=NeM_results,lty=2)

legend(1000,9000,c("Females","Males"),pch=c(16,1),cex=.9,box.lwd=F,y.intersp=.7,bty="n",seg.len=3)


NeM_results <- rep(0,10)
NeF_results <- rep(0,10)

#mean(RunsNeM)/mean(RunsNeF)

#mean(RunsNeM) / N
#mean(RunsNeF) / N

#hist(RunsMales)
