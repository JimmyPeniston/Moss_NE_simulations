##### Code for measuring differences in effective population sizes between moss sexes ######

### The code has two major parts
# 1) Varying the population density
# 2) Varying the sex ratio

# Each part has three subsection
# 1) A single run of the simulation which can plot the arena
# 2) Multiple runs of the simulation
# 3) Full simulation which runs multiple runs for a range of parameter values

## Packages needed for plotting
require(reshape2)
require(ggplot2)

# Making colorblind friendly palette
colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


###########################################################
################# Varying the density #####################
###########################################################

# This simulation can be used to vary the density of indiviudals in the arena

####### Individual run of the simulation ########

##### Program ####
aRows <- 100 # Number of rows in spatial grid
aCols <- 100 # Number of columns in spatial grid

# Create empty spatial grid called "arena"
arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 

N <- 6000 # population size, must be even  number because I assume an equal sex ratio

# Randomly selecting locations of individuals on grid
locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)

males <- matrix(data = 0, nrow = 2, ncol = N/2) # matrix of males
females <- rep(0,N/2) # matrix of males

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
    females[f] <- locs[i] # Storing location
    arena[locs[i]] <- "F"  # placing on grid
    f = f + 1
  }
}

# arena # shows grid

females <- sample(females,(N/2))#shuffle order of males

fM <- 0 # store number of mated females

for(i in 1:length(females)){
  mates <- rep(0,0) # vector of indexes of potential mates
  
  fRow <-(females[i]%% aRows) # convert index to row
  if(fRow == 0){fRow = aRows}
  fCol <- floor(((females[i])/aCols)+.99) # convert index to col
  
  if(fRow-1>0){ #check if there is a female above
    if(arena[(fRow-1),(fCol)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-1)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows){ #check if there is a female below
    if(arena[(fRow+1),(fCol)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-1)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fCol-1>0){ #check if there is a female to the left
    if(arena[(fRow),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fCol+1<=aCols){ #check if there is a female to the right
    if(arena[(fRow),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow-1>0 && fCol-1>0){ #check diagonal up left
    if(arena[(fRow-1),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
    if(arena[(fRow-1),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
    if(arena[(fRow+1),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
    if(arena[(fRow+1),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  ## Mating
  if(length(mates) > 1){
    mate1 <- sample(mates,1) # randomly select mate
    mateX <- which(males[1,]==mate1) # index of mate
    males[2,mateX] = males[2,mateX] + 1
    
    # Record female as mated in arena
    arena[females[i]] <- "Mated" 
    fM = fM+1 # count how many females mated
  }
  if(length(mates) == 1){
    mateX <- which(males[1,]==mates) # index of mate
    males[2,mateX] = males[2,mateX] + 1
    
    # Record female as mated in arena
    arena[females[i]] <- "Mated" 
    fM = fM+1 # count how many females mated
  }
  
}

#### Plotting final arena ####
x1=melt(arena)
names(x1)=c("x","y","value")
levels(x1$value)=c("Empty","Unmated Female","Male","Mated Female")
qplot(x, y, fill=value, data=x1,geom='tile')+scale_fill_manual(values=c("gray70",colors[2],colors[3],colors[1]))
# Note, this plot looks wrong if the population size
# equals the arena size because there is one less level

## Calculating effective population size
# Scaler that is used to adjust fecundity to keep pop size constant
s1 <- (2/(mean(males[2,]))) # scale to keep pop size equal

# Applying scaler s1 to males
males[2,]<- sapply(males[2,],(function(x){x*s1}))
#mean(males[2,])
#var(males[2,])

# Calculating male (V chromosome) effective pop size
Ne <- ((2*N) - 2) / (2+var(males[2,]))

# Applying scaler to females
fList <- c(rep(s1,fM),rep(0,(N/2)-fM))
#mean(fList)
#var(fList)

# Calculating female (U chromosome) effective pop size
NeF <-  ((2*N) - 2) / (2+var(fList))

# Calculating autosome effective population size
alist <- c(males[2,],fList) # list of whole pop
Auto <- ((4*N) - 2) / (2+var(alist))

#histogram of number of mates per male
hist(males[2,]/s1)



####### Multiple runs of the simulation ########

reps <- 100 # Number of replicates

N <- 6000 # population size, must be even  number because I assume an equal sex ratio

RunsMales <- matrix(data = 0, nrow = reps, ncol = N/2) # matrix of males

RunsNeM <- rep(0,reps) # storing NeM values
RunsNeF <- rep(0,reps) # storing NeF values
RunsNeA <- rep(0,reps) # storing NeA values

for(k in 1:reps){
  ##### Program ####
  aRows <- 100 # Number of rows in spatial grid
  aCols <- 100 # Number of columns in spatial grid
  
  # Create empty spatial grid called "arena"
  arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 
  
  # Randomly selecting locations of individuals on grid
  locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)
  
  males <- matrix(data = 0, nrow = 2, ncol = N/2) # matrix of males
  females <- rep(0,N/2) # matrix of males
  
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
      females[m] <- locs[i] # Storing location
      arena[locs[i]] <- "F"  # placing on grid
      f = f + 1
    }
  }
  
  # arena # shows grid
  
  females <- sample(females,(N/2))#shuffle order of males
  
  fM <- 0 # store number of mated females
  
  for(i in 1:length(females)){
    mates <- rep(0,0) # vector of indexes of potential mates
    
    fRow <-(females[i]%% aRows) # convert index to row
    if(fRow == 0){fRow = aRows}
    fCol <- floor(((females[i])/aCols)+.99) # convert index to col
    
    if(fRow-1>0){ #check if there is a female above
      if(arena[(fRow-1),(fCol)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-1)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows){ #check if there is a female below
      if(arena[(fRow+1),(fCol)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-1)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fCol-1>0){ #check if there is a female to the left
      if(arena[(fRow),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fCol+1<=aCols){ #check if there is a female to the right
      if(arena[(fRow),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow-1>0 && fCol-1>0){ #check diagonal up left
      if(arena[(fRow-1),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
      if(arena[(fRow-1),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
      if(arena[(fRow+1),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
      if(arena[(fRow+1),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    ## Mating
    if(length(mates) > 1){
      mate1 <- sample(mates,1) # randomly select mate
      mateX <- which(males[1,]==mate1) # index of mate
      males[2,mateX] = males[2,mateX] + 1
      
      # Record female as mated in arena
      arena[females[i]] <- "Mated" 
      fM = fM+1 # count how many females mated
    }
    if(length(mates) == 1){
      mateX <- which(males[1,]==mates) # index of mate
      males[2,mateX] = males[2,mateX] + 1
      
      # Record female as mated in arena
      arena[females[i]] <- "Mated" 
      fM = fM+1 # count how many females mated
    }
  }
  
  # scaler to keep pop size equal
  s1 <- (2/(mean(males[2,]))) 
  
  # Applying scaler to males
  males[2,]<- sapply(males[2,],(function(x){x*s1}))
  
  # Calculating male (V chromosome) effective pop size
  NeM <- ((2*N) - 2) / (2+var(males[2,]))
  
  # Applying scaler s1 to females 
  fList <- c(rep(s1,fM),rep(0,(N/2)-fM))
  
  # Calculating male (U chromosome) effective pop size
  NeF <-  ((2*N) - 2) / (2+var(fList))
  
  # Calculating autosome effective population size
  alist <- c(males[2,],fList) # list of whole pop
  NeA <- ((4*N) - 2) / (2+var(alist))
  
  RunsMales[k,] <- (males[2,]/s1) # stores males values
  RunsNeM[k] <- NeM # Stores male Ne
  RunsNeF[k] <- NeF # Stores female Ne
  RunsNeA[k] <- NeA # Stores autosome Ne
}


mean(RunsNeM) # Mean NeM of all runs
mean(RunsNeF) # Mean NeF of all runs

### Creates a histogram of number of mates per male
hist(RunsMales,breaks=max(RunsMales),xaxt="n",col="gray70",xlab="Number of mates",ylab="Frequency")
axis(1,at=seq(0.5,max(RunsMales)+.5,by=1),labels=seq(0,max(RunsMales),by=1))
box(which="plot")


######### Full simulation ###########

# Loops through different densities
# Runs multiple replicates for each density
# Plots the results

# Vector of pop sizes to evaluate (must be even numbers)
sizes <- seq(1000,10000,by=1000)

# Vectors for storing results of different runs
NeM_results <- rep(0,length(sizes))
NeF_results <- rep(0,length(sizes))
NeA_results <- rep(0,length(sizes))

vSucM_results <- rep(0,length(sizes))
vSucF_results  <- rep(0,length(sizes))
vSucA_results  <- rep(0,length(sizes))

reps <- 100 # Number of replicates

for(z in 1:length(sizes)){
  
  N <- sizes[z] # population size, must be even  number because I assume an equal sex ratio
  
  RunsMales <- matrix(data = 0, nrow = reps, ncol = N/2) # matrix of males
  
  # Storing Ne results for each runs
  RunsNeM <- rep(0,reps)
  RunsNeF <- rep(0,reps)
  RunsNeA <- rep(0,reps)
  
  vSucM <- rep(0,reps)
  vSucF <- rep(0,reps)
  vSucA <- rep(0,reps)
  
  for(k in 1:reps){
    ##### Program ####
    aRows <- 100 # Number of rows in spatial grid
    aCols <- 100 # Number of columns in spatial grid
    
    # Create empty spatial grid called "arena"
    arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 
    
    # Randomly selecting locations of individuals on grid
    locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)
    
    males <- matrix(data = 0, nrow = 2, ncol = N/2) # matrix of males
    females <- rep(0,N/2) # matrix of males
    
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
        females[m] <- locs[i] # Storing location
        arena[locs[i]] <- "F"  # placing on grid
        f = f + 1
      }
    }
    
    # arena # shows grid
    
    females <- sample(females,(N/2))#shuffle order of males
    
    fM <- 0 # store number of mated females
    
    for(i in 1:length(females)){
      mates <- rep(0,0) # vector of indexes of potential mates
      
      fRow <-(females[i]%% aRows) # convert index to row
      if(fRow == 0){fRow = aRows}
      fCol <- floor(((females[i])/aCols)+.99) # convert index to col
      
      if(fRow-1>0){ #check if there is a female above
        if(arena[(fRow-1),(fCol)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-1)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows){ #check if there is a female below
        if(arena[(fRow+1),(fCol)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-1)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fCol-1>0){ #check if there is a female to the left
        if(arena[(fRow),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fCol+1<=aCols){ #check if there is a female to the right
        if(arena[(fRow),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow-1>0 && fCol-1>0){ #check diagonal up left
        if(arena[(fRow-1),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
        if(arena[(fRow-1),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
        if(arena[(fRow+1),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
        if(arena[(fRow+1),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      ## Mating
      if(length(mates) > 1){
        mate1 <- sample(mates,1) # randomly select mate
        mateX <- which(males[1,]==mate1) # index of mate
        males[2,mateX] = males[2,mateX] + 1
        
        # Record female as mated in arena
        arena[females[i]] <- "Mated" 
        fM = fM+1 # count how many females mated
      }
      if(length(mates) == 1){
        mateX <- which(males[1,]==mates) # index of mate
        males[2,mateX] = males[2,mateX] + 1
        
        # Record female as mated in arena
        arena[females[i]] <- "Mated" 
        fM = fM+1 # count how many females mated
      }
    }
    
    # scaler to keep pop size equal
    s1 <- (2/(mean(males[2,]))) 
    
    # Applying scaler to males
    males[2,]<- sapply(males[2,],(function(x){x*s1}))
    
    # Calculating male (V chromosome) effective pop size
    NeM <- ((2*N) - 2) / (2+var(males[2,]))
    
    # Applying scaler s1 to females 
    fList <- c(rep(s1,fM),rep(0,(N/2)-fM))
    
    # Calculating male (U chromosome) effective pop size
    NeF <-  ((2*N) - 2) / (2+var(fList))
    
    # Calculating autosome effective population size
    alist <- c(males[2,],fList) # list of whole pop
    NeA <- ((4*N) - 2) / (2+var(alist))
    
    RunsMales[k,] <- (males[2,]/s1) # stores males values
    RunsNeM[k] <- NeM # Stores male Ne
    RunsNeF[k] <- NeF # Stores female Ne
    RunsNeA[k] <- NeA # Stores autosome Ne
    
    ### Storing variances in reproductive success
    vSucM[k] <- var(males[2,])
    vSucF[k] <- var(fList)
    vSucA[k] <- var(alist)
    
  }
  NeM_results[z] <- mean(RunsNeM) # Mean NeM of all runs
  NeF_results[z] <- mean(RunsNeF) # Mean NeF of all runs
  NeA_results[z] <- mean(RunsNeA) # Mean NeA of all runs
  
  vSucM_results[z] <- mean(vSucM) # Mean vSucM of all runs
  vSucF_results[z]  <- mean(vSucF) # Mean vSucF of all runs
  vSucA_results[z]  <- mean(vSucA) # Mean vSucA of all runs
}

### Plotting effective pop results
# Plots will have to be adjusted depending on parameters
# Export with size (4.2H X 5W)

plot(x=seq(1000,10000,by=1000),y=NeA_results,pch=16,xlab=NA,ylab=NA,cex.axis=.75,col="black")
lines(x=seq(1000,10000,by=1000),y=NeA_results,col="black",lwd=1.5)

lines(x=seq(1000,10000,by=1000),y=NeF_results,lty=1,col=colors[7],lwd=1.5)
points(x=seq(1000,10000,by=1000),y=NeF_results,pch=16,col=colors[7])

lines(x=seq(1000,10000,by=1000),y=NeM_results,lty=1,col=colors[6],lwd=1.5)
points(x=seq(1000,10000,by=1000),y=NeM_results,pch=16,col=colors[6])

mtext(side = 1, line = 2.3, 'Population size',cex=1)
mtext(side = 2, line = 2.3, 'Effective population size',cex=1)

legend(1000,10000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[7],colors[6]),cex=.75,box.lwd=F,y.intersp=.5,bty="n",pt.cex = c(1,1,1))


## Plotting variances in reproductive success
plot(x=seq(1000,10000,by=1000),y=vSucA_results,pch=16,xlab=NA,ylab=NA,cex.axis=.75,col="blue",ylim=c(0,12))
lines(x=seq(1000,10000,by=1000),y=vSucA_results,col="blue")

lines(x=seq(1000,10000,by=1000),y=vSucF_results,lty=1)
points(x=seq(1000,10000,by=1000),y=vSucF_results,pch=16)

lines(x=seq(1000,10000,by=1000),y=vSucM_results,lty=1)
points(x=seq(1000,10000,by=1000),y=vSucM_results,pch=21,col="black",bg="white")


legend(7000,12,c("Males","Total Population","Females"),pch=c(1,16,16),col=c("black","blue","black"),cex=.75,box.lwd=F,y.intersp=.5,bty="n",seg.len=3,pt.cex = c(.85,1,1))


mtext(side = 1, line = 2.5, 'Realized population size',cex=.9)
mtext(side = 2, line = 2.5, 'Variance in reproductive success',cex=.9)



## Plotting differences variances in reproductive success
plot((x=seq(100,3000,by=100)/10000),y=(vSucM_results/vSucF_results),pch=16,xlab=NA,ylab=NA,cex.axis=.75,col="black",ylim=c(0,4))
lines((x=seq(100,3000,by=100)/10000),y=(vSucM_results/vSucF_results),col="black")

#Vr <- vRatio(.5)
#abline(h = Vr,col="black")

lines((x=seq(100,3000,by=100)/10000),y=(vSucF_results/vSucA_results),lty=1,col="blue")
points((x=seq(100,3000,by=100)/10000),y=(vSucF_results/vSucA_results),pch=16,col="blue")

mtext(side = 1, line = 2.5, 'Density',cex=.9)
mtext(side = 2, line = 2.5, expression(alpha),cex=.9,adj=.4)
mtext(side = 2, line = 2.5, expression(Beta),cex=.9,adj=.6,col="blue")
mtext(side = 2, line = 2.5, "or",cex=.9,adj=.5)


###########################################################
################# Varying the Sex Ratio ###################
###########################################################

# This code runs simulations where you can change the sex ratio

####### Individual run of the simulation ########
##### Program ####
NM <- 5400 # Number of males
NF <- 600 # Number of Females
N <- NM + NF # Tot Population size

aRows <- 100 # Number of rows in spatial grid
aCols <- 100 # Number of columns in spatial grid

# Create empty spatial grid called "arena"
arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 

# Randomly selecting locations of individuals on grid
locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)

males <- matrix(data = 0, nrow = 2, ncol = NM) # matrix of males
females <- rep(0,NF) # matrix of males

## Filling arena

m <- 1 # counter for males added 
f <- 1 # counter for females added 
for(i in 1:N){
  # alternate between placing of males and females on grid
  
  if(NM < NF){ # if less males than females 
    if(i%%2 == 0 && m <= NM){
      males[1,m] <- locs[i] # Storing location
      arena[locs[i]] <- "M" # placing on grid
      m = m + 1
    }
    else{
      females[f] <- locs[i] # Storing location
      arena[locs[i]] <- "F"  # placing on grid
      f = f + 1
    }
  }
  else{ # If more females than males
    if(i%%2 == 0 && f <= NF){
      females[f] <- locs[i] # Storing location
      arena[locs[i]] <- "F" # placing on grid
      f = f + 1
    }
    else{
      males[1,m] <- locs[i] # Storing location
      arena[locs[i]] <- "M" # placing on grid
      m = m + 1
    }
  }
}

# arena # shows grid

females <- sample(females,NF)#shuffle order of males

fM <- 0 # store number of mated females

for(i in 1:length(females)){
  mates <- rep(0,0) # vector of indexes of potential mates
  
  fRow <-(females[i]%% aRows) # convert index to row
  if(fRow == 0){fRow = aRows}
  fCol <- floor(((females[i])/aCols)+.99) # convert index to col
  
  if(fRow-1>0){ #check if there is a female above
    if(arena[(fRow-1),(fCol)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-1)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows){ #check if there is a female below
    if(arena[(fRow+1),(fCol)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-1)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fCol-1>0){ #check if there is a female to the left
    if(arena[(fRow),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fCol+1<=aCols){ #check if there is a female to the right
    if(arena[(fRow),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow-1>0 && fCol-1>0){ #check diagonal up left
    if(arena[(fRow-1),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
    if(arena[(fRow-1),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow-1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
    if(arena[(fRow+1),(fCol+1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
    if(arena[(fRow+1),(fCol-1)] == "M"){
      # convert males loctation back to index
      index <- (((fCol-2)*aCols) + (fRow+1)) 
      # add index to list of potential mates
      mates <- c(mates,index)
    }
  }
  
  ## Mating
  if(length(mates) > 1){
    mate1 <- sample(mates,1) # randomly select mate
    mateX <- which(males[1,]==mate1) # index of mate
    males[2,mateX] = males[2,mateX] + 1
    
    # Record female as mated in arena
    arena[females[i]] <- "Mated" 
    fM = fM+1 # count how many females mated
  }
  if(length(mates) == 1){
    mateX <- which(males[1,]==mates) # index of mate
    males[2,mateX] = males[2,mateX] + 1
    
    # Record female as mated in arena
    arena[females[i]] <- "Mated" 
    fM = fM+1 # count how many females mated
  }
}

#### Plotting final arena ####
x1=melt(arena)
names(x1)=c("x","y","value")
levels(x1$value)=c("Empty","Unmated Female","Male","Mated Female")
qplot(x, y, fill=value, data=x1,geom='tile')+scale_fill_manual(values=c("gray70",colors[2],colors[3],colors[1]))
# Note, this plot looks wrong if the population size
# equals the arena size because there is one less level

# scaler to keep pop size equal
s1 <- (2/(mean(males[2,]))) 

# Applying scaler to males
males[2,]<- sapply(males[2,],(function(x){x*s1}))

# Calculating male (V chromosome) effective pop size
NeM <- ((4*NM) - 2) / (2+var(males[2,]))

# Applying scaler s1 to females 
fList <- c(rep(s1,fM),rep(0,(NF)-fM))

# Calculating male (U chromosome) effective pop size
NeF <-  ((4*NF) - 2) / (2+var(fList))

# Calculating autosome effective population size
alist <- c(males[2,],fList) # list of whole pop
NeA <- ((4*N) - 2) / (2+var(alist))

#histogram of number of mates per male
hist(males[2,]/s1)



####### Running Multiple Reps ########

reps <- 100 # Number of replicates

N <- 6000 # population size, must be even  number because I assume an equal sex ratio

NM <- 3000 # number of males
NF <- 3000 # number of females

RunsMales <- matrix(data = 0, nrow = reps, ncol = NM) # matrix of males

RunsNeM <- rep(0,reps)
RunsNeF <- rep(0,reps)
RunsNeA <- rep(0,reps)

for(k in 1:reps){
  ##### Program ####
  aRows <- 100 # Number of rows in spatial grid
  aCols <- 100 # Number of columns in spatial grid
  
  # Create empty spatial grid called "arena"
  arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 
  
  # Randomly selecting locations of individuals on grid
  locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)
  
  males <- matrix(data = 0, nrow = 2, ncol = NM) # matrix of males
  females <- rep(0,NF) # matrix of males
  
  ## Filling arena
  
  m <- 1 # counter for males added 
  f <- 1 # counter for females added 
  for(i in 1:N){
    # alternate between placing of males and females on grid
    
    if(NM < NF){ # if less males than females 
      if(i%%2 == 0 && m <= NM){
        males[1,m] <- locs[i] # Storing location
        arena[locs[i]] <- "M" # placing on grid
        m = m + 1
      }
      else{
        females[f] <- locs[i] # Storing location
        arena[locs[i]] <- "F"  # placing on grid
        f = f + 1
      }
    }
    else{ # If more females than males
      if(i%%2 == 0 && f <= NF){
        females[f] <- locs[i] # Storing location
        arena[locs[i]] <- "F" # placing on grid
        f = f + 1
      }
      else{
        males[1,m] <- locs[i] # Storing location
        arena[locs[i]] <- "M" # placing on grid
        m = m + 1
      }
    }
  }
  
  # arena # shows grid
  
  females <- sample(females,NF)#shuffle order of males
  
  fM <- 0 # store number of mated females
  
  for(i in 1:length(females)){
    mates <- rep(0,0) # vector of indexes of potential mates
    
    fRow <-(females[i]%% aRows) # convert index to row
    if(fRow == 0){fRow = aRows}
    fCol <- floor(((females[i])/aCols)+.99) # convert index to col
    
    if(fRow-1>0){ #check if there is a female above
      if(arena[(fRow-1),(fCol)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-1)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows){ #check if there is a female below
      if(arena[(fRow+1),(fCol)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-1)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fCol-1>0){ #check if there is a female to the left
      if(arena[(fRow),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fCol+1<=aCols){ #check if there is a female to the right
      if(arena[(fRow),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow-1>0 && fCol-1>0){ #check diagonal up left
      if(arena[(fRow-1),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
      if(arena[(fRow-1),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow-1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
      if(arena[(fRow+1),(fCol+1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
      if(arena[(fRow+1),(fCol-1)] == "M"){
        # convert males loctation back to index
        index <- (((fCol-2)*aCols) + (fRow+1)) 
        # add index to list of potential mates
        mates <- c(mates,index)
      }
    }
    
    ## Mating
    if(length(mates) > 1){
      mate1 <- sample(mates,1) # randomly select mate
      mateX <- which(males[1,]==mate1) # index of mate
      males[2,mateX] = males[2,mateX] + 1
      
      # Record female as mated in arena
      arena[females[i]] <- "Mated" 
      fM = fM+1 # count how many females mated
    }
    if(length(mates) == 1){
      mateX <- which(males[1,]==mates) # index of mate
      males[2,mateX] = males[2,mateX] + 1
      
      # Record female as mated in arena
      arena[females[i]] <- "Mated" 
      fM = fM+1 # count how many females mated
    }
  }
  
  # scaler to keep pop size equal
  s1 <- (2/(mean(males[2,]))) 
  
  # Applying scaler to males
  males[2,]<- sapply(males[2,],(function(x){x*s1}))
  
  # Calculating male (V chromosome) effective pop size
  NeM <- ((4*NM) - 2) / (2+var(males[2,]))
  
  # Applying scaler s1 to females 
  fList <- c(rep(s1,fM),rep(0,(NF)-fM))
  
  # Calculating male (U chromosome) effective pop size
  NeF <-  ((4*NF) - 2) / (2+var(fList))
  
  # Calculating autosome effective population size
  alist <- c(males[2,],fList) # list of whole pop
  NeA <- ((4*N) - 2) / (2+var(alist))
  
  RunsMales[k,] <- (males[2,]/s1) # stores males values
  RunsNeM[k] <- NeM # Stores male Ne
  RunsNeF[k] <- NeF # Stores female Ne
  RunsNeA[k] <- NeA # Stores autosome Ne
  
}


mean(RunsNeM) # Mean NeM of all runs
mean(RunsNeF) # Mean NeF of all runs
mean(RunsNeA)

### Creates a histogram of number of mates per male
hist(RunsMales,breaks=max(RunsMales),xaxt="n",col="gray70",xlab="Number of mates",ylab="Frequency")
axis(1,at=seq(0.5,max(RunsMales)+.5,by=1),labels=seq(0,max(RunsMales),by=1))
box(which="plot")




######### Full simulation ###########

# Loops through different densities
# Runs multiple replicates for each sex ratio
# Plots the results

# Vector of sizes to evaluate

### N = 2000
#sizesM <- c(1800,1750,1667,1500,1000,500,333,250,200) # males
#sizesF <- c(200,250,333,500,1000,1500,1667,1750,1800) # females

### N = 4000
#sizesM <- c(3600,3500,3334,3000,2000,1000,666,500,400) # males
#sizesF <- c(400,500,666,1000,2000,3000,3334,3500,3600) # females

### N = 6000
#sizesM <- c(5400,5250,5000,4500,3000,1500,1000,750,600) # males
#sizesF <- c(600,750,1000,1500,3000,4500,5000,5250,5400) # females

### N = 8000
sizesM <- c(7200,7000,6667,6000,4000,2000,1333,1000,800) # males
sizesF <- c(800,1000,1333,2000,4000,6000,6667,7000,7200) # females

# Vectors for storing results of different runs
NeM_results <- rep(0,length(sizesM))
NeF_results <- rep(0,length(sizesF))
NeA_results <- rep(0,length(sizesM))

reps <- 100 # Number of replicates

for(z in 1:length(sizesM)){
  
  N <- sizesM[z] + sizesF[z] # total population size

  NM <- sizesM[z] # number of males
  NF <- sizesF[z] # number of females
  
  RunsMales <- matrix(data = 0, nrow = reps, ncol = NM) # matrix of males
  
  RunsNeM <- rep(0,reps)
  RunsNeF <- rep(0,reps)
  RunsNeA <- rep(0,reps)
  
  for(k in 1:reps){
    ##### Program ####
    aRows <- 100 # Number of rows in spatial grid
    aCols <- 100 # Number of columns in spatial grid
    
    # Create empty spatial grid called "arena"
    arena <- matrix(data = " ", nrow = aRows, ncol = aCols, byrow = FALSE, dimnames = NULL) 
    
    # Randomly selecting locations of individuals on grid
    locs <- sample(seq(1,(aRows*aCols),by=1),N,replace=FALSE)
    
    males <- matrix(data = 0, nrow = 2, ncol = NM) # matrix of males
    females <- rep(0,NF) # matrix of males
    
    ## Filling arena
    
    m <- 1 # counter for males added 
    f <- 1 # counter for females added 
    for(i in 1:N){
      # alternate between placing of males and females on grid
      
      if(NM < NF){ # if less males than females 
        if(i%%2 == 0 && m <= NM){
          males[1,m] <- locs[i] # Storing location
          arena[locs[i]] <- "M" # placing on grid
          m = m + 1
        }
        else{
          females[f] <- locs[i] # Storing location
          arena[locs[i]] <- "F"  # placing on grid
          f = f + 1
        }
      }
      else{ # If more females than males
        if(i%%2 == 0 && f <= NF){
          females[f] <- locs[i] # Storing location
          arena[locs[i]] <- "F" # placing on grid
          f = f + 1
        }
        else{
          males[1,m] <- locs[i] # Storing location
          arena[locs[i]] <- "M" # placing on grid
          m = m + 1
        }
      }
    }
    
    # arena # shows grid
    
    females <- sample(females,NF)#shuffle order of males
    
    fM <- 0 # store number of mated females
    
    for(i in 1:length(females)){
      mates <- rep(0,0) # vector of indexes of potential mates
      
      fRow <-(females[i]%% aRows) # convert index to row
      if(fRow == 0){fRow = aRows}
      fCol <- floor(((females[i])/aCols)+.99) # convert index to col
      
      if(fRow-1>0){ #check if there is a female above
        if(arena[(fRow-1),(fCol)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-1)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows){ #check if there is a female below
        if(arena[(fRow+1),(fCol)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-1)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fCol-1>0){ #check if there is a female to the left
        if(arena[(fRow),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fCol+1<=aCols){ #check if there is a female to the right
        if(arena[(fRow),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow-1>0 && fCol-1>0){ #check diagonal up left
        if(arena[(fRow-1),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow-1>0 && fCol+1<=aCols){ #check diagonal up right
        if(arena[(fRow-1),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow-1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows && fCol+1<=aCols){ #diagonal down right
        if(arena[(fRow+1),(fCol+1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      if(fRow+1<=aRows && fCol-1>0){ #check diagonal down left
        if(arena[(fRow+1),(fCol-1)] == "M"){
          # convert males loctation back to index
          index <- (((fCol-2)*aCols) + (fRow+1)) 
          # add index to list of potential mates
          mates <- c(mates,index)
        }
      }
      
      ## Mating
      if(length(mates) > 1){
        mate1 <- sample(mates,1) # randomly select mate
        mateX <- which(males[1,]==mate1) # index of mate
        males[2,mateX] = males[2,mateX] + 1
        
        # Record female as mated in arena
        arena[females[i]] <- "Mated" 
        fM = fM+1 # count how many females mated
      }
      if(length(mates) == 1){
        mateX <- which(males[1,]==mates) # index of mate
        males[2,mateX] = males[2,mateX] + 1
        
        # Record female as mated in arena
        arena[females[i]] <- "Mated" 
        fM = fM+1 # count how many females mated
      }
    }
    
    # scaler to keep pop size equal
    s1 <- (2/(mean(males[2,]))) 
    
    # Applying scaler to males
    males[2,]<- sapply(males[2,],(function(x){x*s1}))
    
    # Calculating male (V chromosome) effective pop size
    NeM <- ((4*NM) - 2) / (2+var(males[2,]))
    
    # Applying scaler s1 to females 
    fList <- c(rep(s1,fM),rep(0,(NF)-fM))
    
    # Calculating male (U chromosome) effective pop size
    NeF <-  ((4*NF) - 2) / (2+var(fList))
    
    # Calculating autosome effective population size
    alist <- c(males[2,],fList) # list of whole pop
    NeA <- ((4*N) - 2) / (2+var(alist))
    
    RunsMales[k,] <- (males[2,]/s1) # stores males values
    RunsNeM[k] <- NeM # Stores male Ne
    RunsNeF[k] <- NeF # Stores female Ne
    RunsNeA[k] <- NeA # Stores autosome Ne
  }
  
  NeM_results[z] <- mean(RunsNeM) # Mean NeM of all runs
  NeF_results[z] <- mean(RunsNeF) # Mean NeF of all runs
  NeA_results[z] <- mean(RunsNeA) # Mean NeA of all runs
}



plot(x=seq(1,9,by=1),y=NeF_results,pch=16,xlab="Females:Males",ylab="Effective population size",cex.axis=.75,ylim=c(0,4000),yaxt="n",xaxt="n",col=colors[7])
lines(x=seq(1,9,by=1),y=NeF_results,col=colors[7])

lines(x=seq(1,9,by=1),y=NeM_results,lty=1,col=colors[6])
points(x=seq(1,9,by=1),y=NeM_results,pch=16,col=colors[6])

lines(x=seq(1,9,by=1),y=NeA_results,lty=1,col="black")
points(x=seq(1,9,by=1),y=NeA_results,pch=16,col="black")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.75,padj=-.5)
axis(2,seq(0,4000,by=1000),cex.axis=.75,padj=-.5)

legend(1,4000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[6],colors[7]),cex=.75,box.lwd=F,y.intersp=.5,bty="n",seg.len=3,pt.cex = c(1,1,1))


########### Multipanel figure of sex ratio effect with
########### different densities
# Need to run the "full simulation" above for each density

dev.off()
par(mfrow=c(2,2))
par(mar= c(2.1, 2.1, 2.1, 1))
par(oma=c(1,1,1,1))

#### N 2000
#NeF_2000 <- NeF_results
#NeM_2000 <- NeM_results
#NeA_2000 <- NeA_results

plot(x=seq(1,9,by=1),y=NeF_2000,pch=16,xlab=NA,ylab=NA,cex.axis=.75,ylim=c(0,4000),yaxt="n",xaxt="n",col=colors[7])
lines(x=seq(1,9,by=1),y=NeF_2000,col=colors[7])

lines(x=seq(1,9,by=1),y=NeM_2000,lty=1,col=colors[6])
points(x=seq(1,9,by=1),y=NeM_2000,pch=16,col=colors[6])

lines(x=seq(1,9,by=1),y=NeA_2000,lty=1,col="black")
points(x=seq(1,9,by=1),y=NeA_2000,pch=16,col="black")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.6,padj=-1.3)
axis(2,seq(0,4000,by=1000),cex.axis=.6,padj=1)

mtext(side = 1, line = 1.5, 'Females:Males',cex=.6)
mtext(side = 2, line = 1.5, 'Effective population size',cex=.6)

legend(1,4200,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[7],colors[6]),cex=.75,box.lwd=F,y.intersp=.5,x.intersp=.5,bty="n",pt.cex = c(1,1,1))

mtext(side = 3, line = .25, expression(paste(italic(N)," = 2000")),cex=.7)



#### N 4000
#NeF_4000 <- NeF_results
#NeM_4000 <- NeM_results
#NeA_4000 <- NeA_results

plot(x=seq(1,9,by=1),y=NeF_4000,pch=16,xlab=NA,ylab=NA,cex.axis=.75,ylim=c(0,8000),yaxt="n",xaxt="n",col=colors[7])
lines(x=seq(1,9,by=1),y=NeF_4000,col=colors[7])

lines(x=seq(1,9,by=1),y=NeM_4000,lty=1,col=colors[6])
points(x=seq(1,9,by=1),y=NeM_4000,pch=16,col=colors[6])

lines(x=seq(1,9,by=1),y=NeA_4000,lty=1,col="black")
points(x=seq(1,9,by=1),y=NeA_4000,pch=16,col="black")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.6,padj=-1.3)
axis(2,seq(0,8000,by=2000),cex.axis=.6,padj=1)

mtext(side = 1, line = 1.5, 'Females:Males',cex=.6)
mtext(side = 2, line = 1.5, 'Effective population size',cex=.6)

legend(1,8200,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[7],colors[6]),cex=.75,box.lwd=F,y.intersp=.5,x.intersp=.5,bty="n",pt.cex = c(1,1,1))

mtext(side = 3, line = .25, expression(paste(italic(N)," = 4000")),cex=.7)



#### N 6000
#NeF_6000 <- NeF_results
#NeM_6000 <- NeM_results
#NeA_6000 <- NeA_results

plot(x=seq(1,9,by=1),y=NeF_6000,pch=16,xlab=NA,ylab=NA,cex.axis=.75,ylim=c(0,12000),yaxt="n",xaxt="n",col=colors[7])
lines(x=seq(1,9,by=1),y=NeF_6000,col=colors[7])

lines(x=seq(1,9,by=1),y=NeM_6000,lty=1,col=colors[6])
points(x=seq(1,9,by=1),y=NeM_6000,pch=16,col=colors[6])

lines(x=seq(1,9,by=1),y=NeA_6000,lty=1,col="black")
points(x=seq(1,9,by=1),y=NeA_6000,pch=16,col="black")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.6,padj=-1.3)
axis(2,seq(0,12000,by=4000),cex.axis=.6,padj=1)

mtext(side = 1, line = 1.5, 'Females:Males',cex=.6)
mtext(side = 2, line = 1.5, 'Effective population size',cex=.6)

legend(1,13000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[7],colors[6]),cex=.75,box.lwd=F,y.intersp=.5,x.intersp=.5,bty="n",seg.len=3,pt.cex = c(1,1,1))

mtext(side = 3, line = .25, expression(paste(italic(N)," = 6000")),cex=.7)




#### N 8000
#NeF_8000 <- NeF_results
#NeM_8000 <- NeM_results
#NeA_8000 <- NeA_results

plot(x=seq(1,9,by=1),y=NeF_8000,pch=16,xlab=NA,ylab=NA,cex.axis=.75,ylim=c(0,16000),yaxt="n",xaxt="n",col=colors[7])
lines(x=seq(1,9,by=1),y=NeF_8000,col=colors[7])

lines(x=seq(1,9,by=1),y=NeM_8000,lty=1,col=colors[6])
points(x=seq(1,9,by=1),y=NeM_8000,pch=16,col=colors[6])

lines(x=seq(1,9,by=1),y=NeA_8000,lty=1,col="black")
points(x=seq(1,9,by=1),y=NeA_8000,pch=16,col="black")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.6,padj=-1.3)
axis(2,seq(0,16000,by=4000),cex.axis=.55,padj=1.2)

mtext(side = 1, line = 1.5, 'Females:Males',cex=.6)
mtext(side = 2, line = 1.5, 'Effective population size',cex=.6)

legend(1,17000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,16),col=c("black",colors[7],colors[6]),cex=.75,box.lwd=F,y.intersp=.5,x.intersp=.5,bty="n",seg.len=3,pt.cex = c(1,1,1))

mtext(side = 3, line = .25, expression(paste(italic(N)," = 8000")),cex=.7)





###########################################################
##### Calculating ratios of variances in success ##########
###########################################################

# Parameter values
mu <- 10^-8 # mutation rate
N <- 400000 # realized population size
U <- 0.005 # theta of U
V <- 0.0025
A <- 0.01
lambda <- V/U

# Function giving alpha:
# Ratio of variance in U to V
vRatio <- function(x){
  ((8*mu*((N-1)/(x*U)))-2) / ((8*mu*((N-1)/(U)))-2)
}

# Testing
vRatio(.5)

# Function giving Beta:
# Ratio of variance in U to A
vRatioA <- function(x){
  ((8*mu*((N-1)/(x*A)))-2) / ((8*mu*(((2*N)-1)/(A)))-2)
}

# Testing
vRatioA(.5)
