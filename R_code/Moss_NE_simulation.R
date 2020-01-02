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

reps <- 100 # Number of replicates

for(z in 1:length(sizes)){
  
  N <- sizes[z] # population size, must be even  number because I assume an equal sex ratio
  
  RunsMales <- matrix(data = 0, nrow = reps, ncol = N/2) # matrix of males
  
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
  NeM_results[z] <- mean(RunsNeM) # Mean NeM of all runs
  NeF_results[z] <- mean(RunsNeF) # Mean NeF of all runs
  NeA_results[z] <- mean(RunsNeA) # Mean NeA of all runs
}

### Plotting results
# Plots will have to be adjusted depending on parameters
# Export with size (4.3H X 5W)

plot(x=seq(1000,10000,by=1000),y=NeA_results,pch=16,xlab="Realized population size",ylab="Effective population size",cex.axis=.75,col="blue")
lines(x=seq(1000,10000,by=1000),y=NeA_results,col="blue")

lines(x=seq(1000,10000,by=1000),y=NeF_results,lty=1)
points(x=seq(1000,10000,by=1000),y=NeF_results,pch=16)

lines(x=seq(1000,10000,by=1000),y=NeM_results,lty=1)
points(x=seq(1000,10000,by=1000),y=NeM_results,pch=21,col="black",bg="white")

legend(1000,10000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,1),col=c("blue","black","black"),cex=.75,box.lwd=F,y.intersp=.5,bty="n",seg.len=3,pt.cex = c(1,1,.85))



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
sizesM <- c(5400,5250,5000,4500,3000,1500,1000,750,600) # males
sizesF <- c(600,750,1000,1500,3000,4500,5000,5250,5400) # females

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



plot(x=seq(1,9,by=1),y=NeF_results,pch=16,xlab="Females:Males",ylab="Effective population size",cex.axis=.75,ylim=c(0,11000),yaxt="n",xaxt="n")
lines(x=seq(1,9,by=1),y=NeF_results)

lines(x=seq(1,9,by=1),y=NeM_results,lty=1)
points(x=seq(1,9,by=1),y=NeM_results,pch=21,col="black",bg="white")

lines(x=seq(1,9,by=1),y=NeA_results,lty=1,col="blue")
points(x=seq(1,9,by=1),y=NeA_results,pch=16,col="blue")

xlabs = c("1:9","1:7","1:5","1:3","1:1","3:1","5:1","7:1","9:1")
axis(1,seq(1,9,by=1),labels=xlabs,cex.axis=.75,padj=-.5)
axis(2,seq(0,11000,by=2000),cex.axis=.75,padj=-.5)

legend(1,11000,c("Autosome","U chromosome","V chromosome"),pch=c(16,16,1),col=c("blue","black","black"),cex=.75,box.lwd=F,y.intersp=.5,bty="n",seg.len=3,pt.cex = c(1,1,.85))


