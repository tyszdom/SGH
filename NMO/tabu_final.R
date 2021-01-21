#################################
##### Variables description #####
#################################

# size -> the length of the binary configuration
# iterations -> the number of iterations in the preliminary search of the algorithm
# objFunc -> the objective function is required to take as an argument a vector of zeros and ones
# neigh -> a number of neighbour configurations to check at each iteration
# listSize -> tabu list size
# nRestarts -> the maximum number of restarts in the intensification stage of the algorithm
# repeatAll -> the number of times to repeat the search
# config -> a starting configuration
# verbose -> if true, the name of the current stage of the algorithm is printed (preliminary, intensification, diversification)


####################
##### Function #####
####################

tabufunction <-
  function(size, 
           iterations, 
           objFunc = NULL, 
           neigh = size, 
           listSize, 
           nRestarts,
           repeatAll, 
           config = NULL, 
           verbose = FALSE){
    
    # Variables constraint
    
    if (size < 2){
      stop("Config is too short. Size must be at least 2.")
    }
    if (iterations < 2){
      stop("Not enough iterations. Number of iterations must be at least 2.")
    }
    if (listSize >= size){
      stop("Tabu list size must be lower than size.")
    }
    if (neigh > size){
      stop("Number of tabu neighbour configurations can't be greater than size.")
    }
    if (is.null(objFunc)) {
      stop("A evaluation function must be provided. See the objFunc parameter.")
    }
    if (is.null(config)) {
      config <- matrix(0, 1, size) 
      config[sample(1:size, sample(1:size, 1))] <- 1
    }
    else if (size != length(config)){
      stop("Length of the starting configuration must be equal to size.")
    }
    if (repeatAll < 1){
      stop("repeatAll parameter must be greater than 0")
    }
    
    iter <- 1
    configKeep <- matrix( , repeatAll * iterations * (nRestarts + 4), size)
    eUtilityKeep <- vector( , repeatAll * iterations * (nRestarts + 4))
    
    for(j in 1:repeatAll){
      
      if (j > 1) {
        config <- matrix(0, 1, size) 
        config[sample(1:size, sample(1:size, 1))] <- 1
      }
      
      tabuList <- matrix(0, 1, size)
      listOrder <- matrix(0, 1, listSize)
      
      # Highest utility found
      eUtility <- objFunc(config)
      aspiration <- eUtility    
      
      
      preliminarySearch<- function(){
        
        configKeep[iter, ] <- config
        eUtilityKeep[iter] <- eUtility
        iter <- iter + 1
        
        for (i in 2:iterations){
          neighboursEUtility <- matrix(0, 1, size)  
          configTemp <- t(matrix(config, size, neigh))
          # Random neighbours pick
          randomNeighbours <- sample(size, neigh) 
          diag(configTemp[, randomNeighbours]) <- abs(diag(configTemp[, randomNeighbours]) - 1) #flip
          neighboursEUtility[randomNeighbours] <- apply(configTemp, 1, objFunc) 
          Nontaboo <- neighboursEUtility[tabuList == 0]
          maxNontaboo <- max(Nontaboo)
          Taboo <- neighboursEUtility[tabuList == 1]
          maxTaboo <- max(Taboo, -Inf)
          
          # New move finding
          
          move <- ifelse(maxTaboo > maxNontaboo & maxTaboo > aspiration, 
                         ifelse(length(which(neighboursEUtility == maxTaboo)) == 1, 
                                which(neighboursEUtility == maxTaboo), 
                                sample(which(neighboursEUtility == maxTaboo), 1)),  
                         ifelse(length(which(neighboursEUtility == maxNontaboo & tabuList == 0)) == 1, 
                                which(neighboursEUtility == maxNontaboo & tabuList == 0), 
                                sample(which(neighboursEUtility == maxNontaboo & tabuList == 0), 1)))
          
          # Add move to tabu list in case new utility is lower than old 

          if (eUtility >= neighboursEUtility[move]){
            tabuList[move] <- 1
            if(sum(tabuList) > listSize){ 
              # if tabu list is full
              tabuList[listOrder[1]] <- 0
              listOrder[1:listSize] <- c(listOrder[2:listSize], 0)
            } 
            listOrder[min(which(listOrder == 0))] <- move
          }
          else if(neighboursEUtility[move] > aspiration) aspiration <- neighboursEUtility[move]
          
          # new move
          eUtility <- neighboursEUtility[move]
          config[move] <- abs(config[move]-1)
          configKeep[iter,] <- config
          eUtilityKeep[iter] <- eUtility
          iter <- iter + 1
        }
        
        result = list(aspiration = aspiration,  
                      configKeep = configKeep, 
                      eUtilityKeep = eUtilityKeep, 
                      iter = iter)
        return(result)
      }
      
      # Preliminary
      
      if (verbose) cat("Stage of preliminary search.\n")
      result <- preliminarySearch()
      aspiration <- result$aspiration
      configKeep <- result$configKeep
      eUtilityKeep <- result$eUtilityKeep
      iter <- result$iter
      
      # Intensification
      
      tempo <- 0
      restarts <- 0
      
      while(tempo < aspiration & restarts < nRestarts){
        if (verbose) cat("Stage of intensification.\n")
        eUtility <- max(eUtilityKeep)
        tempo <- aspiration
        config <- configKeep[max(which(eUtilityKeep == max(eUtilityKeep))), ]
        result <- preliminarySearch()  
        aspiration <- result$aspiration
        configKeep <- result$configKeep
        eUtilityKeep <- result$eUtilityKeep
        iter <- result$iter
        restarts <- restarts + 1
      }
      
      # Diversification
      
      if (verbose) cat("Stage of diversification.\n")
      config <- matrix(0, 1, size) 
      config[sample(1:size, sample(1:size, 1))] <- 1
      eUtility <- objFunc(config)
      
      # New tabu list made from most frequent moves
      
      frequent <- apply(configKeep, 2, function(x)sum(diff(x) != 0))         
      tabuList <- as.numeric(rank(frequent, ties.method =  "random") > (size - listSize))
      listOrder <- sample(which(rank(frequent, ties.method =  "random") > (size - listSize)), listSize)
      
      result <- preliminarySearch()
      iter <- result$iter
      configKeep <- result$configKeep
      eUtilityKeep <- result$eUtilityKeep
    }
    
    endResult <- list(type = "binary", configKeep = configKeep[1:(iter - 1), ], 
                      eUtilityKeep = eUtilityKeep[1:(iter - 1)], iterations = iterations, neigh = neigh, 
                      listSize = listSize,  repeatAll = repeatAll)
    class(endResult) = "tabu"  
    return(endResult)
  }


###################
##### Example #####
###################

v_objfunc <- function(th)return(1)

example_result <- tabufunction(size = 10, iterations = 100, objFunc = v_objfunc,listSize = 9, nRestarts = 10, repeatAll = 1)

summary(example_result)

library(tabuSearch)
plot(example_result)

# Best index found
example_index = which.max(example_result$eUtilityKeep) 

# Print result
cat("best result:",example_result$configKeep[example_index,],"\nvalue:",example_result$eUtilityKeep[example_index],"\n")


############################
##### Exercise example #####
############################

# Execute the optimization of the restrigin function (D = 8) using tabusearch method. Adopt a binary representation 
# such that each dimension value is encoded into 8 bits, denoting any of the 256 regular levels within the range
# [-5.2, 5.2]. Use the control parameters: maxit = 500, N = 8, L = 8, nRestarts = 1 and a randomly generated initial point.

# From Modern Optimization with R

rastrigin=function(x) f=10*length(x)+sum(x^2-10*cos(2*pi*x))

intbin=function(x){ 
  sum(2^(which(rev(x==1))-1)) 
  }  # convert binary to integer

breal=function(x) { #convert binary to D real values; note: D and bits need to be set outside this function
  s=vector(length=D) 
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*bits+1;end=ini+bits-1
  n=intbin(x[ini:end])
  s[i]=lower+n*drange/2^bits 
  }
  return(s)
}

# tabusearch does not work well with negative evaluations to solve this drawback, a MAXIMUM constant is defined

MAXIMUM=10000 
brastrigin=function(x) MAXIMUM-rastrigin(breal(x)) # max. goal

D=8
MAXIT=500
bits=8 # per dimension
size=D*bits
lower=-5.2
upper=5.2
drange=upper-lower
s=sample(0:1,size=size,replace=TRUE)

exercise_result <- tabufunction(size=size, iterations=MAXIT, objFunc=brastrigin, config=s, neigh=bits, listSize=bits, nRestarts=1, repeatAll = 1)

summary(exercise_result)

plot(exercise_result)

# Best index found
ib=which.max(exercise_result$eUtilityKeep) 
# Print result
cat("Combination:",exercise_result$configKeep[ib,],"\n","\nMax:",MAXIMUM-exercise_result$eUtilityKeep[ib],"\n")
