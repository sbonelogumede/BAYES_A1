#### Required Functions ####
options(digits=4)
library(ggplot2)
library(MASS)

#### Data Generating Functions ####

generate_lost <- function(grid_size, nsims){
  
  # Function to generate the prior distribution for the 
  # location of the lost fisherman
  # Args: 
  #       grid_size: the dimensions of the square search grid
  #       nsims: number of samples to base the prior distribution on
  
  mu_vec  <- c(grid_size/2, grid_size/2)
  sig_mat <- matrix(c(2, 1, 5, 5), 2,2)
  
  dat <- mvrnorm(nsims, mu_vec, sig_mat)
  dat <- round(abs(dat))
  head(dat)
  summary(as.data.frame(dat))
  prior <- matrix(rep(0,grid_size^2), grid_size, grid_size)
  for (i in 1:NROW(dat)){
    
    if (dat[i,1] < grid_size & dat[i,2] < grid_size){
      prior[dat[i,1], dat[i,2]] <- prior[dat[i,1], dat[i,2]] + 1
    }
    
  }
  prior <- prior/sum(prior)
  return(prior)
}

generate_fisherman <- function(grid_size){
  
  # Function to generate the true location of the lost fisherman.
  # This should not effect the search decision in any way!! It is unkown
  # to the search crew.
  # Args: 
  #       grid_size: the dimensions of the square search grid

  
  mu_vec  <- c(grid_size/2, grid_size/2)
  sig_mat <- matrix(c(2, 1, 5, 5), 2,2)
  
  location  <- round(mvrnorm(1, mu_vec, sig_mat))
  true_grid <- matrix(rep(0, grid_size^2), grid_size, grid_size)
  true_grid[location[1], location[2]] <- 1
  
  return(true_grid)
}

#### Simulation ####

search_size <- 20
unifs <- runif(search_size^2, min = 0.6, max = 0.9)
##Step 2 - gain detection distribution
detect_pr <- matrix(unifs, ncol = search_size)

#...
##step 1 - prior of experts
prior <- generate_lost(search_size,10000)
#get fishermans location 
fish_grid <- generate_fisherman(search_size)
cell <- which(fish_grid == 1,arr.ind=TRUE)
rowf <- cell[1]
colf <- cell[2]

post_tracker <- numeric()
max_hours <- 48
fishermanfound <- FALSE
prior1 <- prior
##posterior=prior at first 
posterior <- prior
df_prior <- expand.grid(x = 1:search_size, y = 1:search_size)

df_prior$value <- as.vector(prior)
ggplot(df_prior, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "blue") +
  coord_fixed() +
  labs(title = "Heat map at step 1", fill = "Probability") +
  theme_minimal()

for (i in 1:max_hours){
  ##step 3 - merge 2 distributions  
  ##posterior initially prior ,we use the previous posterior as the new prior
  grid <- posterior*detect_pr
  ##step 4 - search cell with highest probabibility
  #cell_check <- which(grid == 1,arr.ind=TRUE)
  ##finding the index with the highest probability
  cell_check <- arrayInd(which.max(grid), dim(grid))
  row <- cell_check[1]
  col <- cell_check[2]
 ##check if fisherman is in the cell
  if(fish_grid[row,col]==1){
    ##use detection pr check if we find fisherman given he is in the cell.
    ##1 sample,1 trial using highest probability of cells
    detected <- rbinom(1, 1, detect_pr[row, col])
  }
    else {
      detected <- 0
    }
  post_tracker[i] <- posterior[rowf,colf]
  
  #step 5 -if not detected update using Bayes
  if (detected == 1) {
    cat("Fisherman here", i, "in cell (", row, ",", col, ")\n")
    fishermanfound <- TRUE
    break
  }
  pi <- detect_pr[row,col]
  posterior[row,col] <- ((1-pi)*posterior[row,col])/1 - pi * posterior[row, col]
  posterior[-row, -col] <- posterior[-row, -col] / 1 - pi * posterior[row, col]
  
}
print(prior1)
print(posterior)

df_posterior <- expand.grid(x = 1:search_size, y = 1:search_size)
df_posterior$value <- as.vector(posterior)
ggplot(df_posterior, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "orange", high = "purple") +
  coord_fixed() +
  labs(title = "Heatmap at final search step", fill = "Probability") +
  theme_minimal()



###################################################################################################DO 2B,D AND C(TRACK POSTERIOR PR OF OCCURENCE FOR CELL FISHERMAN ACTUALLY IN)

