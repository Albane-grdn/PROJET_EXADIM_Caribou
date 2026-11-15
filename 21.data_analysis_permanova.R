# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

# Call to latest processed data function 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen)<- data.gen$individu # Turn individuals's ids into rownames 
data.gen <- data.gen[, -c(1)] # Suppress the individual column 
data.env <- readRDS(file = file.path(latest.env)) # load latest version of environmental data
# data.gen[, 6001] <- data.env$ecotype # Add ecotype membership
# colnames(data.gen)[6001] <- "pop" # rename to match with genind objext syntax

# data.gen.perm <- as.data.frame(t(data.gen[,2:6001]))
# colnames(data.gen.perm) <- data.gen$individu
# data.env.perm <- as.data.frame(t(data.env[,2:12]))
# colnames(data.env.perm) <- data.env$individu

# adegenet ---------------------------------------------------------------------
## Data management  ------------------------------------------------------------
gen_ind <- as.genind(data.gen, pop = data.env$ecotype) # Turn data into genind object
pop(gen_ind) # nb of ecotypes included in the analysis 

gen_pop <- genind2genpop(gen_ind) # Converting to genpop object
















