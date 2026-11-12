# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

# Call to latest processed data function 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))


data.gen.perm <- as.data.frame(t(data.gen[,2:6001]))
colnames(data.gen.perm) <- data.gen$individu
data.env.perm <- as.data.frame(t(data.env[,2:12]))
colnames(data.env.perm) <- data.env$individu
