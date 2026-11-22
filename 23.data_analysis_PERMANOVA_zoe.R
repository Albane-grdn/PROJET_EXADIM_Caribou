# DATA ANALYSIS : PERMANOVA to test the significance of the ecotypes based on there
# genotypes or environnemental conditions.

# DATA -------------------------------------------------------------------------
data.gen.perm <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen.perm)<- data.gen.perm$individu # Turn individuals's ids into rownames 
data.gen.perm <- data.gen.perm[, -c(1)] # Suppress the individual column 


# PERMANOVA --------------------------------------------------------------------
## Package ade4 ----------------------------------------------------------------
adonis2(data.gen.perm ~ data.env$ecotype, data = data.env, permutations = 999, method = "euclidean", by = "terms") 
# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999
# 
# adonis2(formula = data.gen.perm ~ data.env$ecotype, data = data.env, permutations = 999, method = "euclidean", by = "terms")
# Df SumOfSqs      R2      F Pr(>F)    
# data.env$ecotype  4    30192 0.14642 3.5595  0.001 ***
#   Residual         83   176003 0.85358                  
# Total            87   206195 1.00000                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


pairwise.adonis2(data.gen.perm ~ ecotype, data = data.env, nperm = 999)
# $parent_call
# [1] "data.gen.perm ~ ecotype , strata = Null , permutations 999"
# 
# $Foret_boreale_vs_Arctique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.6534 0.09403 4.3591  0.001 ***
#   Residual 42   6.2956 0.90597                  
# Total    43   6.9491 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Foret_boreale_vs_Foret_Ouest
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.4439 0.04985 2.8856  0.001 ***
#   Residual 55   8.4604 0.95015                  
# Total    56   8.9043 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Foret_boreale_vs_Haut_Arctique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.6868 0.10924 4.6604  0.001 ***
#   Residual 38   5.6003 0.89076                  
# Total    39   6.2872 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Foret_boreale_vs_Foret_atlantique
# Df SumOfSqs     R2      F Pr(>F)    
# Model     1   0.8427 0.1098 5.7972  0.001 ***
#   Residual 47   6.8318 0.8902                  
# Total    48   7.6745 1.0000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Arctique_vs_Foret_Ouest
# Df SumOfSqs     R2      F Pr(>F)    
# Model     1   0.7133 0.1373 4.9339  0.001 ***
#   Residual 31   4.4816 0.8627                  
# Total    32   5.1949 1.0000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Arctique_vs_Haut_Arctique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1  0.49961 0.23554 4.3136  0.001 ***
#   Residual 14  1.62153 0.76446                  
# Total    15  2.12114 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Arctique_vs_Foret_atlantique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.8156 0.22231 6.5746  0.001 ***
#   Residual 23   2.8530 0.77769                  
# Total    24   3.6686 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Foret_Ouest_vs_Haut_Arctique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.7240 0.16052 5.1626  0.001 ***
#   Residual 27   3.7863 0.83948                  
# Total    28   4.5102 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Foret_Ouest_vs_Foret_atlantique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1   0.9473 0.15881 6.7964  0.001 ***
#   Residual 36   5.0178 0.84119                  
# Total    37   5.9651 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Haut_Arctique_vs_Foret_atlantique
# Df SumOfSqs      R2      F Pr(>F)    
# Model     1  0.77566 0.26443 6.8302  0.001 ***
#   Residual 19  2.15772 0.73557                  
# Total    20  2.93338 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# attr(,"class")
# [1] "pwadstrata" "list"  