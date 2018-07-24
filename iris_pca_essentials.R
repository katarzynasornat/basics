# PCA calculation

# data loading
data("iris")

# libraries loading
require(plotly)
require(dplyr)
require(factoextra)

# data vizualization using plotly library
p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length, color = ~Petal.Width)

# calculating variances of variables
iris %>% summarise_all(var) # the highest variance in the direction of variable Assault

# own implementation

own.pca <- function(dataset, if.standarize = TRUE)
{
  require(dplyr)
  if(if.standarize){
    dataset <- scale(dataset)
  }
  spectral_decomposition <- dataset %>% cov() %>% eigen()
  
  transformed_data <- as.matrix(dataset) %*% spectral_decomposition$vectors
  
  return(list(transf.data = transformed_data, loadings = spectral_decomposition$vectors, variances = spectral_decomposition$values))
}

# Bartlett Test
bart<-function(dat){ #dat is your raw data
  R<-cor(dat)
  p<-ncol(dat)
  n<-nrow(dat)
  chi2<- -((n-1)-((2*p)+5)/6 ) * log(det(R)) #this is the formula
  df<-(p*(p-1)/2)
  crit<-qchisq(.95,df) #critical value
  p<-pchisq(chi2,df,lower.tail=F) #pvalue
  cat("Bartlett's test of sphericity: X2(",
      df,")=",chi2,", p=",
      round(p,3),sep="" )   
}

# we should do the PCA: null hypotesis rejected


pca1 <- own.pca(iris[,1:4])
pca2 <- prcomp(iris[,1:4], scale = TRUE)
pca3 <- princomp(~.,data=iris[,1:4])

# first look at the eigenvalues
pca1$variances

# pca1 and pca2 the same output

# three methods to determine proper number of components
# 1. elbow
# 2. 70% of variance explained
# 3. eigenvalues >1 - means that new component explains more variance than one standarized variable (then variance is 1)

# Rule 3. shows that even one component describes our data in good way. But if we look at the elbow plot then we suppose to pick two components.
fviz_eig(pca2, addlabels = TRUE, ylim = c(0, 90))

summary(pca2)
# We can see in the third row, that first two components explain about 96% of total variance.
var <- get_pca_var(pca2)
var$cos2

# Value of cos2 for the first component is high for almost every variable except 'Sepal.Width'. It means that if we use only first component then explanation of 'Sepal.Width' would be poor. We decide to use first two components.
pca2$rotation
var$coord
var$contrib

# Petal.Length is positively and highly correlated with the first component (var$coord), so the longer petal, the highest value of the first component. 
# Furthermore coordinate which corresponds to Petal.Length for the first component is the highest and equals 0.5804131 (pca2$rotation). Also the contribution is pretty high (33.69%). 
# We analyze the rest in the same way and can describe the first component as 'petal.size' although we should remember that Sepal.Length has also significant impact. 
# In interpretation we should remember: the highest values of petal size, the highest values of pc1. The second we can call 'Sepal.Width'. And here we have: the highest values of pc2, the lower sepal width.

new_values <- pca2$x
fviz_pca_var(pca2, col.var = "black")

biplot(pca2)

# http://manuals.pqstat.pl/statpqpl:redpl:pcapl
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
