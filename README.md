# Sparse Modelling with Categorical Predictors

Lasso as a Sparse Modelling Technique is not a good choice in case of Mixed predictor-type problems. 
Instead, a combination of Group Lasso, Variable Fusion, and Fused Lasso Penalty should be used. A Data Analysis is performed to check the adequacy of the suggested method.

Original Paper : [Sparse modeling of categorial explanatory variables](https://arxiv.org/pdf/1101.1421#:~:text=If%20independent%20variables%20are%20categorial,one%20for%20ordinal%20predic%2D%20tors)

However, we have used a more general method called the "smurf Algorithm". Its implementation is available in **R** as Package - 
[smurf: Sparse Multi-Type Regularized Feature Modeling](https://cran.r-project.org/web/packages/smurf/) 





