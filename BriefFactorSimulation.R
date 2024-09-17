# Brief simulation for the case of three item math test using either only arithmetic items or adding one geometry item

# a is the variance shared by all items
a <- 0.3
# b is the variance shared by each pair of items
b12 <- 0.1 # this represents the variance shared by all arithmetic items
b13 <- 0.1 #    this is placed here and not in a to show the impact of adding in non-arithmetic items
b23 <- 0.1 #    in the next stage.
# u is the uniqueness of each item, set so that the total variance of each item is 1 for ease of interpretation 
u1 <- 1-a-b12-b13
u2 <- 1-a-b12-b23
u3 <- 1-a-b13-b23
# Create covariance matrix for factoring
mat.fat <- matrix(c(u1+a+b12+b13,a+b12       ,a+b13,
                    a+b12       ,u2+a+b12+b23,a+b23,
                    a+b13       ,a+b23       ,u3+a+b13+b23), byrow = T,nrow=3)

# simulate replacing a arithmetic item with a geometry item that does not share the arithmetic variation
b13 <- 0.0 # This is zero since item 3 no longer is about arithmetic but geometry in order to
b23 <- 0.0 #    expand the construct
u1 <- 1-a-b12-b13
u2 <- 1-a-b12-b23
u3 <- 1-a-b13-b23

mat.anh <- matrix(c(u1+a+b12+b13,a+b12       ,a+b13,
                    a+b12       ,u2+a+b12+b23,a+b23,
                    a+b13       ,a+b23       ,u3+a+b13+b23), byrow = T,nrow=3)

# the results of the two models shows that replacing a arithmetic item with a geometry item actually increases the uniquenesses in the model,
#   which implies that the measured factor is actually narrower after bringing in items to capture the broader construct then when trying
#   to narrowly measure the construct.
psych::fa(mat.fat,nfactors=1,fm="ml")
psych::fa(mat.anh,nfactors=1,fm="ml")

#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################

# Readers are encouraged to play with the variance decompositions and see how factor models decompose the resulting covariance matrices.
# a is the variance shared by all items
a <- 0.3
# b is the variance shared by each pair of items
b12 <- 0.1
b13 <- 0.0
b23 <- 0.0
# u is the uniqueness of each item, set so that the total variance of each item is 1 for ease of interpretation 
u1 <- 1-a-b12-b13
u2 <- 1-a-b12-b23
u3 <- 1-a-b13-b23
# stop execution if the variance of each items is more than 1, which happens if a and b terms are too high
stopifnot(u1>0 & u2>0 & u3>0) 

mat <- matrix(c(u1+a+b12+b13,a+b12       ,a+b13,
                a+b12       ,u2+a+b12+b23,a+b23,
                a+b13       ,a+b23       ,u3+a+b13+b23), byrow = T,nrow=3)
mat

# Size of the uniquenesses implied by the setup
c(Item1=u1,Item2=u2,Item3=u3)
# Size of the shared variance implied by the setup
c(Item1=a,Item2=a,Item3=a)
# Size of the "misfit" implied by the setup
c(Item1.2=b12,Item1.3=b13,Item2.3=b23)


psych::fa(mat,nfactors=1,fm="ml")

