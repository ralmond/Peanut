## Set up incidence matrix for this graph
## B -> C
## B -> D
## C -> A
## D -> A
tOmeg <- matrix(c(1,0,1,1,
                  0,1,0,0,
                  0,1,1,0,
                  0,1,0,1), 4,4, byrow=TRUE,
                dimnames=list(c("A","B","C","D"),c("A","B","C","D")))
topsort(tOmeg)

