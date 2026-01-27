# Peanut
Parameterized Bayeisan Networks in R

This provides and object oriented layer on top of a CPTtools, to facilitate calculations with Parameterized models for Bayesian networks.  
Peanut is a collection of abstract classes and generic functions defining a protocol, with the intent that the protocol can be implemented 
with different Bayes net engines.  The companion pacakge PNetica provides an implementation using Netica and RNetica.

To install the latest verison of this package on your machine, you can use:
```
install.packages("Peanut",repos=c(ralmond="https://ralmond.r-universe.dev", cran="https://cloud.r-project.org/"))
```


## More Information

Many of the algorithms in the package are documented in 

Almond, Mislevy, Steinberg, Yan and Williamson (1995).  _Bayesian Networks in Educational Assessment._  Springer.

Project information is available at https://pluto.coe.fsu.edu/RNetica/Peanut.html

## Acknowledgements

Work on RNetica, CPTtools and Peanut has been sponsored in part by the
following grants:

* Bill & Melinda Gates Foundation grant "Games as Learning/Assessment:
Stealth Assessment" (#0PP1035331, Val Shute, PI)

* National Science Foundation grant "DIP:
Game-based Assessment and Support of STEM-related Competencies"
(#1628937, Val Shute, PI).

* National Science Foundation grant "Mathematical Learning via
Architectual Design and Modeling Using E-Rebuild." (#1720533,
Fengfeng Ke, PI)

* Intitute of Educational Statistics grant "Exploring Adaptive
  Cognitive and Affective Learning Support for Next-Generation STEM
  Learning Games", (R305A170376,Russell Almond, PI)
  
