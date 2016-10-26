# gradualizer
Implementation of the Gradualizer (described in The gradualizer: a methodology and algorithm for generating gradual type systems, M. Cimini and J. G. Siek)

### TODO:
* Modify code to take into account covariance and contravariance of types

### Load module Gradualizer
```haskell
$ cd gradualizer/src/
$ ghci
Prelude> :l Gradualizer.hs
```
### How to generate a gradual type system:
```haskell
*Gradualizer> ts <- generateGradual "stlc"
*Gradualizer> GTS.printSystem ts
```
### How to generate a compiler to the cast calculus:
```haskell
*Gradualizer> ts <- generateCompiler "stlc"
*Gradualizer> CC.printSystem ts
```
