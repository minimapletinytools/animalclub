# animalclub

*Animal Club* is still in development. I'm working hard on a first release!!

*Animal Club* (AC) is a general purpose genetic expression library written in Haskell. Each *Animal* is made of *DNA* and several *Genotypes* written in a special purpose composable Monad. 

*DNA* is a *Gene* array--an array of *alleles* pairs. When breeding two *DNAs*, one allele from each parent is randomly chosen and put together to be the new *Gene* in the child's *DNA*.

*Genotype* is a computation on an indexed subset of *DNA*. AC includes several basic computations for either strategically or randomly creating computations.

The *FastGeneBuilder* monad is a composable Writer / State monad for creating *Genotypes*. The State portion contains hierarchical info about what part of the DNA to operate on. The writer portion is where the user writes out computed values with option string headers to dilineate various sections. A typical usage might look something like this:

```haskell
import           AnimalClub.Genetics.FastGeneBuilder
import           AnimalClub.Genetics.Gene
import           AnimalClub.Genetics.Genotype

example = do
    tellGeneName "first"
    gbNormalizedSum >>= tellGene
    gbTypical (0,100) >>= tellGene
    gbNormalizedThresh 0.5 >>= tellGene . (\x -> if x then 1 else 0)
    gbTypical (0,100) >>= tellGene
    tellGeneName "ranges"
    gbRandomRanges [(0,1) | x <-[0..9]] >>= tellGenes
```

AC comes with relevant C bindings. You'll still need to write and compile your animals in Haskell and these can be manipulated from C.