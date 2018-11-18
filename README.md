[![Build Status](https://travis-ci.com/pdlla/animalclub.svg?branch=master)](https://travis-ci.com/pdlla/animalclub)

# animalclub

_Animal Club_ (_AC_) is still in development. I'm working hard on a first release!! Each "animal" is made of _DNA_ and several _Genotypes_ written in a special purpose Monad. Using the _Skellygen_ and _Animal_ module, the animal's _Genome_ can be mapped onto a parameterized mesh generator to create unique 3D meshes for each genetic expression of the animal.

# genetics

The _Animal Club_  _Genetics_ module is a general purpose genetic expression library written in Haskell. _DNA_ is a bit array. When breeding two _DNAs_, 1 bit out of every 2 from each parent is randomly chosen and put together to be the the 2 new bits of child's _DNA_.

_Gene_ is a computation on an indexed subset of _DNA_. The _Genotype_ monad is a Writer / State monad for creating computations on _Genes_. _AC_ includes several basic computations for either strategically or randomly creating these computations. The State portion contains hierarchical info about what part of the _DNA_ (_Genes_) to operate on. The writer portion is where the user writes out computed values with string labels to be read by another program. A typical usage might look something like this:

```haskell
import           AnimalClub.Genetics

-- AnimalClub provides support methods for the writer Monoid [(Text,[Float])]
-- such as tellGene and tellGenes. You are free to use whatever Monoid you want.
example = do
	gbNormalizedSum >>= tellGene "Some Label 1"
    gbNormalizedThresh 0.5 >>= tellGene "Some Label 2" . (\x -> if x then 1 else 0)
    a1 <- gbTypical (0,100)
    a2 <- gbTypical (0,100)
    tellGenes "third" [a1,a2]
    gbRandomRanges [(0,1) | _ <-[(0::Int)..9]] >>= tellGenes "Some Label 3"
```

# skellygen

The _Skellygen_ module contains a special parameterizable mesh generating data structure. It's rather esoteric but works.

# animals

The _Animals_ module bridges _Genetics_ and _Skellygen_. It also contains several helper methods for automatically generating _Genotypes_ by specifying only a high level overview.

# roadmap
Coming soon ~~_AC_ comes with relevant C bindings. You'll still need to write and compile your animals in Haskell and these can be manipulated from C.~~
