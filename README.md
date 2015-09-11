MPTC
====

1 - Description

Type Inference for Haskell with support to multiparameter type classes 
without functional dependencies or type families.

2 - Code structure

/src/BuiltIn : contains modules for built-in definitions of types and classes.
/src/Iface: contains modules that does the building of a initial type environment before the
            type checking / inference of a given module.
/src/Libs: This folder contains several Haskell libraries that have been checked with this
           front-end.             
/src/Tc: contains the type inference algorithm
/src/Tc/Kc: contains the kind inference algorithm
/src/Tests: Unit tests for the implementation. To run all tests, just execute the main function in module
            RunAllTests
/src/Utils: Some utilities.

3  - Building code

First, you will need Haskell stack:


