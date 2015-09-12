Type Inference for Haskell with Multi-parameter type classes without extensions
=======================================================

Description
----------

Type Inference for Haskell with support to multiparameter type classes 
without functional dependencies or type families. 

Code structure
-------------

    - /src/BuiltIn : contains modules for built-in definitions of types and classes.
    - /src/Iface: contains modules that does the building of a initial
    type environment before the type checking / inference of a given module.
    - /src/Libs: This folder contains several Haskell libraries that have been checked with thisfront-end.        
    - /src/Tc: contains the type inference algorithm
    - /src/Tc/Kc: contains the kind inference algorithm
    - /src/Tests: Unit tests for the implementation. To run all tests,
    just execute the main function in module RunAllTests
    - /src/Utils: Some utilities.

Building the code
---------------
First, you will need Haskell stack tool, which can be downloaded [here](https://github.com/commercialhaskell/stack/wiki/Downloads).
Next, run

	 stack setup
	
 to install the correct GHC version. Now, run

     stack build

to install all Haskell libraries dependencies and compiling the whole project.

Executing the code
-----------------

The best way is to use stack itself to run the code. To run the code
do:

    stack exec -- mptc-exe [OPTIONS] FILE_NAME

where avaliable OPTIONS are:

    --k: Show infered kinds
    --c: Show infered class constraints
    --i: Show infered instance constraints
    --a: Show infered types (include data constructors)
    --r: Turn off recompilation checker
    --make : Enable multi-module processing
    --h: Show this message

    
