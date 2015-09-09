{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Utils.ErrMsg (module Utils.ErrMsg, 
                     module Text.PrettyPrint.HughesPJ) where

import Control.Monad(replicateM)
import Control.Monad.Except(throwError)
import Control.Monad.State
import Data.List(intersperse)
import Language.Haskell.Exts

import Text.PrettyPrint.HughesPJ

-- a module to put all error related messages
-- and error related functions

type ErrMsg = Doc

-- a type class for monads that support diagnostics

-- a diagnostic is just a doc

type Diagnostic = Doc

class DiagnosticM s where
    pushDiagnostic :: MonadState s m => Diagnostic -> m ()
    popDiagnostic  :: MonadState s m => m Diagnostic
    size           :: MonadState s m => m Int

-- executes a monadic io / state action within a diagnostic
-- context    
    
withDiagnostic :: (MonadIO m, DiagnosticM s, MonadState s m, Pretty a) => a -> m b -> m b
withDiagnostic e m = do
                        pushDiagnostic (text $ prettyPrint e)
                        x <- m
                        popDiagnostic
                        return x     
                        
-- dumping the context given an error message

dump :: (DiagnosticM s, MonadState s m) => ErrMsg -> Int -> m ErrMsg
dump e depth = do
                l <- size                
                let
                   depth' = if depth > l then l else depth
                es <- replicateM depth' popDiagnostic                         
                return (dumpDiagnostic e es)
                
dumpDiagnostic :: ErrMsg -> [Diagnostic] -> ErrMsg
dumpDiagnostic e [] = e
dumpDiagnostic e ds = e <+> text "that occurs in:" $+$ e'
                      where
                        e' = scat (intersperse s ds)
                        scat = foldr ($+$) empty
                        s  = text "that occurs in:"
                        
differentSizeListUnifyError t t' 
    = launchError msg
      where
        msg = text "Cannot unify two lists of different sizes:" $+$
               (foldr ((<+>) . prettyPrint') empty t)           $+$
              text "with:"                                      $+$
               (foldr ((<+>) . prettyPrint') empty t')                                         

unificationError e1 e2
    = launchError msg
      where
        msg = text "Cannot unify:" $+$
              (prettyPrint' e1)    $+$
              text "with:"         $+$
              (prettyPrint' e2)  <> text "\n" 
                        
occursCheckError v k 
    = launchError msg
      where
        msg = text "Occurs check error! The expression" $+$ 
              (prettyPrint' v) $+$
              text "occurs in" $+$
              (prettyPrint' k) <> text "\n"                       

matchingError x y
    = launchError msg
        where
            msg = text "Cannot match the expression:" $+$
                   (prettyPrint' x)                   $+$
                   text "with"                        $+$
                   (prettyPrint' y)

differentConstraintsError x y
    = launchError msg
        where
            msg = text "Cannot match/unify two different constraints:" $+$
                    (prettyPrint' x)                                   $+$
                  text "with"                                          $+$
                    (prettyPrint' y)
                    
boundVariableInUnifyError v t
    = launchError msg
        where
            msg = text "Trying to unify the bound variable:" $+$
                    (prettyPrint' v)                         $+$
                  text "with the type:"                      $+$
                    (prettyPrint' t)     
                    
noSatInstanceError p
    = launchError msg
        where
            msg = text "There's no instance to satisfy the following constraint:" $+$
                    (prettyPrint' p)              
                    
notDeacreasingError p
    = launchError msg
        where
            msg = text "The constraint:" $+$
                    (prettyPrint' p)     $+$
                  text "isn't satisfiable by a finite deacreasing chain." 
                  
infiniteReductionError p 
    = launchError msg
        where
            msg = text "The constraint:" $+$
                    (prettyPrint' p)     $+$
                  text "isn't reducible by a finite deacreasing chain."                     
                  
notDefinedError x
    = launchError msg
        where
            msg = text "The symbol:"  $+$
                     (prettyPrint' x) $+$
                  text "isn't in scope."     
                                                                                        
noTypeConstructorExpression x
    = launchError msg
        where
            msg = text "The expression:" $+$
                    (prettyPrint' x)     $+$
                  text "isn't a valid type constructor expression."
                  
lastStatementExpressionError x
    = launchError msg
        where
            msg = text "The last statement in a do must be an expression." $+$
                    (prettyPrint' x)             
                    
classNotDefinedError x
    = launchError msg
        where
            msg = text "The type class:" $+$
                    (prettyPrint' x)     $+$
                  text "isn't defined."
                  
              
                  
ambiguousOverloadingError x
    = launchError msg
        where
            msg = text "The constraint:" $+$
                    (prettyPrint' x)     $+$
                  text "is ambiguous (more than one instance could satify it)."    
                  
notWellFormedTypeError t
    = launchError msg
        where
            msg = text "The type:" $+$
                  (prettyPrint' t) $+$
                  text "isn't well formed."    
                  
undefinedClassError xs
    = launchError msg
        where
            nl = text "\n"
            msg = text "The following classes aren't visible or defined:" $+$
                  foldr ($+$) empty (intersperse nl (map prettyPrint' xs))    
                  
undefinedInstanceError xs
    = launchError msg
        where
            msg = foldr ($+$) empty (map f xs)
            f (i,ctx) = text "The instance:" $+$ pprint i $+$ 
                        text "doesnt satisfy the following constraints:" $+$
                        foldr (($+$) . prettyPrint') empty ctx   
                        
undefinedInstances is
    = launchError msg
        where
            msg = text "There's no instances for:" $+$ 
                    foldr ($+$) empty (map prettyPrint' is)   

overlappingInstancesError is
	= launchError msg
		where
			msg = text "There's an overlapping between these instances:" $+$
					foldr ($+$) empty (map (prettyPrint' . snd) is)
                  
kindNotFoundError x = launchError (text ("Cannot find kind for:\n" ++ (prettyPrint x) ++ "\n"))  

typeMostGeneralThanExpectedError inf ann
    = launchError msg
        where
            msg = text "Type most general than expected.\nAnnotated:\n" $+$
                   (pprint ann) $+$ text "Infered:\n" $+$ (pprint inf)      
                                                                                                          


launchError e = dump e 5 >>= throwError . show


-- 

cyclicImportsError xs
    = "The following modules forms a imports cycle:\n" ++
      (foldr ((++) . prettyPrint1) [] xs)
      where
         prettyPrint1 x = (prettyPrint x) ++ "\n"                    


filesDontExistError xs
    = "The following modules can't be found:\n" ++
      (foldr ((++) . prettyPrint1) [] xs)
      where
         prettyPrint1 x = (prettyPrint x) ++ "\n"                    
        

interfacesDontExistError xs
    = "The following modules / interfaces can't be found:\n" ++
      (foldr ((++) . prettyPrint1) [] xs)
      where
         prettyPrint1 x = (prettyPrint x) ++ "\n"

                
-- unsupported features errors

unsupportedDeclMsg :: Pretty a => a -> b
unsupportedDeclMsg x = error ("Panic! Unsupported declaration:\n" ++ (prettyPrint x))

unsupportedExpErrMsg :: Pretty a => a -> b
unsupportedExpErrMsg x = error ("Panic! Unsupported expression:\n" ++ (prettyPrint x))

unsupportedPatErrMsg :: Pretty a => a -> b
unsupportedPatErrMsg x = error ("Panic! Unsupported pattern expression:\n" ++ (prettyPrint x))

unsupportedKindErrMsg :: Pretty a => a -> b
unsupportedKindErrMsg x = error ("Panic! Unsupported kind expression:\n" ++ (prettyPrint x))

-- panic error messages

diagnosticStackPanic :: a
diagnosticStackPanic = error "Panic! Diagnostic Stack!\nPop a empty stack."

emptyLcgList :: a 
emptyLcgList = error "Panic! Lcg List is Empty!"

emptyStackPanic :: String -> a
emptyStackPanic l = error ("Panic! Stack is Empty! At location:\n" ++ l)

typeDeclErrorPanic :: Pretty a => String -> a -> b
typeDeclErrorPanic s x = error ("Panic! Invalid Declaration:\n" ++ (prettyPrint x) ++ "\nat\n" ++ s)

interfaceFileParserError :: String -> a
interfaceFileParserError s = error ("Panic! Invalid Interface file for module:\n" ++ s)

lcgWithoutConstraint :: String -> a
lcgWithoutConstraint s = error ("Panic! Lcg generated without constraint:" ++ s)

-- simple error reporting from parsing

parserErrMsg l s = putStrLn s

-- simple auxiliar functions

prettyPrint' x = text (prettyPrint x)

pprint x = text (show x)
