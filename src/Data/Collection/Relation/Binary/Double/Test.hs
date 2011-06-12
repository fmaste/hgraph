-- Author: Federico Mastellone (fmaste@gmail.com)

module Main where

import qualified Test.QuickCheck as QC
import Data.Collection.Relation.Binary.Double.Quickcheck as BRQC
import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-30s: " s >> a) tests	
	
tests  = [
	("Add to domain, check domain", QC.quickCheck BRQC.prop_addToDomainCheckDomain),
	("Add to codomain, check codomain", QC.quickCheck BRQC.prop_addToCodomainCheckCodomain),
	("Add to domain, check domain count", QC.quickCheck BRQC.prop_addToDomainCheckDomainCount),
	("Add to codomain, check codomain count", QC.quickCheck BRQC.prop_addToCodomainCheckCodomainCount),
	("Add to domain, check codomain", QC.quickCheck BRQC.prop_addToDomainCheckCodomain),
	("Add to codomain, check domain", QC.quickCheck BRQC.prop_addToCodomainCheckDomain),
	("Add to domain, check codomain count", QC.quickCheck BRQC.prop_addToDomainCheckCodomainCount),
	("Add to codomain, check domain count", QC.quickCheck BRQC.prop_addToCodomainCheckDomainCount),
	("Add to domain, check relations", QC.quickCheck BRQC.prop_addToDomainCheckRelations),
	("Add to codomain, check relations", QC.quickCheck BRQC.prop_addToCodomainCheckRelations)]
