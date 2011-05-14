-- Author: Federico Mastellone (fmaste@gmail.com)

module Main where

import qualified Test.QuickCheck as QC
import Data.BinaryRelation.Quickcheck as BRQC
import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-30s: " s >> a) tests

tests  = [
	("Add to domain, check domain", QC.quickCheck BRQC.prop_addToDomainCheckDomain),
	("Add to codomain, check codomain", QC.quickCheck BRQC.prop_addToCodomainCheckCodomain),
	("Add to domain, check domain count", QC.quickCheck BRQC.prop_addToDomainCount),
	("Add to codomain, check codomain count", QC.quickCheck BRQC.prop_addToCodomainCount),
	("Add to domain, check getCodomain", QC.quickCheck BRQC.prop_addToDomainCheckCodomain),
	("Add to codomain, check getDomain", QC.quickCheck BRQC.prop_addToCodomainCheckDomain),
	("Add to domain, check getCodomainCount", QC.quickCheck BRQC.prop_addToDomainCheckCodomainCount),
	("Add to codomain, check getDomainCount", QC.quickCheck BRQC.prop_addToCodomainCheckDomainCount)]
