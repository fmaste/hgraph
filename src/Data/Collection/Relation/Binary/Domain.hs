-- Author: Federico Mastellone (fmaste@gmail.com)

-- Domain of a binary relation class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Relation.Binary.Domain (
	Domain(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Relation.Binary as DCRB

-- CLASS
-------------------------------------------------------------------------------

class DCRB.BinaryRelation r => Domain r where

	getDomainList :: r -> [DC.Element (DCRB.DomainSet r)]

	getDomainCount :: r -> Integer

	containsDomainElement ::  DC.Element (DCRB.DomainSet r) -> r -> Bool

	getRelatedToList :: DC.Element (DCRB.DomainSet r) -> r -> [DC.Element (DCRB.CodomainSet r)]

	getRelatedToCount ::  DC.Element (DCRB.DomainSet r) -> r -> Integer

	isRelatedTo ::  DC.Element (DCRB.DomainSet r) -> DC.Element (DCRB.CodomainSet r) -> r -> Bool

