-- Author: Federico Mastellone (fmaste@gmail.com)

-- Codomain of a binary relation class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Relation.Binary.Codomain (
	Codomain(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Relation.Binary as DCRB

-- CLASS
-------------------------------------------------------------------------------

class DCRB.BinaryRelation r => Codomain r where

	getCodomainList :: r -> [DC.Element (DCRB.CodomainSet r)]

	getCodomainCount :: r -> Integer

	containsCodomainElement :: DC.Element (DCRB.CodomainSet r) -> r -> Bool

	getRelatedFromList :: DC.Element (DCRB.CodomainSet r) -> r -> [DC.Element (DCRB.DomainSet r)]

	getRelatedFromCount :: DC.Element (DCRB.CodomainSet r) -> r -> Integer

	isRelatedFrom :: DC.Element (DCRB.CodomainSet r) ->  DC.Element (DCRB.DomainSet r) -> r -> Bool

