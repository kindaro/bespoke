{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Prelude qualified
import Prelude.Fancy

import Data.Csv
import Data.ByteString.Lazy qualified as ByteStream
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Either hiding (partitionEithers)
import Data.List qualified as List
import Text.Printf
import Data.Typeable hiding (cast)
import Text.Show.Pretty
import Data.Ord
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Witherable (Filterable)
import Witherable qualified
import Data.Set qualified as Set
import Data.Ratio
import Text.Regex.TDFA

average ∷ Vector Float → Float
average = uncurry (/) ∘ (Vector.sum ▵ fromIntegral ∘ Vector.length)

deviation ∷ Vector Float → Float
deviation vector = (sqrt ∘ average ∘ fmap (**2) ∘ fmap (+ (negate ∘ average) vector)) vector

decodeAsNoodles ∷ FilePath → IO (Header, Vector (HashMap ByteArray Text))
decodeAsNoodles = fmap (fromRight (mempty, mempty) ∘ decodeByName @(HashMap ByteArray Text)) ∘ ByteStream.readFile

decodeAsPasta ∷ FilePath → IO (Vector (Vector Text))
decodeAsPasta = fmap (fromRight mempty ∘ decode @(Vector Text) NoHeader) ∘ ByteStream.readFile

extractCollectionFromTag ∷ Text → Text
extractCollectionFromTag text =
  let matched = (text =~ ("&lt;strong&gt;.*&lt;/strong&gt;" ∷ Text) ∷ Text) =~ ("[A-Z][^&]+" ∷ Text)
  in if Text.null matched then "None" else matched

main ∷ IO ()
main = do
  (headline, someRows) ← decodeAsNoodles "pixel-prestige.csv"
  moreRows ← ((fmap ∘ fmap ∘ fmap) extractCollectionFromTag ∘ decodeAsPasta) "collections.csv"
  let rows = Vector.zipWith HashMap.union someRows (numbered moreRows)
  let numberOfSamples = Vector.length rows
  print numberOfSamples
  print (Vector.head rows)
  let columns = transpose rows
  pPrint headline
  pPrint do fmap typify columns
  let (intVectors, (floatVectors, (textVectors, ( )))) = cast columns
  let numbers = HashMap.union ((fmap ∘ fmap) fromIntegral intVectors) floatVectors
  pPrint do for numbers (showNumber ∘ average ▵ showNumber ∘ deviation)
  pPrint do
    partitionEithers do
      for textVectors \ textVector →
        let
          elements = (Set.fromList ∘ Vector.toList) textVector
          size = Set.size elements
        in if size ≡ 1
           then Left (Vector.head textVector)
           else Right let uniquity = fromIntegral size / (fromIntegral numberOfSamples ∷ Float) in do
            ( size % numberOfSamples, (showNumber uniquity, if uniquity ≤ 0.1 ∧ size ≤ constantMaximumNumberOfCategories then elements else mempty))

constantMaximumNumberOfCategories :: Int
constantMaximumNumberOfCategories = 64

numbered ∷ Vector (Vector α) → Vector (HashMap ByteArray α)
numbered vector = fmap (HashMap.fromList
  ∘ zip indexList
  ∘ Vector.toList) vector
  where indexList = (fmap Text.encodeUtf8 ∘ decorateLexicographically) (replicate ((length ∘ Vector.head) vector) " additional")

transposeVector ∷ Vector (Vector α) → Vector (Vector α)
transposeVector = fmap Vector.fromList ∘ Vector.fromList ∘ List.transpose ∘ Vector.toList ∘ fmap Vector.toList

transposeHashMap ∷ (Hashable key₁, Hashable key₂) ⇒ HashMap key₁ (HashMap key₂ value) → HashMap key₂ (HashMap key₁ value)
transposeHashMap hashMap =
  let keys = HashSet.toList do foldMap HashMap.keysSet hashMap
  in HashMap.fromList (zip keys (for keys \ key → HashMap.mapMaybe (HashMap.lookup key) hashMap))

class Transpose outer inner where transpose ∷ outer (inner α) → inner (outer α)

instance
  (Functor outer, Foldable outer
  , Filterable outer
  , Hashable key
  ) ⇒ Transpose outer (HashMap key)
  where
    transpose container =
      let keys = HashSet.toList do foldMap HashMap.keysSet container
      in HashMap.fromList (zip keys (for keys \ key → Witherable.mapMaybe (HashMap.lookup key) container))

decorateLexicographically ∷ [Text] → [Text]
decorateLexicographically list =
  let
    lengthOfList = length list
    orderOfMagnitude = (ceiling ∘ logBase @Float 10 ∘ fromIntegral) lengthOfList + 1 ∷ Int
    render = Text.pack ∘ printf ("%" ⊕ Prelude.show orderOfMagnitude <> "d")
  in zipWith (⊕) (fmap render (enumFromTo 1 lengthOfList)) list

decorateVectorLexicographically ∷ Vector Text → Vector Text
decorateVectorLexicographically vector =
  let
    lengthOfVector = Vector.length vector
    orderOfMagnitude = (ceiling ∘ logBase @Float 10 ∘ fromIntegral) lengthOfVector + 1 ∷ Int
    render = Text.pack ∘ printf ("%" ⊕ Prelude.show orderOfMagnitude <> "d")
  in Vector.zipWith (⊕) (fmap render (Vector.enumFromTo 1 lengthOfVector)) vector

showNumber ∷ Float → Text
showNumber = Text.pack ∘ printf "%.4f"

data TypeOfColumn = TextColumn | NumberColumn deriving (Eq, Ord, Show)

typifyAs ∷ ∀ (this ∷ ★). Read this => Vector Text → Float
typifyAs values = (fromIntegral ∘ Vector.length) (Vector.mapMaybe (read @this) values) / (fromIntegral ∘ Vector.length) values

nameOfType ∷ ∀ (this ∷ ★). Typeable this ⇒ Text
nameOfType = (show ∘ typeRep) (Proxy @this)

typifyAsNoting ∷ ∀ (this ∷ ★). (Read this, Typeable this) ⇒ Vector Text → Text × Float
typifyAsNoting values = nameOfType @this :× typifyAs @this values

typify ∷ Vector Text → [(Text, Float)]
typify = List.sortOn (Down ∘ snd) ∘ tupleToList ∘ fork (typifyAsNoting @Int) (typifyAsNoting @Float) (const ("Text" ∷ Text, 1.0 ∷ Float))

cast ∷ HashMap ByteArray (Vector Text) → HashMap ByteArray (Vector Int) × HashMap ByteArray (Vector Float) × HashMap ByteArray (Vector Text) × ( )
cast = fork parseInt parseFloat parseText ∘ fmap (fork id typify)
  where
    parseInt = fmap (Witherable.mapMaybe (read @Int) ∘ fst) ∘ Witherable.filter (\ (_, typeOfVector) → (fst ∘ head ∘ fst) typeOfVector ≡ "Int")
    parseFloat = fmap (Witherable.mapMaybe (read @Float) ∘ fst) ∘ Witherable.filter (\ (_, typeOfVector) → (fst ∘ head ∘ fst) typeOfVector ≡ "Float")
    parseText = fmap fst ∘ Witherable.filter (\ (_, typeOfVector) → (fst ∘ head ∘ fst) typeOfVector ≡ "Text")
