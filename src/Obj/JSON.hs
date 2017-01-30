{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Obj.JSON
    ( Vertex (..)
    , Triangle (..)
    ) where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           Obj.Vec3     (Vec3)

data Vertex = Vertex
    { position :: !Vec3
    , normal   :: !Vec3
    } deriving (Generic, ToJSON, Show)

data Triangle = Triangle
    { vertex1 :: !Vertex
    , vertex2 :: !Vertex
    , vertex3 :: !Vertex
    } deriving (Generic, ToJSON, Show)
