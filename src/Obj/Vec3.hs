{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Obj.Vec3
    ( Vec3 (..)
    ) where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)

data Vec3 = Vec3
    { x :: !Float
    , y :: !Float
    , z :: !Float
    } deriving (Generic, ToJSON, Show)
