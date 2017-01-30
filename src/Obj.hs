module Obj
    ( toJSON
    ) where

import           Data.Vector (Vector, fromList, (!))
import           Obj.JSON    (Triangle (..), Vertex (..))
import           Obj.Parser  (Face (..), ObjType (..), parseFromFile)
import           Obj.Vec3    (Vec3)

toJSON :: FilePath -> IO (Either String [Triangle])
toJSON file = do
    parseResult <- parseFromFile file
    case parseResult of
        Right objs -> return $ makeTriangles objs
        Left err   -> return $ Left err

makeTriangles :: [ObjType] -> (Either String [Triangle])
makeTriangles objs =
    let vertices = fromList $ grep grepVertex objs
        normals  = fromList $ grep grepNormal objs
        faces    = grep grepFace objs
        groups   = grep grepGroup objs
    in
        if length groups > 2 then
            Left "Too many polygon groups. Sorry :-("
        else
            Right $ map (makeTriangle vertices normals) faces

makeTriangle :: Vector Vec3 -> Vector Vec3 -> Face -> Triangle
makeTriangle vertices normals (VertexFace v1 v2 v3) = undefined
makeTriangle vertices normals (VertexNormalFace (v1, n1) (v2, n2) (v3, n3)) =
    Triangle
        { vertex1 = Vertex { position = vertices ! ind v1, normal = normals ! ind n1 }
        , vertex2 = Vertex { position = vertices ! ind v2, normal = normals ! ind n2 }
        , vertex3 = Vertex { position = vertices ! ind v3, normal = normals ! ind n3 }
        }

ind :: Int -> Int
ind n = n - 1

grepVertex :: ObjType -> Maybe Vec3
grepVertex (VertexCoord v) = Just v
grepVertex _               = Nothing

grepNormal :: ObjType -> Maybe Vec3
grepNormal (VertexNormal vn) = Just vn
grepNormal _                 = Nothing

grepFace :: ObjType -> Maybe Face
grepFace (Face f) = Just f
grepFace _        = Nothing

grepGroup :: ObjType -> Maybe String
grepGroup (PolygonGroup g) = Just g
grepGroup _                = Nothing

grep :: (a -> Maybe b) -> [a] -> [b]
grep g = go []
    where
        go out []     = reverse out
        go out (x:xs) = maybe (go out xs) (\y -> go (y:out) xs) (g x)
