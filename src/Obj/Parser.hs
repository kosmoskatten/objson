module Obj.Parser
    ( ObjType (..)
    , Face (..)
    , parseFromFile
    ) where

import           Control.Applicative             (empty)
import           Control.Exception               (IOException, catch)
import           Control.Monad                   (void)
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Scientific                 (toRealFloat)
import           Obj.Vec3                        (Vec3 (..))
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as L

data ObjType
    = Vertex !Vec3
    | VertexNormal !Vec3
    | Face !Face
    | PolygonGroup !String
    deriving Show

-- | Only supporting triangle data.
data Face
    = VertexFace !Int !Int !Int -- ^ v1 v2 v3
    | VertexNormalFace !(Int, Int) !(Int, Int) !(Int, Int) -- ^ v1/n1 v2/n2 v3/n3
    deriving Show

parseFromFile :: FilePath -> IO (Either String [ObjType])
parseFromFile file = parseIt `catch` handler
    where
        parseIt :: IO (Either String [ObjType])
        parseIt = do
            content <- LBS.readFile file
            case runParser parseObjData file content of
                Right objData -> return $ Right objData
                Left err      -> return $ Left (show err)

        handler :: IOException -> IO (Either String [ObjType])
        handler = return . Left . show

parseObjData :: Parser [ObjType]
parseObjData = manyTill (sc *> objType) eof
    where
        objType :: Parser ObjType
        objType = (try vertex)
              <|> vertexNormal
              <|> face
              <|> polygonGroup

vertex :: Parser ObjType
vertex =
    v *> (Vertex <$> (Vec3 <$> signedFloat <*> signedFloat <*> signedFloat))

vertexNormal :: Parser ObjType
vertexNormal =
    vn *> (VertexNormal <$> (Vec3 <$> signedFloat <*> signedFloat <*> signedFloat))

polygonGroup :: Parser ObjType
polygonGroup =
    g *> (PolygonGroup <$> getString)

face :: Parser ObjType
face = f *> (Face <$> (try vertexFace <|> try vertexNormalFace))

vertexFace :: Parser Face
vertexFace = VertexFace <$> unsignedInt <*> unsignedInt <*> unsignedInt

vertexNormalFace :: Parser Face
vertexNormalFace = do
    one   <- (,) <$> unsignedInt <*> (string "//" *> unsignedInt)
    two   <- (,) <$> unsignedInt <*> (string "//" *> unsignedInt)
    three <- (,) <$> unsignedInt <*> (string "//" *> unsignedInt)
    return $ VertexNormalFace one two three

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

signedFloat :: Parser Float
signedFloat = toRealFloat <$> (L.signed sc $ lexeme L.scientific)

unsignedInt :: Parser Int
unsignedInt = fromIntegral <$> (lexeme L.integer)

v :: Parser ()
v = void $ L.symbol sc "v"

vn :: Parser ()
vn = void $ L.symbol sc "vn"

g :: Parser ()
g = void $ L.symbol sc "g"

f :: Parser ()
f = void $ L.symbol sc "f"

getString :: Parser String
getString = manyTill alphaNumChar spaceChar
