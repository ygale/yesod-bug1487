{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module I18N
    ( mkMessage
    ) where

import Language.Haskell.TH.Syntax
import Control.Applicative ((<$>))
import Control.Monad (filterM, forM)
import Data.Text (Text, pack, unpack)
import System.Directory
import Data.Maybe (catMaybes)
import Data.List (isSuffixOf, sortBy, foldl')
import qualified Data.Map as Map
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isSpace, toLower, toUpper)
import Data.Ord (comparing)
import Base (Deref (..), Ident (..), parseHash, derefToExp)
import Text.ParserCombinators.Parsec (parse, many, eof, many1, noneOf, (<|>))
import Control.Arrow ((***))
import Data.Monoid (mempty, mappend)
import qualified Data.Text as T
import Data.String (IsString (fromString))

-- | 'ToMessage' is used to convert the value inside #{ } to 'Text'
--
-- The primary purpose of this class is to allow the value in #{ } to
-- be a 'String' or 'Text' rather than forcing it to always be 'Text'.
class ToMessage a where
    toMessage :: a -> Text
instance ToMessage Text where
    toMessage = id
instance ToMessage String where
    toMessage = Data.Text.pack

-- | the 'RenderMessage' is used to provide translations for a message types
--
-- The 'master' argument exists so that it is possible to provide more
-- than one set of translations for a 'message' type. This is useful
-- if a library provides a default set of translations, but the user
-- of the library wants to provide a different set of translations.
class RenderMessage master message where
    renderMessage :: master  -- ^ type that specifies which set of translations to use
                  -> [Lang]  -- ^ acceptable languages in descending order of preference
                  -> message -- ^ message to translate
                  -> Text

instance RenderMessage master Text where
    renderMessage _ _ = id

-- | an RFC1766 / ISO 639-1 language code (eg, @fr@, @en-GB@, etc).
type Lang = Text

-- |generate translations from translation files
--
-- This function will:
--
--  1. look in the supplied subdirectory for files ending in @.msg@
--
--  2. generate a type based on the constructors found
--
--  3. create a 'RenderMessage' instance
--
mkMessage :: String   -- ^ base name to use for translation type
          -> FilePath -- ^ subdirectory which contains the translation files
          -> Lang     -- ^ default translation language
          -> Q [Dec]
mkMessage dt folder lang =
    mkMessageCommon True "Msg" "Message" dt dt folder lang

-- |used by 'mkMessage' and 'mkMessageFor' to generate a 'RenderMessage' and possibly a message data type
mkMessageCommon :: Bool      -- ^ generate a new datatype from the constructors found in the .msg files
                -> String    -- ^ string to append to constructor names
                -> String    -- ^ string to append to datatype name
                -> String    -- ^ base name of master datatype
                -> String    -- ^ base name of translation datatype
                -> FilePath  -- ^ path to translation folder
                -> Lang      -- ^ default lang
                -> Q [Dec]
mkMessageCommon genType prefix postfix master dt folder lang = do
    files <- qRunIO $ getDirectoryContents folder
    let files' = filter (`notElem` [".", ".."]) files
    (_files', contents) <- qRunIO $ fmap (unzip . catMaybes) $ mapM (loadLang folder) files'
    mapM_ qAddDependentFile $ concat _files'
    let contents' = Map.toList $ Map.fromListWith (++) contents
    sdef <-
        case lookup lang contents' of
            Nothing -> error $ "Did not find main language file: " ++ unpack lang
            Just def -> toSDefs def
    mapM_ (checkDef sdef) $ map snd contents'
    let mname = mkName $ dt ++ postfix
    c1 <- fmap concat $ mapM (toClauses prefix dt) contents'
    c2 <- mapM (sToClause prefix dt) sdef
    c3 <- defClause
    return $
     ( if genType
       then ((DataD [] mname []
                    Nothing
                    (map (toCon dt) sdef) []) :)
       else id)
        [ instanceD
            []
            (ConT ''RenderMessage `AppT` (ConT $ mkName master) `AppT` ConT mname)
            [ FunD 'renderMessage $ c1 ++ c2 ++ [c3]
            ]
        ]

toClauses :: String -> String -> (Lang, [Def]) -> Q [Clause]
toClauses prefix dt (lang, defs) =
    mapM go defs
  where
    go def = do
        a <- newName "lang"
        (pat, bod) <- mkBody dt (prefix ++ constr def) (map fst $ vars def) (content def)
        guard <- fmap NormalG [|$(return $ VarE a) == pack $(lift $ unpack lang)|]
        return $ Clause
            [WildP, ConP (mkName ":") [VarP a, WildP], pat]
            (GuardedB [(guard, bod)])
            []

mkBody :: String -- ^ datatype
       -> String -- ^ constructor
       -> [String] -- ^ variable names
       -> [Content]
       -> Q (Pat, Exp)
mkBody dt cs vs ct = do
    vp <- mapM go vs
    let pat = RecP (mkName cs) (map (varName dt *** VarP) vp)
    let ct' = map (fixVars vp) ct
    pack' <- [|Data.Text.pack|]
    tomsg <- [|toMessage|]
    let ct'' = map (toH pack' tomsg) ct'
    mapp <- [|mappend|]
    let app a b = InfixE (Just a) mapp (Just b)
    e <-
        case ct'' of
            [] -> [|mempty|]
            [x] -> return x
            (x:xs) -> return $ foldl' app x xs
    return (pat, e)
  where
    toH pack' _ (Raw s) = pack' `AppE` SigE (LitE (StringL s)) (ConT ''String)
    toH _ tomsg (Var d) = tomsg `AppE` derefToExp [] d
    go x = do
        let y = mkName $ '_' : x
        return (x, y)
    fixVars vp (Var d) = Var $ fixDeref vp d
    fixVars _ (Raw s) = Raw s
    fixDeref vp (DerefIdent (Ident i)) = DerefIdent $ Ident $ fixIdent vp i
    fixDeref vp (DerefBranch a b) = DerefBranch (fixDeref vp a) (fixDeref vp b)
    fixDeref _ d = d
    fixIdent vp i =
        case lookup i vp of
            Nothing -> i
            Just y -> nameBase y

sToClause :: String -> String -> SDef -> Q Clause
sToClause prefix dt sdef = do
    (pat, bod) <- mkBody dt (prefix ++ sconstr sdef) (map fst $ svars sdef) (scontent sdef)
    return $ Clause
        [WildP, ConP (mkName "[]") [], pat]
        (NormalB bod)
        []

defClause :: Q Clause
defClause = do
    a <- newName "sub"
    c <- newName "langs"
    d <- newName "msg"
    rm <- [|renderMessage|]
    return $ Clause
        [VarP a, ConP (mkName ":") [WildP, VarP c], VarP d]
        (NormalB $ rm `AppE` VarE a `AppE` VarE c `AppE` VarE d)
        []

toCon :: String -> SDef -> Con
toCon dt (SDef c vs _) =
    RecC (mkName $ "Msg" ++ c) $ map go vs
  where
    go (n, t) = (varName dt n, notStrict, ConT $ mkName t)

varName :: String -> String -> Name
varName a y =
    mkName $ concat [lower a, "Message", upper y]
  where
    lower (x:xs) = toLower x : xs
    lower [] = []
    upper (x:xs) = toUpper x : xs
    upper [] = []

checkDef :: [SDef] -> [Def] -> Q ()
checkDef x y =
    go (sortBy (comparing sconstr) x) (sortBy (comparing constr) y)
  where
    go _ [] = return ()
    go [] (b:_) = error $ "Extra message constructor: " ++ constr b
    go (a:as) (b:bs)
        | sconstr a < constr b = go as (b:bs)
        | sconstr a > constr b = error $ "Extra message constructor: " ++ constr b
        | otherwise = do
            go' (svars a) (vars b)
            go as bs
    go' ((an, at):as) ((bn, mbt):bs)
        | an /= bn = error "Mismatched variable names"
        | otherwise =
            case mbt of
                Nothing -> go' as bs
                Just bt
                    | at == bt -> go' as bs
                    | otherwise -> error "Mismatched variable types"
    go' [] [] = return ()
    go' _ _ = error "Mistmached variable count"

toSDefs :: [Def] -> Q [SDef]
toSDefs = mapM toSDef

toSDef :: Def -> Q SDef
toSDef d = do
    vars' <- mapM go $ vars d
    return $ SDef (constr d) vars' (content d)
  where
    go (a, Just b) = return (a, b)
    go (a, Nothing) = error $ "Main language missing type for " ++ show (constr d, a)

data SDef = SDef
    { sconstr :: String
    , svars :: [(String, String)]
    , scontent :: [Content]
    }

data Def = Def
    { constr :: String
    , vars :: [(String, Maybe String)]
    , content :: [Content]
    }

(</>) :: FilePath -> FilePath -> FilePath
path </> file = path ++ '/' : file

loadLang :: FilePath -> FilePath -> IO (Maybe ([FilePath], (Lang, [Def])))
loadLang folder file = do
    let file' = folder </> file
    isFile <- doesFileExist file'
    if isFile && ".msg" `isSuffixOf` file
        then do
            let lang = pack $ reverse $ drop 4 $ reverse file
            defs <- loadLangFile file'
            return $ Just ([file'], (lang, defs))
        else do
            isDir <- doesDirectoryExist file'
            if isDir
                then do
                    let lang = pack file
                    (files, defs) <- unzip <$> loadLangDir file'
                    return $ Just (files, (lang, concat defs))
                else
                    return Nothing

loadLangDir :: FilePath -> IO [(FilePath, [Def])]
loadLangDir folder = do
    paths <- map (folder </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents folder
    files <- filterM doesFileExist paths
    dirs  <- filterM doesDirectoryExist paths
    langFiles <-
        forM files $ \file -> do
            if ".msg" `isSuffixOf` file
                then do
                  defs <- loadLangFile file
                  return $ Just (file, defs)
                else do
                  return Nothing
    langDirs <- mapM loadLangDir dirs
    return $ catMaybes langFiles ++ concat langDirs

loadLangFile :: FilePath -> IO [Def]
loadLangFile file = do
    bs <- S.readFile file
    let s = unpack $ decodeUtf8 bs
    defs <- fmap catMaybes $ mapM (parseDef . T.unpack . T.strip . T.pack) $ lines s
    return defs

parseDef :: String -> IO (Maybe Def)
parseDef "" = return Nothing
parseDef ('#':_) = return Nothing
parseDef s =
    case end of
        ':':end' -> do
            content' <- fmap compress $ parseContent $ dropWhile isSpace end'
            case words begin of
                [] -> error $ "Missing constructor: " ++ s
                (w:ws) -> return $ Just Def
                            { constr = w
                            , vars = map parseVar ws
                            , content = content'
                            }
        _ -> error $ "Missing colon: " ++ s
  where
    (begin, end) = break (== ':') s

data Content = Var Deref | Raw String

compress :: [Content] -> [Content]
compress [] = []
compress (Raw a:Raw b:rest) = compress $ Raw (a ++ b) : rest
compress (x:y) = x : compress y

parseContent :: String -> IO [Content]
parseContent s =
    either (error . show) return $ parse go s s
  where
    go = do
        x <- many go'
        eof
        return x
    go' = (Raw `fmap` many1 (noneOf "#")) <|> (fmap (either Raw Var) parseHash)

parseVar :: String -> (String, Maybe String)
parseVar s =
    case break (== '@') s of
        (x, '@':y) -> (x, Just y)
        _ -> (s, Nothing)

data SomeMessage master = forall msg. RenderMessage master msg => SomeMessage msg

instance IsString (SomeMessage master) where
    fromString = SomeMessage . T.pack

instance master ~ master' => RenderMessage master (SomeMessage master') where
    renderMessage a b (SomeMessage msg) = renderMessage a b msg

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing
