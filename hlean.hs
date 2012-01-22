
import System.Environment (getArgs)
import System.Exit
import qualified System.IO as IO
import System.Directory (getDirectoryContents, doesDirectoryExist, getCurrentDirectory, doesFileExist, removeFile, renameFile)
import System.FilePath.Posix (combine, normalise)
import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.Types (FileOffset)
import System.Process (runInteractiveCommand)
import Control.Monad (filterM, foldM)
    
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, sortBy)
import Data.Function (on)
    
import System.Console.Readline (readline, readKey, initialize)
    
type DB = M.Map Hash Files
type Hash = B.ByteString
type Files = [B.ByteString]

data Action = Keep
            | KeepRest
            | Delete
            | DeleteRest
            | Skip
            | SkipRest
            | Move FilePath
  deriving (Show)

-- ==========================================================================================
--
--                              USER EDIT HERE...
--
-- hashCommand and hashCommandParse are specific to the intended user hash function of choice.
-- Comment out only one alternative (specific to the system):
--

-- Alternative 1
-- $ sha256 LICENSE                                                                                                                           [ 8:39AM]
-- SHA256 (LICENSE) = 9abf1037bcba4400b53a40305cce721fe61d3de916b9a010693f752054fcaa1e
--
-- hashCommand :: FilePath -> String
-- hashCommand f = "sha256 \"" ++ f ++ "\""
-- hashCommandParse :: B.ByteString -> B.ByteString
-- hashCommandParse h = last $ B.words h

-- Alternative 2
-- $ sha256sum LICENSE
-- 9abf1037bcba4400b53a40305cce721fe61d3de916b9a010693f752054fcaa1e  LICENSE
--
hashCommand :: FilePath -> String
hashCommand f = "sha256sum \"" ++ f ++ "\""
hashCommandParse :: B.ByteString -> B.ByteString
hashCommandParse h = head $ B.words h

-- Alternative 3
-- $  md5sum LICENSE
-- 0d1912d498fa40178fc569e7f395226e  LICENSE
--
-- hashCommand :: FilePath -> String
-- hashCommand f = "md5sum \"" ++ f ++ "\""
-- hashCommandParse :: B.ByteString -> B.ByteString
-- hashCommandParse h = first $ B.words h

--
-- ==========================================================================================



main :: IO ()
main =
    do
       initialize
       IO.hSetBuffering IO.stdin IO.NoBuffering
       IO.hSetBuffering IO.stdout IO.NoBuffering
       args <- getArgs
       cur_path <- getCurrentDirectory
       fs <- foldM (\a b -> do {f <- recurseDir (normalise (combine cur_path b)); return (a ++ f)}) [] args
       fs' <- filterM doesFileExist fs
       let files = nub $ filter ignoreFilters $ map B.pack fs'
       hashes <- mapM (\x -> hashFile x) files
       let db = foldr (\(f,h) -> (addFile h f)) M.empty (zip files hashes)
       interactMenu $ findDuplicates db

makeAction :: FilePath -> Action -> IO ()
makeAction f Keep = return () -- (do nothing)
makeAction f Skip = do print $ "Skipping " ++ f
makeAction f Delete = do {putStrLn $ "Deleting " ++ f ++ " ... "; removeFile f}
makeAction f (Move n) = do {putStrLn $ "Moving " ++ f ++ " ... "; renameFile f n}
makeAction f _ = error "Action not supported."

sortFiles :: [(Hash, Files)] -> IO [(Hash, Files)]
sortFiles v = do s1 <- mapM addS v
                 return $ reverse $ map subS $ sortBy (compare `on` fst2) s1
    where addS :: (Hash, Files) -> IO (FileOffset, Hash, Files)
          addS (h,fs) = do s' <- getFileSize $ B.unpack $ head fs
                           let s = s' * (fromIntegral (length fs) :: FileOffset)
                           return (s, h, fs)
          subS :: (a, b, c) -> (b, c)
          subS (_,h,fs) = (h,fs)
          fst2 :: (a,b,c) -> a
          fst2 (x,_,_) = x

getFileSize :: FilePath -> IO FileOffset
getFileSize f = do s <- getFileStatus f
                   return $ fileSize s

interactMenu :: DB -> IO ()
interactMenu db =
    do putStrLn $ "Total Found " ++ (show (M.size db)) ++ " duplicate sets."
       putStrLn $ take 30 $ repeat '='
       sorted <- sortFiles $ M.assocs db
       if ((length sorted) == 0)
         then return () -- quit
         else do
           putStr $ showUsage
           res <- mapM (uncurry interactSet) sorted
           maybeContinue <- confirmPrompt "Last chance to back out. Are you sure you want to continue?"
           if maybeContinue
             then sequence_ $ map (uncurry makeAction) $ concat res
             else return () -- quit
    where
      interactSet :: Hash -> Files -> IO [(FilePath, Action)]
      interactSet h fs = do
        size <- getFileSize $ B.unpack $ head fs
        putStrLn $ "\nFound " ++ (show (length fs)) ++ " duplicate files (Size: " ++ (show size) ++ "; Hash " ++ (show h) ++ ")."
        sequence_ $ map print fs
        putStrLn ""
        res <- interactChoices h fs []
        return res
      interactChoices :: Hash -> Files -> [(FilePath, Action)] -> IO [(FilePath, Action)]
      interactChoices _ [] acc = return acc
      interactChoices h (f:fs) acc = do
        let file = B.unpack f
        action <- readEvalPrintLoop file
        case action of
          KeepRest -> return $ map (\f -> (B.unpack f, Keep)) (f:fs) ++ acc
          SkipRest -> return $ map (\f -> (B.unpack f, Skip)) (f:fs) ++ acc
          DeleteRest -> return $ map (\f -> (B.unpack f, Delete)) (f:fs) ++ acc
          otherwise -> interactChoices h fs $ (file,action):acc
      readEvalPrintLoop :: FilePath -> IO Action
      readEvalPrintLoop f = do
        maybeChar <- readChar $ f ++ " >> "
        case maybeChar of
          '\EOT' -> exitWith ExitSuccess -- EOF / control-d
          'q'    -> exitWith ExitSuccess
          's'    -> return Skip
          'S'    -> return SkipRest
          ' '    -> return Keep
          '\n'   -> return KeepRest
          'd'    -> return Delete
          'D'    -> return DeleteRest
          'm'    -> do new <- readline "Enter new full path: "
                       case new of
                         Nothing   -> do {putStrLn ""; res <- readEvalPrintLoop f; return res}
                         Just "" -> do {putStrLn ""; res <- readEvalPrintLoop f; return res}
                         Just n    -> return $ Move n
          'h'    -> do putStrLn "User input not valid."
                       putStrLn showUsage
                       res <- readEvalPrintLoop f
                       return res
          _      -> do putStrLn "User input not valid."
                       putStrLn showUsage
                       res <- readEvalPrintLoop f
                       return res

confirmPrompt :: String -> IO Bool
confirmPrompt prompt = do
  c <- readline $ prompt ++ " [y/n] "
  case c of
    Just "y" -> return True
    Just ""  -> confirmPrompt prompt
    _        -> return False
    
addFile :: Hash -> B.ByteString -> DB -> DB
addFile h f = M.insertWith' (++) h [f]
             
hashFile :: B.ByteString -> IO B.ByteString
hashFile file =
    do (_,out,_,_) <- runInteractiveCommand $ hashCommand (B.unpack file)
       h <- B.hGetContents out
       return $ hashCommandParse h

findDuplicates :: DB -> DB
findDuplicates = M.filter ((>1) . length)

ignoreFilters :: B.ByteString -> Bool
ignoreFilters f = not $ (B.isInfixOf (B.pack "/_darcs/") f) || (B.isInfixOf (B.pack "/.git/") f)

showUsage :: String
showUsage = unlines
              [ "h       -> Print Usage Help"
              , "q       -> Exit"
              , "(space) -> Keep"
              , "(enter) -> Keep (this one and all remaining duplicates)"
              , "d       -> Delete"
              , "D       -> Delete (this one and all remaining duplicates)"
              , "s       -> Skip"
              , "S       -> Skip (this one and all remaining duplicates)"
              , "m       -> Move/Rename"]

recurseDir :: FilePath -> IO [FilePath]
recurseDir f =
    do
      is_d <- doesDirectoryExist f
      if is_d
         then do files <- getDirectoryContents f 
                 subdirs <- mapM recurseDir $ map (combine f) $ filter notDotOrDotDot files
                 return $ foldl (++) [f] subdirs 
         else return [f]
    where
      notDotOrDotDot :: FilePath -> Bool
      notDotOrDotDot f = case f of
                           "."  -> False
                           ".." -> False
                           _    -> True

readChar :: String -> IO Char
readChar prompt = do
  putStr prompt
  k <- readKey
  putStrLn ""
  return k
                                 
