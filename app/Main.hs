module Main where

import Data.Hashable (hash)
import Data.List (isPrefixOf)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeFileName)
import Control.Monad (forM_, when)
import Numeric (showHex)
import qualified Data.ByteString as B (readFile)
import Data.Word (Word32)

main :: IO ()
main = do
    str <- getCurrentDirectory
    isExist <- doesDirectoryExist str
    if isExist
        then do
            files <- listDirectory str
            let fullPaths :: [String]
                fullPaths = map (str </>) files
            forM_ fullPaths $ \ fullPath -> do
                isFileExist <- doesFileExist fullPath
                let fileName = takeFileName fullPath
                when (isFileExist && not ("check-hash" `isPrefixOf` fileName)) $ do
                    bstr <- B.readFile fullPath
                    putStr $ fileName ++ " : "
                    let n :: Word32
                        n = fromIntegral $ hash bstr
                    putStrLn $ padZero 8 $ showHex n ""
        else do
            putStrLn "Wrong Path!"
    _ <- getLine
    return ()

padZero :: Int -> String -> String
padZero n str | m < 0 = str
              | otherwise = replicate m '0' ++ str
 where m = n - length str





    
-- 標準入力 -> ディレクトリのPATH指定


