import Control.Monad(filterM)
import System.Environment(getArgs)
import System.Directory(doesDirectoryExist, doesFileExist, getDirectoryContents)

main :: IO [String]
main = do { args <- getArgs
          ; files <- getFiles args
          ; return files
          }

getFiles :: [String] -> IO [String]
getFiles [] = return []
getFiles (n : ns) = prout (doesDirectoryExist n, doesFileExist n) >>= jeez >>= jaaz
    where
        jeez (True, False) = getDirectoryContents n >>= filterM doesFileExist
        jeez (False, True) = return [n]

        jaaz files = getFiles ns >>= \otherfiles ->
            return $ files ++ otherfiles
    
prout :: Monad m => (m a, m b) -> m (a, b)
prout (ma, mb) = ma >>= \a -> mb >>= \b -> return (a, b)

-- import Thrift.Tokenize (tokenize)

--- interact :: (String -> String) -> IO () -> IO String
-- main = interact thrift

-- thrift :: String -> String
-- thrift t =
--     let res = tokenize t
--     in case res of Left err -> show err
--                    Right doc -> show doc
