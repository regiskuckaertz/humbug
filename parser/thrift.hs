import Thrift.Tokenize (tokenize)

--- interact :: (String -> String) -> IO () -> IO String
main = interact thrift

thrift :: String -> String
thrift t =
    let res = tokenize t
    in case res of Left err -> show err
                   Right doc -> show doc
