module Main where

import Ipseity

main     = ipseity "test.ini"
wrong    = ipseity "wrong.ini"
wrong2   = ipseity "wrong2.ini"
notfound = ipseity "nofile.ini"

