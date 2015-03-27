module Main where

import Ipseity
import System.IO

main     = ipseity "test.ini"
wrong    = ipseity "wrong.ini"
wrong2   = ipseity "wrong2.ini"
notfound = ipseity "nofile.ini"

