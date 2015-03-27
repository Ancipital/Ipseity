module Main where

import Ipseity
import System.IO

main     = ipseity "test.ini"
wrong    = ipseity "wrong.ini"
notfound = ipseity "nofile.ini"

