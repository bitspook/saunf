module Shared where

import Data.Text
import Text.Pandoc as P

readOrg' :: Text -> IO Pandoc
readOrg' str = P.handleError =<< P.runIO (readOrg def str)
