module Type.CompFN (CompFN) where

import Type.Setup
import Type.File

type CompFN = Setup -> FileWPath -> File
