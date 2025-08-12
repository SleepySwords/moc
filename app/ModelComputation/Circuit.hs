{-# LANGUAGE KindSignatures #-}

module ModelComputation.Circuit where

import Control.Lens ((&))
import Data.Set (Set, fromList)

newtype InputPins = InputPins [Bool] deriving (Ord, Eq, Show)

newtype OutputPins = OutputPins [Bool] deriving (Ord, Eq, Show)

data Gate
  = Gate
      { inputPins :: InputPins,
        outputPins :: OutputPins,
        connections :: Set Connection,
        components :: Set Component
      }
  | Nand
      { nandInputPin :: (Bool, Bool),
        nandOutputPin :: Bool
      }
  deriving (Ord, Eq, Show)

data Component = Component String Gate deriving (Ord, Eq, Show)

data ConnectionType = MainGate | InnerComponent String deriving (Ord, Eq, Show)

data Pin = Pin ConnectionType Integer deriving (Ord, Eq, Show)

data Connection = Connection
  { inputPin :: Pin,
    outputPin :: Pin
  }
  deriving (Ord, Eq, Show)

modify :: (Pin, Bool) -> Gate -> Gate
modify (Pin MainGate 0, v) Nand {nandInputPin = (_, s)} = Nand {nandOutputPin = not s || not v, nandInputPin = (v, s)}
modify (Pin MainGate 1, v) Nand {nandInputPin = (f, _)} = Nand {nandOutputPin = not f || not v, nandInputPin = (f, v)}
modify _ Nand {} = error "Pin out of bounds"

modify (Pin gate pinNo, value) Gate {} = Gate {}

-- >>> test
-- Nand {nandInputPin = (False,True), nandOutputPin = True}

test :: Gate
test = defaultNand & modify (Pin MainGate 0, True) & modify (Pin MainGate 1, True) & modify (Pin MainGate 0, False)

defaultNand :: Gate
defaultNand =
  Nand
    { nandInputPin = (False, False),
      nandOutputPin = True
    }

-- This can actually be a monad?!
bLatch :: Gate
bLatch =
  Gate
    { inputPins = InputPins [False, False],
      outputPins = OutputPins [False, False],
      components = fromList [Component "c1" defaultNand, Component "c2" defaultNand],
      connections =
        fromList
          [ Connection
              { inputPin = Pin MainGate 0,
                outputPin = Pin (InnerComponent "c1") 0
              },
            Connection
              { inputPin = Pin (InnerComponent "c1") 0,
                outputPin = Pin MainGate 0
              },
            Connection
              { inputPin = Pin MainGate 1,
                outputPin = Pin (InnerComponent "c2") 1
              },
            Connection
              { inputPin = Pin (InnerComponent "c2") 0,
                outputPin = Pin MainGate 1
              },
            Connection
              { inputPin = Pin (InnerComponent "c1") 0,
                outputPin = Pin (InnerComponent "c2") 0
              },
            Connection
              { inputPin = Pin (InnerComponent "c2") 0,
                outputPin = Pin (InnerComponent "c1") 1
              }
          ]
    }
