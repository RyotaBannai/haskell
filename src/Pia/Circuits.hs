module Pia.Circuits where

import Control.Arrow
import Data.List

class ArrowLoop a => ArrowCircuit a where
  delay :: b -> a b b

{- nor := 001 100 010 110 . Not OR -}
-- λ nor (False, False) # True
nor :: Arrow a => a (Bool, Bool) Bool
nor = arr (not . uncurry (||))

{-
By connecting two NOR-gates together, one can build a flip-flop.
A flip-flop takes two inputs, SET and RESET, and produces two outputs, one of which is the negation of the other.
As long as both inputs remain low, the outputs remain stable,
but when the SET input goes high, then the first output does also,
and when the RESET input goes high, then the first output goes low.
If SET and RESET are high simultaneously, then the flip-flop becomes unstable.
A flip-flop is made by connecting the output of each NOR-gate to one input of the other;
the remaining two inputs of the NOR-gates are the inputs of the flip-flop, and their outputs are the outputs of the flip-flop.
-}

flipflop :: ArrowCircuit a => a (Bool, Bool) (Bool, Bool)
flipflop =
  loop
    ( arr (\((reset, set), ~(c, d)) -> ((set, d), (reset, c)))
        >>> nor *** nor -- first f >>> second g
        >>> delay (False, True)
        >>> arr id &&& arr id -- duplicate
    )

class Signal a where
  showSignal :: [a] -> String

instance Signal Bool where
  showSignal bs = concat top ++ "\n" ++ concat bot ++ "\n"
    where
      (top, bot) = unzip (zipWith sh (False : bs) bs)
      sh True True = ("__", "  ")
      sh True False = ("  ", "|_")
      sh False True = (" _", "| ")
      sh False False = ("  ", "__")

instance (Signal a, Signal b) => Signal (a, b) where
  showSignal xys =
    showSignal (map fst xys)
      ++ showSignal (map snd xys)

instance Signal a => Signal [a] where
  showSignal = concat . map showSignal . transpose

sig :: [(Int, a)] -> [a]
sig = concat . map (uncurry replicate)

flipflopInput :: [(Bool, Bool)]
flipflopInput =
  sig
    [ (5, (False, False)),
      (2, (False, True)),
      (5, (False, False)),
      (2, (True, False)),
      (5, (False, False)),
      (2, (True, True)),
      (6, (False, False))
    ]