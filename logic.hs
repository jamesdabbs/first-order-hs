import Prelude hiding (negate)

import Control.Monad (join)
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- A property is simply an ID number for now
-- Eventually, it should have a name and possibly a parametrized value type
data Property = Property Int
  deriving (Eq, Ord, Show)

propId :: Property -> Int
propId (Property n) = n

-- Formulae are built up from atoms recursively
-- TODO: should we use Maybe Bool?
data Formula = Atom Property Bool
             | And  [Formula]
             | Or   [Formula]

-- And should be printed recursively
instance Show Formula where
  show (Atom p True)  = show(propId p)
  show (Atom p False) = "~" ++ show(propId p)
  show (And  fs) = "(" ++ (intercalate " & " . map show $ fs) ++ ")"
  show (Or   fs) = "(" ++ (intercalate " | " . map show $ fs) ++ ")"

-- And negated recursively
negate :: Formula -> Formula
negate (Atom p v) = Atom p $ not v
negate (And   fs) = Or  $ map negate fs
negate (Or    fs) = And $ map negate fs 

-- An implication is a special type of formula
data Implication = Implication Formula Formula

instance Show Implication where
  show (Implication a c) = show a ++ " ⇒ " ++ show c

-- Infix operators for more convenient construction of formulae
(.=) :: Property -> Bool -> Formula
(.=) p v = Atom p v

(.&) :: Formula -> Formula -> Formula
(.&) (And fs) (And gs) = And (fs ++ gs)
(.&) f g = And [f, g]

(.|) :: Formula -> Formula -> Formula
(.|) (Or fs) (Or gs) = Or (fs ++ gs)
(.|) f g = Or [f, g]

(.->) :: Formula -> Formula -> Implication
(.->) = Implication

-- Set precedence so that e.g.
-- compact = True + compact = False -> connected = True
infixr 6 .=
infixr 5 .&
infixr 5 .|
infixr 4 .->

-- A space is largely just its collection of traits, which we model with a
--   Property -> Bool map
type Traits = Map.Map Property Bool
data Space = Space { spaceId :: Int, traits :: Traits }

-- This helper finds the first item in a list different from the default value
firstDiff :: Eq a => a -> [a] -> a
firstDiff v vs = fromMaybe v $ find (/= v) vs

-- Pattern matching makes it fairly easy to check if a space satisfies a given
-- formula
satisfies :: Space -> Formula -> Maybe Bool
satisfies s (Atom p True)  =            Map.lookup p $ traits s
satisfies s (Atom p False) = fmap not . Map.lookup p $ traits s
satisfies s (And fs) = firstDiff (Just True)  $ map (satisfies s) fs
satisfies s (Or  fs) = firstDiff (Just False) $ map (satisfies s) fs

evaluate :: Formula -> Space -> Maybe Bool
evaluate = flip satisfies

contrapositive :: Implication -> Implication
contrapositive (Implication a c) = Implication (negate a) (negate c)

-- And to "update" a Space (modulo actually mutating state)
-- FIXME: implement - check for antecedent Just False and consequent Nothing
apply :: Implication -> Space -> Space
apply i s =
  let apply' (Implication ant cons) s' = s'
  in
    apply' (contrapositive i) . apply' i $ s

-- The default show instance for space is way too verbose. This is better:
instance Show Space where
  show s = "<" ++ show(spaceId s) ++ ": " ++ intercalate ", " atoms ++ ">"
    where
      atoms = map (\(p,v) -> show $ Atom p v) . Map.toList . traits $ s

-- This is a simpler way to construct a space with a list of traits
-- FIXME: it's too simple ... there's no way to specify an unknown value except
--         by truncating the list
spaceFromList spaceId v =
  Space { spaceId = spaceId, traits = traits }
  where
    traits = Map.fromList $ do
      (i, val) <- zip [0..] v
      return $ (Property i, val) 

-- Some simple sample data
compact   = Property 0
connected = Property 1
something = Property 2

interval = spaceFromList 0 [True,  True ]
discrete = spaceFromList 1 [True,  False]
line     = spaceFromList 2 [False, True ]
sorg     = spaceFromList 3 [False, False]
part     = spaceFromList 4 [True        ]

spaces = [interval, discrete, line, sorg, part]

connected_known = (connected .= True) .| (connected .= False)
contradiction   = (connected .= True) .& (connected .= False)

theorem = (compact .= True) .-> (connected .= True)

-- An ad-hoc "global" search method
matching :: Formula -> Maybe Bool -> [Space]
matching f val = filter ((== val) . evaluate f) spaces

satisfying :: Formula -> [Space]
satisfying f = matching f (Just True)
