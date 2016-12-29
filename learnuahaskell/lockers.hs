-- a high-school has lockers so that students have some place to put their
-- Guns'n'Roses posters. Each locker has a code combination. When a student
-- wants a new locker, they tell the locker supervisor which locker number
-- they want and he gives them the code. However, if someone is already using
-- that locker, he can't tell them the code for the locker and they have to
-- pick a different one

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerNumber = Int

type LockerMap = Map.Map LockerNumber (LockerState, Code)

lockerLookup :: LockerNumber -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker number " ++ show lockerNumber ++ " is already taken."

lockers :: LockerMap
lockers = Map.fromList
          [(100,(Taken,"ZD39I"))
          ,(101,(Free,"JAH3I"))
          ,(103,(Free,"IQSA9"))
          ,(105,(Free,"QOTSA"))
          ,(109,(Taken,"893JJ"))
          ,(110,(Taken,"99292"))
          ]
