-- In JavaScript and some other weakly typed languages, you can put almost
-- anything inside an if expression. For example, you can do all of the
-- following: if (0) alert("YEAH!") else alert("NO!"), if ("") alert ("YEAH!")
-- else alert("NO!"), if (false) alert("YEAH") else alert("NO!), etc. and all of
-- these will throw an alert of NO!. If you do if ("WHAT") alert ("YEAH") else
-- alert("NO!"), it will alert a "YEAH!" because JavaScript considers non-empty
-- strings to be a sort of true-ish value.

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

 -- yesnoIf [2,3,4] "YEAH!" "NO!"
 -- yesnoIf (Just 500) "YEAH!" "NO!"
