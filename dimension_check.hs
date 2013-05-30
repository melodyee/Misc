type One a = a -> a
newtype Two a = Two a
newtype Three a = Three a
plus' = undefined :: (a,b) -> (a,b) -> (a,b)
dot' = undefined :: (a,b) -> (b,c) -> (a,c)
m12' = undefined :: (One a, Two a)
m23' = undefined :: (Two a, Three a)

plus = undefined :: (a->b) -> (a->b) -> (a->b)
dot = undefined :: (a->b) -> (b->c) -> (a->c)
-- dot a b = \x -> b (a x)
tr = undefined :: (a -> b) -> (b -> a)
m12 = undefined :: Two a -> One a
m21 = undefined :: One a -> Two a
m23 = undefined :: Three a -> Two a
--v2 = undefined :: Two a
v2 = undefined :: One a -> Two a
rv2 = undefined :: Two a -> One a
{--
*Main> :t (dot m23 m12)
(dot m23 m12) :: Three a -> One a

*Main> :t (dot tr tr)
(dot tr tr) :: (a -> b) -> a -> b
--}

data Zero
 
data Succ a
 
class Add a b ab | a b -> ab, a ab -> b
instance Add Zero b b
instance (Add a b ab) => Add (Succ a) b (Succ ab)
