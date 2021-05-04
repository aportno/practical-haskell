firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

concatenateList :: [a] -> [a] -> [a]
concatenateList lst1 lst2 = if (null lst1) {- check emptyness -}
                            then lst2      -- base case
                            else (head lst1) : (tail lst1 ++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if (null lst)
               then []
               else reverse2 (tail lst) ++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin list = let h = head list
              in if null (tail list)
                  then (h, h)
                  else (if h > t_max then h else t_max,
                        if h < t_min then h else t_min)
                        where t = maxmin (tail list)
                              t_max = fst t
                              t_min = snd t

-- Data types

-- algebraic data type (ADT)
-- note there is a nested ADT "Person" that represents two strings
data Client = GovOrg     String
            | Company    String Integer Person
            | Individual Person Bool
            deriving Show

-- ADT that is nested inside ADT Client
-- note all types used inside another one must be showable i.e., have "deriving show"
data Person = Person     String String Gender
            deriving Show

-- ADT that is nested inside ADT client
data Gender = Male | Female | Unknown
            deriving Show

