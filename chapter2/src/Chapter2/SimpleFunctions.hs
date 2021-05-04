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
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

-- ADT that is nested inside ADT Client
-- note all types used inside another one must be showable i.e., have "deriving show"
data Person = Person     String String Gender
            deriving Show

-- ADT that is nested inside ADT client
data Gender = Male | Female | Unknown
            deriving Show

-- pattern matching
-- sample call clientName1 (Individual (Person "Alex" "Portno" Male) True)
clientName1 :: Client -> String
clientName1 client = case client of
                      GovOrg name                 -> name {- GovOrg constructor; name is the binding variable -}
                      Company name id person resp -> name {- Company constructor; name id person resp are all binding variables -}
                      Individual person ads       ->      {- Individual constructor; person and ads are the binding variables -}
                          case person of
                              Person firstName lastName gender -> firstName ++ " " ++ lastName

clientName2 :: Client -> String
clientName2 client = case client of
                        GovOrg name        -> name
                        Company name _ _ _ -> name
                        Individual (Person firstName lastName _ ) _ -> firstName ++ " " ++ lastName

companyName :: Client -> Maybe String
companyName client = case client of
                        Company name _ _ _ -> Just name
                        _                  -> Nothing

sorted :: [Integer] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x : r@(y:_)) = x < y && sorted r


