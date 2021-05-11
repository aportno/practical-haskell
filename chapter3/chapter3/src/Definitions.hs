-- parameteric polymorphism

maybeString (Just _) = "Just"
maybeString Nothing = "Nothing"

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Eq, Ord)

-- functions as parameters

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x, y) -> x == y) t -- sample call equalTuples [(1,1) (1,2)] will return [True, False]

