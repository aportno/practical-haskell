firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

-- concatenateList :: [[Char]] -> [[Char]] -> [Char]
concatenateList lst1 lst2 = if (null lst1) then lst2 else (head lst1) : (tail lst1 ++ lst2)

-- reverse2 :: [Char] -> [Char]
reverse2 lst = if (null lst) then [] else reverse2 (tail lst) ++ [head lst]