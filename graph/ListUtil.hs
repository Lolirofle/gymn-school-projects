module ListUtil where

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []           = []
filterFirst eq (head:tail) = if eq head then tail else head : filterFirst eq tail

addUnique :: Eq a => a -> [a] -> [a]
addUnique x []               = [x]
addUnique x list@(head:tail) = if head == x then list else head:addUnique x tail

addUniqueOf :: (a -> a -> Bool) -> a -> [a] -> [a]
addUniqueOf _  x []               = [x]
addUniqueOf eq x list@(head:tail) = if eq x head then list else head:addUniqueOf eq x tail
