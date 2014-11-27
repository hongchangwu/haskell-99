isPalindrome :: Eq x => [x] -> Bool
isPalindrome []     = True
isPalindrome [_]    = True
isPalindrome (x:xs) = x == last xs && (isPalindrome $ init xs)

