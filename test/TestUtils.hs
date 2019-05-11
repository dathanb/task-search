module TestUtils where

-- We're ok with these being impure, since they're only going to get used in a test context
getLeft :: Either a b -> a
getLeft (Left a) = a
getLeft (Right _) = error "Expected Left, got Right"

-- We're ok with these being impure, since they're only going to get used in a test context
getRight :: Either a b -> b
getRight (Left _) = error "Expected Left, got Right"
getRight (Right a) = a


