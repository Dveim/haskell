import Text.Regex.Posix
for_both a p1 p2 = (a =~ p1 :: Bool) && (a =~ p2 :: Bool) -- "new" task ???