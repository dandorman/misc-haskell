lastButOne :: [a] -> a
lastButOne [x, _] = x
lastButOne (_:xs) = lastButOne xs
