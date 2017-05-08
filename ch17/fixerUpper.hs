-- 1
one = const <$> Just "Hello" <*> Just "World"

-- 2
two = 
  (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
