data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
               Just (a, b, a') -> Node (unfold f a) b (unfold f a')
               _               -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold makeTree 0
  where makeTree x
          | x == n    = Nothing
          | otherwise = Just (x + 1, n, x + 1)
