module Main where

import Control.Arrow
import Prelude hiding (id,(.))


main :: IO ()
main = do
        let
        -- arr :: (Arrow a) => (b -> c) -> a b c
        -- returnA :: Arrow a => a b b
        -- (>>>) :: Category cat => cat a b -> cat b c -> cat a c
        -- (<+>) :: ArrowPlus a => a b c -> a b c -> a b c
        -- first  :: y a b -> y (a, c) (b, c)          -- maps over first component
        -- second :: y a b -> y (c, a) (c, b)          -- maps over second component
        -- (***)  :: y a c -> y b d -> y (a, b) (c, d) -- first and second combined
        -- (&&&)  :: y a b -> y a c -> y a (b, c)      -- (***) on a duplicated value
        -- runKleisli :: Kleisli m a b -> a -> m b

        -- | Precomposition with a pure function.
        -- (^>>) :: Arrow a => (b -> c) -> a c d -> a b d
        -- f ^>> a = arr f >>> a

        -- | Postcomposition with a pure function.
        -- (>>^) :: Arrow a => a b c -> (c -> d) -> a b d
        -- a >>^ f = a >>> arr f

        -- | Precomposition with a pure function (right-to-left variant).
        -- (<<^) :: Arrow a => a c d -> (b -> c) -> a b d
        -- a <<^ f = a <<< arr f

        -- | Postcomposition with a pure function (right-to-left variant).
        -- (^<<) :: Arrow a => (c -> d) -> a b c -> a b d
        -- f ^<< a = arr f <<< a

        -- infixr 5 <+>
        -- infixr 3 ***
        -- infixr 2 +++
        -- infixr 2 |||
        -- infixr 1 >>>
        -- infixr 1 ^>>, >>^
        -- infixr 1 ^<<, <<^

          prepend x = arr (x ++)
          append  x = arr (++ x)
--           join = arr(\x -> fst head x)
--           join2 = arr(\x -> fst x)
--           join3 = arr(\x -> head x)

          xf1 =  (prepend "<") <+> (append ">")
          xf2 =  (prepend "<") >>> (append ">")  <+>  (prepend "{") >>> (append "}")
          xf3 = ((prepend "<") >>> (append ">")) <+> ((prepend "{") >>> (append "}"))
--           xf4 = ( ((prepend "<") >>> (append ">")) <+> ((prepend "{") >>> (append "}")) ) >>> join3

          -- Expected type: Kleisli [] [Char] [Char]
          -- Actual type: Kleisli [] ([Char], [Char]) ([Char], [Char])

        mapM_ (\xform -> print (["foobar"] >>= runKleisli xform) >> putStrLn "--------------")
              [ xf1
                , xf2
                , xf3
--                 , xf4
              ]
