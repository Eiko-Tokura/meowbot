module Parser.AST where

-- | Implementing a monadic parser combinator that converts a parsing operation to an abstract syntax tree.
--
-- The following design goals are considered:
--
-- 1. Support Applicative and Monad operations.
--
-- 2. Support <|>
--
-- 3. Optimized tree for code generation.
--


-- Consider the following functions as our building blocks:
--
-- getItem :: m i
--
-- just :: i -> m i
--
-- <|> :: m a -> m a -> m a
--
-- with these we can already implement a lot of stuff, like the following example of what we need to use in practice:
--
-- data Command = Add String | Remove String | List
--
-- parseCommand :: m Command
-- parseCommand =   string "add" *> (Add <$> some getItem)
--              <|> string "remove" *> (Remove <$> some getItem)
--              <|> string "list" *> pure List
