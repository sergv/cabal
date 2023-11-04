{-# LANGUAGE ImplicitParams #-}

module Distribution.Compat.Stack
  ( WithCallStack
  , CallStack
  , withFrozenCallStack
  , withLexicalCallStack
  , callStack
  , prettyCallStack
  , parentSrcLocPrefix
  ) where

import GHC.Stack

type WithCallStack a = HasCallStack => a


-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.
parentSrcLocPrefix :: WithCallStack String
parentSrcLocPrefix = prettyCallStack callStack

-- Yeah, this uses skivvy implementation details.
withLexicalCallStack :: (a -> WithCallStack (IO b)) -> WithCallStack (a -> IO b)
withLexicalCallStack f =
  let stk = ?callStack
   in \x -> let ?callStack = stk in f x
