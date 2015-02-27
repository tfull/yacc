module Base where

data Result a = Reject String | Accept a

instance Monad Result where
    Reject s >>= _ = Reject s
    Accept a >>= f = f a
    return = Accept
    fail s = Reject s
