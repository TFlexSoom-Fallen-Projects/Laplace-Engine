{-# LANGUAGE FlexibleInstances #-}
module Dynamic where

data Dynamic =
    BOOL Bool |
    CHAR Char |
    INT Int |
    WORD Word |
    FLOAT Float |
    DOUBLE Double |
    ORDERING Ordering |
    EMPTY_TPL () |
    PAIR_TPL (Dynamic, Dynamic) |
    TRIPLE_TPL (Dynamic, Dynamic, Dynamic) |
    QUAD_TPL (Dynamic, Dynamic, Dynamic, Dynamic) |
    PENT_TPL (Dynamic, Dynamic, Dynamic, Dynamic, Dynamic) |
    SIX_TPL (Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic) |
    ARRAY [Dynamic]

type DynamicFunction = Dynamic -> Dynamic

data DynamicHolder = VALUE Dynamic | FUNC DynamicFunction

class DynamicallyAware a where
    toDyn :: a -> Dynamic
    fromDyn :: Dynamic -> a
    tryFromDyn :: Dynamic -> Maybe a

instance DynamicallyAware Bool where
    toDyn = BOOL

    fromDyn (BOOL v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (BOOL v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Char where
    toDyn = CHAR

    fromDyn (CHAR v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (CHAR v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Int where
    toDyn = INT

    fromDyn (INT v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (INT v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Word where
    toDyn = WORD

    fromDyn (WORD v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (WORD v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Float where
    toDyn = FLOAT

    fromDyn (FLOAT v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (FLOAT v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Double where
    toDyn = DOUBLE

    fromDyn (DOUBLE v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (DOUBLE v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware Ordering where
    toDyn = ORDERING

    fromDyn (ORDERING v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (ORDERING v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware () where
    toDyn = EMPTY_TPL

    fromDyn (EMPTY_TPL v) = v
    fromDyn _ = error "Type Error"

    tryFromDyn (EMPTY_TPL v) = Just v
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware (a, a) where
    toDyn (a, b) = PAIR_TPL (toDyn a, toDyn b)

    fromDyn (PAIR_TPL (a, b)) = (fromDyn a, fromDyn b) 
    fromDyn _ = error "Type Error"

    tryFromDyn (PAIR_TPL (a, b)) = Just (fromDyn a, fromDyn b) 
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware (a, a, a) where
    toDyn (a, b, c) = TRIPLE_TPL (toDyn a, toDyn b, toDyn c)

    fromDyn (TRIPLE_TPL (a, b, c)) = (fromDyn a, fromDyn b, fromDyn c) 
    fromDyn _ = error "Type Error"

    tryFromDyn (TRIPLE_TPL (a, b, c)) = Just (fromDyn a, fromDyn b, fromDyn c) 
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware (a, a, a, a) where
    toDyn (a, b, c, d) = QUAD_TPL (toDyn a, toDyn b, toDyn c, toDyn d)

    fromDyn (QUAD_TPL (a, b, c, d)) = (fromDyn a, fromDyn b, fromDyn c, fromDyn d) 
    fromDyn _ = error "Type Error"

    tryFromDyn (QUAD_TPL (a, b, c, d)) = Just (fromDyn a, fromDyn b, fromDyn c, fromDyn d) 
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware (a, a, a, a, a) where
    toDyn (a, b, c, d, e) = PENT_TPL (toDyn a, toDyn b, toDyn c, toDyn d, toDyn e)

    fromDyn (PENT_TPL (a, b, c, d, e)) = (fromDyn a, fromDyn b, fromDyn c, fromDyn d, fromDyn e) 
    fromDyn _ = error "Type Error"

    tryFromDyn (PENT_TPL (a, b, c, d, e)) = Just (fromDyn a, fromDyn b, fromDyn c, fromDyn d, fromDyn e) 
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware (a, a, a, a, a, a) where
    toDyn (a, b, c, d, e, f) = SIX_TPL (toDyn a, toDyn b, toDyn c, toDyn d, toDyn e, toDyn f)

    fromDyn (SIX_TPL (a, b, c, d, e, f)) = (fromDyn a, fromDyn b, fromDyn c, fromDyn d, fromDyn e, fromDyn f) 
    fromDyn _ = error "Type Error"

    tryFromDyn (SIX_TPL (a, b, c, d, e, f)) = Just (fromDyn a, fromDyn b, fromDyn c, fromDyn d, fromDyn e, fromDyn f) 
    tryFromDyn _ = Nothing

instance DynamicallyAware a => DynamicallyAware [a] where
    toDyn a = ARRAY (map toDyn a) 

    fromDyn (ARRAY a) = map fromDyn a
    fromDyn _ = error "Type Error"

    tryFromDyn (ARRAY a) = Just (map fromDyn a)
    tryFromDyn _ = Nothing



