-- This fails because
-- sqrt does a rounding because
-- the floating point binary has to
-- be truncated.
--
-- Basically a rounding error

square :: Num a => a
square x = x * x

squareIdentity = square . sqrt
