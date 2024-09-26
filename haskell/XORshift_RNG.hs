-- Pseudo-random Int generator, it uses xorshift as generator 
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
type Rng32 = Word32

xorshift32 :: Rng32 -> Rng32
xorshift32 a = d where
    b = a `xor` (a `shiftL` 13)
    c = b `xor` (b `shiftR` 17)
    d = c `xor` (c `shiftL` 5)

randInt :: (Int, Int) -> Rng32 -> (Int, Rng32)
randInt (nmin, nmax) gen = (val, nxt) where
    nxt = xorshift32 gen
    val = nmin + (fromIntegral nxt)  `mod` (nmax + 1 - nmin)
