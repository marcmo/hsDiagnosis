import QC
import Test.QuickCheck

deepCheck s = quickCheckWith stdArgs { maxSuccess = 500 } s

main = do
     deepCheck prop_bytes2Msg2bytes
     deepCheck prop_diag2hsfz2diag
