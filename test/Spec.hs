
module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Toolkit


main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [removeSpacesTest])

removeSpacesTest :: TestTree
removeSpacesTest = testCase "Testing removeSpaces" $ do
  assertEqual "One word " "سلام" (removeSpaces "سلام   ")
  assertEqual "A sentence" "به گزارش خبرگزاری فارس از ساعت 23 و30 دقیقه سن ژرمن به مصاف گنگام می رود." (removeSpaces "  به گزارش خبرگزاری فارس از    ساعت 23 و30 دقیقه سن ژرمن   به مصاف گنگام می رود. ")
