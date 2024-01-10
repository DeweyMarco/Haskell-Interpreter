import           Control.Monad (forM_, when)
import           Data.List     (permutations, sort)
import           Test.Hspec
import           Parser

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    describe "Read Numbers" $ do
      it "can parse integers" $ do
        eval "10" `shouldBe` Left ([10.0] :: [Double])

      it "can parse negative integers" $ do
        eval "-10" `shouldBe` Left ([-10.0] :: [Double])

      it "can parse doubles" $ do
        eval "1.5" `shouldBe` Left ([1.5] :: [Double])

      it "can parse negative doubles" $ do
        eval "-1.5" `shouldBe` Left ([-1.5] :: [Double])

      it "can parse floats" $ do
        eval "1e2" `shouldBe` Left ([100.0] :: [Double])

      it "can parse negative floats" $ do
        eval "-1e2" `shouldBe` Left ([-100.0] :: [Double])

      it "can parse scientific notation" $ do
        eval "1.1e2" `shouldBe` Left ([110.0] :: [Double])

      it "can parse negative scientific notation" $ do
        eval "-1.1e2" `shouldBe` Left ([-110.0] :: [Double])

      it "can parse scientific notation" $ do
        eval "1.1e-1" `shouldBe` Left ([0.11] :: [Double])

      it "can parse negative scientific notation" $ do
        eval "-1.1e-1" `shouldBe` Left ([-0.11] :: [Double])

      it "can parse scientific notation with negative exp" $ do
        eval "111.1e-2" `shouldBe` Left ([1.111] :: [Double])

      it "can parse negative scientific notation with negative exp" $ do
        eval "-111.1e-2" `shouldBe` Left ([-1.111] :: [Double])
    
    describe "Isolated Operation" $ do
      it "can add numbers" $ do
        eval "5+5" `shouldBe` Left ([10.0] :: [Double])

      it "can subtract numbers" $ do
        eval "15-5" `shouldBe` Left ([10.0] :: [Double])

      it "can multiply numbers" $ do
        eval "5*2" `shouldBe` Left ([10.0] :: [Double])

      it "can divide numbers" $ do
        eval "20/2" `shouldBe` Left ([10.0] :: [Double])

    describe "Ignore Comments" $ do
      it "can ignore comments" $ do
        eval "-- comment" `shouldBe` Left ([] :: [Double])

      it "can ignore multiple comments" $ do
        eval "--comment;--comment" `shouldBe` Left ([] :: [Double])

    describe "Chaining Operators" $ do
      it "can add and subtract numbers" $ do
        eval "2+1-3" `shouldBe` Left ([0.0] :: [Double])

      it "can subtract and add numbers" $ do
        eval "5-4+3" `shouldBe` Left ([4.0] :: [Double])

      it "can add and multiply numbers" $ do
        eval "5+2*3" `shouldBe` Left ([11.0] :: [Double])

      it "can multiply and add numbers" $ do
        eval "5*2+3" `shouldBe` Left ([13.0] :: [Double])

      it "can subtract and multiply numbers" $ do
        eval "5-2*3" `shouldBe` Left ([-1.0] :: [Double])

      it "can multiply and subtract numbers" $ do
        eval "5*2-3" `shouldBe` Left ([7.0] :: [Double]) 

      it "can add and divide numbers" $ do
        eval "5+4/2" `shouldBe` Left ([7.0] :: [Double])

      it "can divide and add numbers" $ do
        eval "6/2+3" `shouldBe` Left ([6.0] :: [Double])

      it "can subtract and divide numbers" $ do
        eval "5-8/4" `shouldBe` Left ([3.0] :: [Double])

      it "can divide and subtract numbers" $ do
        eval "10/2-3" `shouldBe` Left ([2.0] :: [Double])

      it "can multiply and divide numbers" $ do
        eval "5*8/4" `shouldBe` Left ([10.0] :: [Double])

      it "can divide and multiply numbers" $ do
        eval "10/2*3" `shouldBe` Left ([15.0] :: [Double])

      it "can add, subtract, divide, and multiply numbers" $ do
        eval "4+5*8/4-2" `shouldBe` Left ([12.0] :: [Double])

      it "can add multiple numbers" $ do
        eval "2+2+2" `shouldBe` Left ([6] :: [Double])

      it "can subtract multiple numbers" $ do
        eval "2-2-2" `shouldBe` Left ([-2.0] :: [Double])

      it "can multiply multiple numbers" $ do
        eval "2*2*2" `shouldBe` Left ([8.0] :: [Double])

      it "can divide multiple numbers" $ do
        eval "2/2/2" `shouldBe` Left ([0.5] :: [Double])

    describe "Parentheses" $ do 
      it "can add before multiply" $ do
        eval "2*(2+3)" `shouldBe` Left ([10.0] :: [Double])

      it "can subtract before multiply" $ do
        eval "(5-4)*3" `shouldBe` Left ([3.0] :: [Double])

    describe "Exponents" $ do 
      it "can do exponents" $ do
        eval "2^2" `shouldBe` Left ([4.0] :: [Double])

      it "can do multiple exponents" $ do
        eval "4^2^2" `shouldBe` Left ([256.0] :: [Double])

      it "can do order of operations on exponents" $ do
        eval "2^2/2^2" `shouldBe` Left ([1.0] :: [Double])

      it "can do multiple exponents with expressions" $ do
        eval "4^(2+2)" `shouldBe` Left ([256.0] :: [Double]) 

    describe "Multiple Statements" $ do  
      it "can evaluate multiple statements" $ do
        eval "2*2;1*0" `shouldBe` Left ([4.0,0.0] :: [Double])

      it "can evaluate multiple statements and comments" $ do
        eval "2*2;--4.0;1*0;--0.0" `shouldBe` Left ([4.0,0.0] :: [Double])

    describe "Variables" $ do 
      it "can use variables" $ do
        eval "x=3;var = 4; x + var" `shouldBe` Left ([7.0] :: [Double])

      it "can use same variables" $ do
        eval "y=2;y=3;y=5;y+1" `shouldBe` Left ([6.0] :: [Double])
    describe "Negation" $ do 
      it "negative numbers" $ do
        eval "-1--1" `shouldBe` Left ([0.0] :: [Double])

      it "can use same variables" $ do
        eval "x=5;-x" `shouldBe` Left ([-5.0] :: [Double])
    describe "Errors" $ do 
      it "to many additions" $ do
        eval "1++3" `shouldBe` Right ("Parser Error" :: String)

      it "unparsed symbols" $ do
        eval "1+3+" `shouldBe` Right ("Parser Error" :: String)
      
      it "use before declaration" $ do
        eval "a+b" `shouldBe` Right ("Use Before Defined" :: String)
      
      it "explicit divistion by zero" $ do
        eval "1/0" `shouldBe` Right ("Division by Zero" :: String)

      it "implicit divistion by zero" $ do
        eval "1/(3-3)" `shouldBe` Right ("Division by Zero" :: String)

  describe "Ast" $ do
    describe "Read Numbers" $ do
      it "can parse integers" $ do
        eval_Ast (produce_Ast "10") `shouldBe` Left ([10.0] :: [Double])

      it "can parse negative integers" $ do
        eval_Ast (produce_Ast "-10") `shouldBe` Left ([-10.0] :: [Double])

      it "can parse doubles" $ do
        eval_Ast (produce_Ast "1.5") `shouldBe` Left ([1.5] :: [Double])

      it "can parse negative doubles" $ do
        eval_Ast (produce_Ast "-1.5") `shouldBe` Left ([-1.5] :: [Double])

      it "can parse floats" $ do
        eval_Ast (produce_Ast "1e2") `shouldBe` Left ([100.0] :: [Double])

      it "can parse negative floats" $ do
        eval_Ast (produce_Ast "-1e2") `shouldBe` Left ([-100.0] :: [Double])

      it "can parse scientific notation" $ do
        eval_Ast (produce_Ast "1.1e2") `shouldBe` Left ([110.0] :: [Double])

      it "can parse negative scientific notation" $ do
        eval_Ast (produce_Ast "-1.1e2") `shouldBe` Left ([-110.0] :: [Double])

      it "can parse scientific notation" $ do
        eval_Ast (produce_Ast "1.1e-1") `shouldBe` Left ([0.11] :: [Double])

      it "can parse negative scientific notation" $ do
        eval_Ast (produce_Ast "-1.1e-1") `shouldBe` Left ([-0.11] :: [Double])

      it "can parse scientific notation with negative exp" $ do
       eval_Ast (produce_Ast "111.1e-2") `shouldBe` Left ([1.111] :: [Double])

      it "can parse negative scientific notation with negative exp" $ do
       eval_Ast (produce_Ast "-111.1e-2") `shouldBe` Left ([-1.111] :: [Double])
    
    describe "Isolated Operation" $ do
      it "can add numbers" $ do
       eval_Ast (produce_Ast "5+5") `shouldBe` Left ([10.0] :: [Double])

      it "can subtract numbers" $ do
       eval_Ast (produce_Ast "15-5") `shouldBe` Left ([10.0] :: [Double])

      it "can multiply numbers" $ do
       eval_Ast (produce_Ast "5*2") `shouldBe` Left ([10.0] :: [Double])

      it "can divide numbers" $ do
       eval_Ast (produce_Ast "20/2") `shouldBe` Left ([10.0] :: [Double])

    describe "Ignore Comments" $ do
      it "can ignore comments" $ do
       eval_Ast (produce_Ast "-- comment") `shouldBe` Left ([] :: [Double])

      it "can ignore multiple comments" $ do
       eval_Ast (produce_Ast "--comment;--comment") `shouldBe` Left ([] :: [Double])

    describe "Chaining Operators" $ do
      it "can add and subtract numbers" $ do
       eval_Ast (produce_Ast "2+1-3") `shouldBe` Left ([0.0] :: [Double])

      it "can subtract and add numbers" $ do
       eval_Ast (produce_Ast "5-4+3") `shouldBe` Left ([4.0] :: [Double])

      it "can add and multiply numbers" $ do
       eval_Ast (produce_Ast "5+2*3") `shouldBe` Left ([11.0] :: [Double])

      it "can multiply and add numbers" $ do
       eval_Ast (produce_Ast "5*2+3") `shouldBe` Left ([13.0] :: [Double])

      it "can subtract and multiply numbers" $ do
       eval_Ast (produce_Ast "5-2*3") `shouldBe` Left ([-1.0] :: [Double])

      it "can multiply and subtract numbers" $ do
       eval_Ast (produce_Ast "5*2-3") `shouldBe` Left ([7.0] :: [Double]) 

      it "can add and divide numbers" $ do
       eval_Ast (produce_Ast "5+4/2") `shouldBe` Left ([7.0] :: [Double])

      it "can divide and add numbers" $ do
       eval_Ast (produce_Ast "6/2+3") `shouldBe` Left ([6.0] :: [Double])

      it "can subtract and divide numbers" $ do
       eval_Ast (produce_Ast "5-8/4") `shouldBe` Left ([3.0] :: [Double])

      it "can divide and subtract numbers" $ do
       eval_Ast (produce_Ast "10/2-3") `shouldBe` Left ([2.0] :: [Double])

      it "can multiply and divide numbers" $ do
       eval_Ast (produce_Ast "5*8/4") `shouldBe` Left ([10.0] :: [Double])

      it "can divide and multiply numbers" $ do
       eval_Ast (produce_Ast "10/2*3") `shouldBe` Left ([15.0] :: [Double])

      it "can add, subtract, divide, and multiply numbers" $ do
       eval_Ast (produce_Ast "4+5*8/4-2") `shouldBe` Left ([12.0] :: [Double])

      it "can add multiple numbers" $ do
       eval_Ast (produce_Ast "2+2+2") `shouldBe` Left ([6] :: [Double])

      it "can subtract multiple numbers" $ do
       eval_Ast (produce_Ast "2-2-2") `shouldBe` Left ([-2.0] :: [Double])

      it "can multiply multiple numbers" $ do
       eval_Ast (produce_Ast "2*2*2") `shouldBe` Left ([8.0] :: [Double])

      it "can divide multiple numbers" $ do
       eval_Ast (produce_Ast "2/2/2") `shouldBe` Left ([0.5] :: [Double])

    describe "Parentheses" $ do 
      it "can add before multiply" $ do
       eval_Ast (produce_Ast "2*(2+3)") `shouldBe` Left ([10.0] :: [Double])

      it "can subtract before multiply" $ do
       eval_Ast (produce_Ast "(5-4)*3") `shouldBe` Left ([3.0] :: [Double])

    describe "Exponents" $ do 
      it "can do exponents" $ do
       eval_Ast (produce_Ast "2^2") `shouldBe` Left ([4.0] :: [Double])

      it "can do multiple exponents" $ do
       eval_Ast (produce_Ast "4^2^2") `shouldBe` Left ([256.0] :: [Double])

      it "can do order of operations on exponents" $ do
       eval_Ast (produce_Ast "2^2/2^2") `shouldBe` Left ([1.0] :: [Double])

      it "can do multiple exponents with expressions" $ do
       eval_Ast (produce_Ast "4^(2+2)") `shouldBe` Left ([256.0] :: [Double]) 

    describe "Multiple Statements" $ do  
      it "can evaluate multiple statements" $ do
       eval_Ast (produce_Ast "2*2;1*0") `shouldBe` Left ([4.0,0.0] :: [Double])

      it "can evaluate multiple statements and comments" $ do
       eval_Ast (produce_Ast "2*2;--4.0;1*0;--0.0") `shouldBe` Left ([4.0,0.0] :: [Double])

    describe "Variables" $ do 
      it "can use variables" $ do
       eval_Ast (produce_Ast "x=3;var = 4; x + var") `shouldBe` Left ([7.0] :: [Double])

      it "can use same variables" $ do
       eval_Ast (produce_Ast "y=2;y=3;y=5;y+1") `shouldBe` Left ([6.0] :: [Double])
    describe "Negation" $ do 
      it "negative numbers" $ do
       eval_Ast (produce_Ast "-1--1") `shouldBe` Left ([0.0] :: [Double])

      it "can use same variables" $ do
       eval_Ast (produce_Ast "x=5;-x") `shouldBe` Left ([-5.0] :: [Double])
    describe "Errors" $ do 
      it "to many additions" $ do
       eval_Ast (produce_Ast "1++3") `shouldBe` Right ("Parser Error" :: String)

      it "unparsed symbols" $ do
       eval_Ast (produce_Ast "1+3+") `shouldBe` Right ("Parser Error" :: String)
      
      it "use before declaration" $ do
       eval_Ast (produce_Ast "a+b") `shouldBe` Right ("Use Before Defined" :: String)
      
      it "explicit divistion by zero" $ do
       eval_Ast (produce_Ast "1/0") `shouldBe` Right ("Division by Zero" :: String)

      it "implicit divistion by zero" $ do
       eval_Ast (produce_Ast "1/(3-3)") `shouldBe` Right ("Division by Zero" :: String)