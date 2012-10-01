module Tests.PreProcessorTests where

import qualified Test.HUnit as HU
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Diagnoser.PreProcessor

scriptPath = "Tests/diagnoser/implemented/Callscript"


preProcessorTests :: IO Test
preProcessorTests =
  do simpleA   <- assertPreProcess simple
     withParametersA <- assertPreProcess withParameters
     betweenA <- assertPreProcess between
     nestedA  <- assertPreProcess nested
     nestedWPA  <- assertPreProcess nestedWithParameters
     subFolderA  <- assertPreProcess subFolder
     parentFolderA  <- assertPreProcess parentFolder
     canMsgA  <- assertPreProcess canMsgTest
     cycliccanMsgA  <- assertPreProcess cyclicCanMsgTest
     return $ testGroup "PreProcessor"
       [testCase "Simple callscript"  simpleA
       ,testCase "With parameters"  withParametersA
       ,testCase "Callscript betwen otherstuff"  betweenA
       ,testCase "Nested"  nestedA
       ,testCase "Nested with parameters"  nestedWPA
       ,testGroup "Call scripts in diffrent folders"
          [testCase "Call script in sub folder"  subFolderA
          ,testCase "Call script in parent folder"  parentFolderA]
       ,testGroup "replacement for diffrent skript elements"
          [testCase "CanMsg"        canMsgA
          ,testCase "CyclicCanMsg"  cycliccanMsgA]]



simple = ("/simple.skr",
          "DIAG [ReadDataByIdentifier_3] SEND [22,F1,90] EXPECT [*] TIMEOUT [1400]")


between = ("/between.skr"
          ,"WAIT [100]\nDIAG [ReadDataByIdentifier_3] SEND [22,F1,90] EXPECT [*] TIMEOUT [1400]\nDIAG [abc] SEND [01,02,03] EXPECT [aa,bb,cc] TIMEOUT [2000] SOURCE [A0] TARGET [B0]")

withParameters = ("/withParameters.skr",
                  "DIAG [ReadDataByIdentifier__DID_] SEND [22,f1,90] EXPECT [62,f1,90,a1,b2] TIMEOUT [1000]")

nested  = ("/nested.skr"
          ,"DIAG [ReadDataByIdentifier_3] SEND [22,F1,90] EXPECT [*] TIMEOUT [1400]")

nestedWithParameters  = ("/nestedWithParameters.skr"
                        ,"DIAG [ReadDataByIdentifier__DID_] SEND [f1,90,22] EXPECT [62] TIMEOUT [100]")


subFolder = ("/subFolder.skr",
             "DIAG [ReadDataByIdentifier_3] SEND [22,F1,90] EXPECT [*] TIMEOUT [1400]")

parentFolder = ("/subfolder/parentFolder.skr",
                "DIAG [ReadDataByIdentifier_3] SEND [22,F1,90] EXPECT [*] TIMEOUT [1400]")

canMsgTest = ("/canMsg.skr"
             ,"CANMSG [CAN_1] ID [000] DATA [11,11,22,*,44]")

cyclicCanMsgTest = ("/cyclicCanMsg.skr"
                   ,"STARTCYCLICCANMSG [Klemme_15] ID [000] DATA [11,22,00,00,00] CYCLE [11]\nWAIT [1000]\nSTOPCYCLICCANMSG  [Klemme_15]")


assertPreProcess :: (FilePath, String) -> IO HU.Assertion
assertPreProcess test =
  do let (file,expected) = test
--     skript    <- readFile (scriptPath ++ file)
     processed <- preProcess (scriptPath ++ file)
--     processed <- preProcess skript
     let res = case processed of
                (Left  _) -> False
                (Right p) -> p == expected
     return $ HU.assert res


preP test = preProcess (scriptPath ++ fst test)


