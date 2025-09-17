{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Network.Wai                 (Application)
import           Network.Wai.Test            (runSession, request, defaultRequest, setPath)
import           Network.HTTP.Types          (status200)
import qualified Main                        (app)
import           DB                          (initDB)
import           Database.SQLite.Simple      (Connection)

data TestEnv = TestEnv { conn :: Connection, app :: Application }

initTestApp :: IO TestEnv
initTestApp = do
  c <- initDB "test.db"
  let application = Main.app c
  pure $ TestEnv c application

shutdownTestApp :: TestEnv -> IO ()
shutdownTestApp _ = pure ()

main :: IO ()
main = hspec $ beforeAll initTestApp $ afterAll shutdownTestApp $ do
  describe "GET /snapshots pagination" $ do
    it "returns 200 and paginated list" $ \env -> do
      let req = setPath defaultRequest "/snapshots?page=1&limit=5"
      res <- runSession (request req) (app env)
      simpleStatus res `shouldBe` status200
      simpleBody   res `shouldSatisfy` (not . null)
