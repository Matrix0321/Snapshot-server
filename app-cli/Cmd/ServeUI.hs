{-# LANGUAGE OverloadedStrings #-}
module Cmd.ServeUI (runServeUI) where

import           Data.Proxy       (Proxy(..))
import           Servant.Client
import           API              (SnapshotAPI, SparqlInput, SnapshotCreated)
import           Models           (Snapshot)

-- 新 API 多了分页参数，先取 Raw client 再封装
(postSnap :<|> getByName :<|> listSnapshotsRaw
 :<|> getSignature :<|> delAll :<|> delByName)
  = client (Proxy :: Proxy SnapshotAPI)

-- 无分页默认调用
listSnapshots :: ClientM [Snapshot]
listSnapshots = listSnapshotsRaw Nothing Nothing Nothing Nothing Nothing

-- Browser UI 的路由类型
type UI =
       Get '[HTML] (HtmlT Identity ())
  :<|> "create" :> Get    '[HTML] (HtmlT Identity ())
  :<|> "create" :> ReqBody '[JSON] APIType.SparqlInput
               :> Post   '[HTML] (HtmlT Identity ())
  :<|> "query"
        :> QueryParam "rel"   Text
        :> QueryParam "limit" Int
        :> QueryParam "from"  Int
        :> QueryParam "to"    Int
        :> Get    '[HTML] (HtmlT Identity ())

-- 组装 Servant server
uiServer :: IO (Server UI)
uiServer = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")
  pure $ homePage
        :<|> showCreateForm
        :<|> submitCreate env
        :<|> runQuery env
 where
  homePage :: Handler (HtmlT Identity ())
  homePage = pure $ html_ $ do
    h2_ "Snapshot Browser"
    form_ [ action_ "/query", method_ "get" ] $ do
      "Relationship keyword: " >> input_ [ name_ "rel" ] >> br_ []
      "Limit: "                >> input_ [ name_ "limit", type_ "number" ] >> br_ []
      "From year: "            >> input_ [ name_ "from",  type_ "number" ] >> br_ []
      "To year: "              >> input_ [ name_ "to",    type_ "number" ] >> br_ []
      input_ [ type_ "submit", value_ "Query" ]

  showCreateForm :: Handler (HtmlT Identity ())
  showCreateForm = pure $ html_ $ do
    h2_ "Create Snapshot"
    form_ [ action_ "/create", method_ "post" ] $ do
      "Paste SparqlInput JSON:" >> br_ []
      textarea_ [ name_ "body", rows_ "10", cols_ "80" ] "" >> br_ []
      input_ [ type_ "submit", value_ "Create" ]

  submitCreate
    :: ClientEnv
    -> APIType.SparqlInput
    -> Handler (HtmlT Identity ())
  submitCreate env inp = do
    eres <- liftIO $ runClientM (_postSnap inp) env
    pure $ html_ $ case eres of
      Left err -> do
        h3_ "Error creating snapshot"
        p_ $ toHtml $ show err
        p_ $ a_ [ href_ "/" ] "↩ Back"
      Right (APIType.SnapshotCreated newId) -> do
        h3_ "Created!"
        p_ $ toHtml $ "New ID: " <> newId
        p_ $ a_ [ href_ "/" ] "↩ Home"

  runQuery
    :: ClientEnv
    -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int
    -> Handler (HtmlT Identity ())
  runQuery env mRel mLim mFrom mTo = do
    eres <- liftIO $ runClientM listSnapshots env
    pure $ html_ $ case eres of
      Left err -> p_ $ toHtml $ "Error: " <> T.pack (show err)
      Right snaps ->
        let filtered = filterBy mRel mFrom mTo snaps
            limited  = maybe filtered (`take` filtered) mLim
        in do
          h3_ "Results"
          table_ $ do
            thead_ $ tr_ $ mapM_ th_ ["Name","ID","CreatedAt"]
            tbody_ $ forM_ limited $ \(Snapshot sid md _) ->
              tr_ $ do
                td_ $ toHtml $ snapshotName md
                td_ $ toHtml sid
                td_ $ toHtml $ createdAt md
          p_ $ a_ [ href_ "/" ] "Back"

filterBy
  :: Maybe Text -> Maybe Int -> Maybe Int -> [Snapshot] -> [Snapshot]
filterBy mRel mFrom mTo =
    filter byDate . filter byName
  where
    byName (Snapshot _ md _) = maybe True (`T.isInfixOf` snapshotName md) mRel
    byDate (Snapshot _ md _) =
      case reads (T.unpack $ T.take 4 $ createdAt md) of
        [(y,"")] -> maybe True (y>=) mFrom && maybe True (y<=) mTo
        _        -> False

-- 启动 UI
run :: Int -> IO ()
run port = do
  server <- uiServer
  putStrLn $ "snapshot-ui running at http://localhost:" <> show port
  Warp.run port (serve (Proxy :: Proxy UI) server)
