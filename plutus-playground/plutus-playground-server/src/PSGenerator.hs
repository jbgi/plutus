{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module PSGenerator
  ( generate
  ) where

import Control.Applicative ()
import Control.Lens ((&), set)
import Data.Aeson ()
import Data.Monoid ()
import Data.Proxy (Proxy(Proxy))
import qualified Data.Set as Set ()
import Data.Text (Text)
import qualified Data.Text as T ()
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import Language.PureScript.Bridge
  ( BridgePart
  , Language(Haskell)
  , SumType
  , buildBridge
  , mkSumType
  , writePSTypes
  )
import Language.PureScript.Bridge.PSTypes ()
import Playground.API (API, Evaluation, Fn, SourceCode)
import Servant.API ((:>), JSON, Post, ReqBody)
import Servant.PureScript
  ( HasBridge
  , Settings
  , _generateSubscriberAPI
  , apiModuleName
  , defaultBridge
  , defaultSettings
  , languageBridge
  , writeAPIModuleWithSettings
  )
import Wallet.UTXO.Types (Blockchain)

myBridge :: BridgePart
myBridge = defaultBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes = [mkSumType (Proxy :: Proxy SourceCode), mkSumType (Proxy :: Proxy Fn)]

mySettings :: Settings
mySettings =
  (defaultSettings & set apiModuleName "Playground.Server")
    {_generateSubscriberAPI = False}

generate :: FilePath -> IO ()
generate outputDir = do
  writeAPIModuleWithSettings
    mySettings
    outputDir
    myBridgeProxy
    (Proxy :: Proxy ("evaluate" :> ReqBody '[ JSON] Evaluation :> Post '[ JSON] Blockchain))
  writePSTypes outputDir (buildBridge myBridge) myTypes
