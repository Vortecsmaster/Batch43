{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Batch43SignedMinting where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--ON-CHAIN

{-# INLINABLE signedMintingPolicy #-}
signedMintingPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicy pkh _ sContext = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

signedpolicy :: PaymentPubKeyHash -> Scripts.MintingPolicy
signedpolicy pkh = mkMintingPolicyScript $ 
                                   $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . signedMintingPolicy ||])
                                   `PlutusTx.applyCode`
                                   PlutusTx.liftCode pkh
             
curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . signedpolicy

-- OFF-CHAIN
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
          pkh <- Contract.ownPaymentPubKeyHash 
          let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
              lookups = Constraints.mintingPolicy (signedpolicy pkh)
              tx      = Constraints.mustMintValue val
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ printf "We forged %s" (show val)

type FreeSchema = Endpoint "mint" MintParams

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

test:: IO ()
test= runEmulatorTraceIO $ do
                          h1 <- activateContractWallet (knownWallet 1) endpoints
                          h2 <- activateContractWallet (knownWallet 2) endpoints
                          callEndpoint @"mint" h1 $ MintParams
                                                  { mpTokenName = "Batch43token1",
                                                    mpAmount = 11000}
                          callEndpoint @"mint" h2 $ MintParams
                                                  { mpTokenName = "Batch43token1",
                                                    mpAmount = 22000}
                          void $ Emulator.waitNSlots 10