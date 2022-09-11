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
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Batch43GuessingGame where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Text                  (Text)
import Data.Void
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value, getCardanoTxId)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import Prelude (Semigroup (..))
import Prelude qualified as Haskell
import           Text.Printf          (printf)

--ON-CHAIN

newtype HashedString = HS BuiltinByteString
                       deriving (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''HashedString

newtype ClearString = CS BuiltinByteString
                      deriving (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''ClearString

{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HS secret) (CS guess)  = secret == sha2_256 guess

data Game 
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
               $$(PlutusTx.compile [|| validateGuess ||])
               $$(PlutusTx.compile [|| wrap ||])
    where
       wrap = Scripts.wrapValidator @HashedString @ClearString
    
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- UTIL

clearString :: Haskell.String -> ClearString
clearString = CS . toBuiltin . C.pack

hashString :: Haskell.String -> HashedString
hashString = HS . sha2_256 . toBuiltin . C.pack 


type GameSchema =
                Endpoint "theBounty" BountyParams
            .\/ Endpoint "theGuess" GuessParams

data BountyParams = BP
                  { bpSecret :: Haskell.String
                  , bpValue  :: Value}
                  deriving (Generic, FromJSON, ToJSON, ToSchema)

data GuessParams = GP
                { gpGuess :: Haskell.String
                } deriving (Generic, FromJSON, ToJSON, ToSchema)


theBounty :: BountyParams -> Contract w GameSchema Text ()
theBounty (BP secret value) = do
                            let tx = Constraints.mustPayToTheScript (hashString secret) value
                            ledgerTx <- submitTxConstraints gameInstance tx
                            logInfo @Haskell.String $ printf "Bounty created!"
-- tx-out $(cat vesting.addr)+5000000 \
-- tx-out-datum-hash-file unit.json \                            
-- cardano-cli address build --payment-script-file <actual plutus script> $TESTNET

theGuess :: GuessParams -> Contract w GameSchema Text ()
theGuess (GP guess) = do
                      utxos <- utxosAt gameAddress
                      case Map.toList utxos of
                        []             -> logInfo @Haskell.String $ printf "No UTxOs on the Contract!"                       
                        (oref,a):utxs  -> do
                                          let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                                                        Constraints.otherScript gameValidator
                                          let tx = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData $ clearString guess)
                                          ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                          logInfo @Haskell.String $ printf "Guess is right!"

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (theBounty' `select` theGuess') >> endpoints
   where
       theBounty' = endpoint @"theBounty" theBounty
       theGuess'  = endpoint @"theGuess" theGuess

mkSchemaDefinitions ''GameSchema
mkKnownCurrencies []       



