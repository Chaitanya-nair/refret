{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin -fplugin=Data.Record.Anon.Plugin -fplugin=Data.Record.Plugin.HasFieldPattern #-}

module EC.MerchantAccount
  ( module EC.MerchantAccount,
    module EC.MerchantAccount.Types,
  )
where

import Config.Constants (ecDB)
import DB.Types (Date)
import qualified DB.Utils as DBUtils ()
import qualified {- nau -} Database.Beam as B
import qualified {- nau -} Database.Beam.Backend.SQL as B
import qualified {- nau -} Database.Beam.MySQL
import qualified {- nau -} Database.Beam.Postgres
import {- nau -} EC.MerchantAccount.Types (Industry (..), IntegrationType (..), InternalMetadata (..), MerchantAccount, MerchantAccountT (..), MerchantTrack (..))
import ECPrelude
import Engineering.Flow (internalError, merchantAccountNull)
import Engineering.Types.API (ECErrorResponse)
import qualified {- nau -} Euler.DB.Storage.Types.MerchantAccount as StorageType
import qualified {- nau -} Nau.Runtime as Nau
import {- nau -} NauPrelude
import qualified {- nau -} NauPrelude as NauP
import qualified {- nau -} PS.Prelude
import PS.Presto.Backend.Flow (BackendFlow)
import qualified {- nau -} PS.Sequelize as Se
import PS.Sequelize.CRUD.Read ()
import qualified {- nau -} PS.Sequelize.Extra as SE
import PS.Sequelize.Models ()
import PS.Sequelize.Query.Options (where_)
import PS.Sequelize.Types (Conn, Instance, ModelOf, SEQUELIZE)
import PS.Sequelize.Where (Literal, Literal' (..), Term' (..), Where, Where' (..))
import qualified {- nau -} Sequelize as Se
import qualified Types.Alias as A
import Types.App (ApiType (..), Configs (..), Flow)
import qualified {- nau -} Types.Lenses as L
import qualified {- nau -} Types.Lenses.Shims as L
import Utils.PrestoBackend (TState)
import qualified Utils.PrestoBackend as DB
import qualified {- nau -} Prelude


baz, bar, quux :: IO ()
baz = fooOld "foo"

bar = fooOld "bar"

quux = fooOld "quux"

_txnIdCustomPrefix :: forall a b c. (Nau.HasCallStack, Field' "____txnIdCustomPrefix" c b, Newtype a c) => Lens' a b
_txnIdCustomPrefix = lens (unwrap >>> (\lhs -> lhs.____txnIdCustomPrefix)) (\oldRec newVal -> wrap ((unwrap oldRec){____txnIdCustomPrefix = newVal}))

_returnUrl :: forall a b c. (Nau.HasCallStack, Field' "____returnUrl" c b, Newtype a c) => Lens' a b
_returnUrl = lens (unwrap >>> (\lhs -> lhs.____returnUrl)) (\oldRec newVal -> wrap ((unwrap oldRec){____returnUrl = newVal}))

_lockerId :: forall a b c. (Nau.HasCallStack, Field' "____lockerId" c b, Newtype a c) => Lens' a b
_lockerId = lens (unwrap >>> (\lhs -> lhs.____lockerId)) (\oldRec newVal -> wrap ((unwrap oldRec){____lockerId = newVal}))

-- findOneMerchantAccount :: forall e. ModelOf MerchantAccount -> Options MerchantAccount ->  Aff (sequelize :: SEQUELIZE | e) (Maybe (Instance MerchantAccount ))
-- findOneMerchantAccount model whereC = findOne model whereC
findMerchantAccountById :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Int -> BackendFlow st Configs MerchantAccount
findMerchantAccountById merchantAccountId = DB.findOneWithErr ecDB [Se.And [Se.Is (StorageType.id) (Se.Eq (Just merchantAccountId))]] $ merchantAccountNull

findMerchantAccount :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st Configs MerchantAccount
findMerchantAccount mid = DB.findOneWithErr ecDB [Se.And [Se.Is (StorageType.merchantId) (Se.Eq (Just mid))]] $ merchantAccountNull

findByMerchantIdMaybe :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st Configs (Maybe MerchantAccount)
findByMerchantIdMaybe "" = pure Nothing
findByMerchantIdMaybe mid = DB.findOne ecDB [Se.And [Se.Is (StorageType.merchantId) (Se.Eq (Just mid))]]

-- NAU: replaced _ with Configs because otherwise GHC gives out:
-- "Iface type variable out of scope: k"
findMerchantAccountByIdOpt :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Int -> BackendFlow st Configs (Maybe MerchantAccount)
findMerchantAccountByIdOpt id = DB.findOne ecDB [Se.And [Se.Is (StorageType.id) (Se.Eq (Just id))]]

findMerchantAccountByMerchantId :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st Configs (Maybe MerchantAccount)
findMerchantAccountByMerchantId mid = DB.findOne ecDB [Se.And [Se.Is (StorageType.merchantId) (Se.Eq (Just mid))]]

findMerchantAccountsByLockerId :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st _ ([MerchantAccount])
findMerchantAccountsByLockerId lockerId = DB.findAll ecDB [Se.And [Se.Is (StorageType.lockerId) (Se.Eq (Just lockerId))]]

findMerchantAccountsByTokenLockerId :: forall st e. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st _ ([MerchantAccount])
findMerchantAccountsByTokenLockerId tokenLockerId = DB.findAll ecDB [Se.And [Se.Is (StorageType.tokenLockerId) (Se.Eq (Just tokenLockerId))]]

getMerchantAccount :: forall e st. (Nau.HasCallStack, Newtype st (TState e)) => Text -> BackendFlow st Configs MerchantAccount
getMerchantAccount merchantId = DB.findOneWithErr ecDB [(Se.And [Se.Is (StorageType.merchantId) (Se.Eq (Just merchantId))] :: Se.Clause Database.Beam.MySQL.MySQL MerchantAccountT)] internalError

_merchantId :: Nau.HasCallStack => Lens' MerchantAccount (Maybe Text)
_merchantId = lens (unwrap >>> (\lhs -> lhs.____merchantId)) (\o n -> wrap ((unwrap o){____merchantId = n}))

isMerchantInRestrictedMode :: Nau.HasCallStack => MerchantAccount -> Bool
isMerchantInRestrictedMode (mAcc@MerchantAccount {}) =
  let (i :: Maybe InternalMetadata) = getInternalMetadata
   in fromMaybe False $ i >>= (\(meta@InternalMetadata {}) -> meta.____restricted_95_mode)
  where
    getInternalMetadata = mAcc.____internalMetadata >>= (hush <<< runExcept <<< decodeJSON)

-- Untranspiled names from EC.MerchantAccount:
--  - __enabledInstantRefund
--  - _apiKey
--  - _enableSendingCardIsin
--  - _enableSendingLastFourDigits
--  - _enableUnauthenticatedOrderStatusApi
--  - _resellerId
--  - aboutBusiness
--  - adminContactEmail
--  - alternativeEmail
--  - amexAccessCode
--  - amexMerchantId
--  - amexPassword
--  - amexSecureSecret
--  - amexUsername
--  - amexVerified
--  - apiKey
--  - autoRefundConflictThresholdInMins
--  - autoRefundConflictTransactions
--  - autoRefundMultipleChargedTransactions
--  - axisAccessCode
--  - axisMerchantId
--  - axisSecureSecret
--  - axisVerified
--  - bankAccountNumber
--  - cardEncodingKey
--  - ccavenueAccountId
--  - ccavenueBackendGateway
--  - ccavenueSecretKey
--  - ccavenueVerified
--  - citiKey
--  - citiMerchantCode
--  - citiTestMode
--  - citiVerified
--  - city
--  - conflictStatusEmail
--  - contactPerson
--  - contactPersonEmail
--  - contactPersonPrimary
--  - contactPersonSecondary
--  - contactPersonSecondaryEmail
--  - country
--  - creditBalance
--  - dateCreated
--  - domain
--  - ebsAccountId
--  - ebsBackendGateway
--  - ebsHash
--  - ebsVerified
--  - enable3DsecureHelpMail
--  - enableAutomaticRetry
--  - enableConflictStatusNotification
--  - enableExternalRiskCheck
--  - enableGatewayReferenceIdBasedRouting
--  - enableOrderNotification
--  - enablePaymentResponseHash
--  - enableReauthentication
--  - enableReauthorization
--  - enableRecapture
--  - enableRefundsInDashboard
--  - enableSaveCardBeforeAuth
--  - enableSendingCardIsin
--  - enableSendingLastFourDigits
--  - enableSuccessRateBasedGatewayElimination
--  - enableTxnFilter
--  - enableUnauthenticatedCardAdd
--  - enableUnauthenticatedOrderStatusApi
--  - enabled
--  - enabledInstantRefund
--  - executeMandateAutoRetryEnabled
--  - expressCheckoutEnabled
--  - externalMetadata
--  - fetchCardsFromPayu
--  - findByIdWithErr
--  - findByMerchantIdWithErr
--  - fingerprintOnTokenize
--  - gatewayDecidedByHealthEnabled
--  - gatewayPriority
--  - gatewayPriority'
--  - gatewayPriorityLogic
--  - gatewaySuccessRateBasedDeciderInput
--  - gatewaySuccessRateBasedOutageInput
--  - getMerchantAccountById
--  - getMerchantAccountByIdE
--  - getMerchantAccountByMerchantId
--  - getMerchantAccountByMerchantIdE
--  - getMerchantAccountByUserId
--  - getMerchantAccountForUser
--  - hdfcIvrMerchantId
--  - hdfcIvrPassword
--  - hdfcIvrVerified
--  - hdfcMerchantCode
--  - hdfcMerchantId
--  - hdfcPassword
--  - hdfcTestMode
--  - hdfcVerified
--  - iciciKey
--  - iciciMerchantId
--  - iciciSecondFactorId
--  - iciciVerified
--  - id
--  - includeSurchargeAmountForRefund
--  - inlineCheckoutEnabled
--  - inlineCheckoutHtml
--  - internalHashKey
--  - internalMetadata
--  - landmark
--  - lastModified
--  - lockerId
--  - makeMerchantAccount
--  - mandateAutoRevokeEnabled
--  - mandateRetryConfig
--  - mandatoryFA
--  - merchantId
--  - merchantLegalName
--  - merchantName
--  - mobile
--  - mobileVersion
--  - mustUseGivenOrderIdForTxn
--  - offerEnabled
--  - officeLine1
--  - officeLine2
--  - officeLine3
--  - orderFieldsInSettlementReport
--  - orderNotificationEmail
--  - otpEnabled
--  - paymentResponseHashKey
--  - payoutMid
--  - paypalClientId
--  - paypalSecret
--  - payuMerchantKey
--  - payuSalt
--  - payuTestMode
--  - payuVerified
--  - prefixMerchantIdForCardKey
--  - processAuthorizingResponse
--  - realModeOnly
--  - redirectToMerchantWithHttpPost
--  - resellerId
--  - resellerId'
--  - returnUrl
--  - reverseTokenEnabled
--  - secondaryMerchantAccountId
--  - settlementAccountId
--  - shouldAddSurcharge
--  - showSurchargeBreakupScreen
--  - state
--  - telephone
--  - timezone
--  - tokenLockerId
--  - txnIdCustomPrefix
--  - useCodeForGatewayPriority
--  - userId
--  - version
--  - webHookCustomHeaders
--  - webHookPassword
--  - webHookUsername
--  - webHookapiversion
--  - webHookurl
--  - webhookConfigs
--  - website
--  - whitelabelEnabled
--  - zip
