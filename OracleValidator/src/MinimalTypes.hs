{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MinimalTypes where

import qualified Prelude           as P
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx
import           PlutusTx.Prelude

data ChoiceId = ChoiceId Builtins.BuiltinByteString Builtins.BuiltinData 
    deriving (P.Eq, P.Show)

instance Eq ChoiceId where
    (==) = (==)

PlutusTx.makeIsDataIndexed ''ChoiceId [('ChoiceId, 0)]

data InputContent = IChoice ChoiceId Builtins.BuiltinData
    deriving (P.Eq, P.Show)

instance Eq InputContent where
    (==) = (==)

PlutusTx.makeIsDataIndexed ''InputContent [('IChoice, 1)]

data Input
    = NormalInput InputContent
    | MerkleizedInput InputContent Builtins.BuiltinData Builtins.BuiltinData
    deriving (P.Eq, P.Show)

instance Eq Input where
    (==) = (==)

PlutusTx.makeIsDataIndexed ''Input [('NormalInput, 0), ('MerkleizedInput, 1)]

-- | Extract the content of input.
getInputContent :: Input -> InputContent
getInputContent (NormalInput inputContent) = inputContent
getInputContent (MerkleizedInput inputContent _ _) = inputContent
{-# INLINEABLE getInputContent #-}