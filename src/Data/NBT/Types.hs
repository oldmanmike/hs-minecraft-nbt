{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- |
-- Module       : Data.NBT.Types
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable
--
-------------------------------------------------------------------------------
module Data.NBT.Types
  ( NBT (..)
  , TagType (..)
  , NamelessNBT (..)
  , NBTList (..)
  ) where

import            Control.Applicative
import            Control.DeepSeq
import            Data.Data
import            Data.Int
import qualified  Data.Text as T
import            Data.Typeable
import qualified  Data.Vector.Unboxed as U
import            GHC.Generics

data NBT
  = TagByte       T.Text Int8
  | TagShort      T.Text Int16
  | TagInt        T.Text Int32
  | TagLong       T.Text Int64
  | TagFloat      T.Text Float
  | TagDouble     T.Text Double
  | TagByteArray  T.Text (U.Vector Int8)
  | TagString     T.Text T.Text
  | TagList       T.Text NBTList
  | TagCompound   T.Text [NBT]
  | TagIntArray   T.Text (U.Vector Int32)
  deriving (Show,Read,Eq,Typeable,Data,Generic)

instance NFData NBT

data NamelessNBT
  = NTagByte       Int8
  | NTagShort      Int16
  | NTagInt        Int32
  | NTagLong       Int64
  | NTagFloat      Float
  | NTagDouble     Double
  | NTagByteArray  (U.Vector Int8)
  | NTagString     T.Text
  | NTagList       NBTList
  | NTagCompound   [NBT]
  | NTagIntArray   (U.Vector Int32)
  deriving (Show,Read,Eq,Typeable,Data,Generic)

instance NFData NamelessNBT

data NBTList = NBTList TagType [NamelessNBT] deriving (Show,Read,Eq,Typeable,Data,Generic)

instance NFData NBTList

data TagType
  = TypeEnd
  | TypeByte
  | TypeShort
  | TypeInt
  | TypeLong
  | TypeFloat
  | TypeDouble
  | TypeByteArray
  | TypeString
  | TypeList
  | TypeCompound
  | TypeIntArray
  deriving (Show,Read,Eq,Enum,Typeable,Data,Generic)

instance NFData TagType
