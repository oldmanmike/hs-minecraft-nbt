{-# LANGUAGE DeriveDataTypeable #-}
module Data.NBT.Types
  ( NBT (..)
  , TagType (..)
  ) where

import            Control.Applicative
import            Data.Array.IArray (Array)
import            Data.Array.Unboxed (UArray)
import            Data.Int
import qualified  Data.Text as T
import            Data.Typeable

data NBT
  = TagByte       T.Text !Int8
  | TagShort      T.Text !Int16
  | TagInt        T.Text !Int32
  | TagLong       T.Text !Int64
  | TagFloat      T.Text !Float
  | TagDouble     T.Text !Double
  | TagByteArray  T.Text !(UArray Int32 Int8)
  | TagString     T.Text !T.Text
  | TagList       T.Text !(Array Int32 Int8)
  | TagCompound   T.Text ![NBT]
  | TagIntArray   T.Text !(UArray Int32 Int32)
  deriving (Show,Eq,Typeable)

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
  deriving (Show,Eq,Enum)
