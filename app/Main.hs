{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.DeepSeq (NFData, deepseq)
import Control.Exception (AsyncException(StackOverflow), handle)
import Data.Foldable (for_, traverse_)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import GHC.Generics (Generic(..), (:*:)(..), (:+:)(..), K1(..), U1(..), Rec0)
import GHC.Stack (currentCallStack)
import Language.C (parseCFile)
import Language.C.Data.Position (Position)
import Language.C.Data.Position (Position)
import Language.C.Pretty (prettyUsingInclude)
import Language.C.System.GCC (newGCC)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)
import qualified Language.C.Data.Ident
import qualified Language.C.Data.Name
import qualified Language.C.Data.Node
import qualified Language.C.Data.Position as Position
import qualified Language.C.Syntax.AST
import qualified Language.C.Syntax.Constants
import qualified Text.PrettyPrint as Pretty

main :: IO ()
main = handle (\StackOverflow -> mapM_ putStrLn =<< currentCallStack) $ do
  [monoPrefix, read -> count] <- getArgs
  let
    paths = take count $ (monoPrefix </>) <$> sources
    preprocessorFlags =
      [ "-I" <> monoPrefix
      , "-I" <> monoPrefix </> "eglib/src"
      , "-I" <> monoPrefix </> "mono"
      , "-DHAVE_SGEN_GC"
      , "-fno-blocks"
      ]

  print =<< getCurrentTime
  parsed <- for paths
    $ parseCFile preprocessor temporaryDirectory preprocessorFlags
  print =<< getCurrentTime
  case sequence parsed of
    Left err -> hPrint stderr err
    Right translationUnits -> do
      translationUnits `deepseq` pure ()
  print =<< getCurrentTime
  where
    temporaryDirectory = Nothing
    preprocessor = newGCC "gcc"
    sources =
      [ "mono/sgen/sgen-alloc.c"
      , "mono/sgen/sgen-array-list.c"
      , "mono/sgen/sgen-cardtable.c"
      , "mono/sgen/sgen-debug.c"
      , "mono/sgen/sgen-descriptor.c"
      , "mono/sgen/sgen-fin-weak-hash.c"
      , "mono/sgen/sgen-gc.c"
      , "mono/sgen/sgen-gchandles.c"
      , "mono/sgen/sgen-gray.c"
      , "mono/sgen/sgen-hash-table.c"
      , "mono/sgen/sgen-internal.c"
      , "mono/sgen/sgen-layout-stats.c"
      , "mono/sgen/sgen-los.c"
      , "mono/sgen/sgen-marksweep.c"
      , "mono/sgen/sgen-memory-governor.c"
      , "mono/sgen/sgen-nursery-allocator.c"
      , "mono/sgen/sgen-pinning-stats.c"
      , "mono/sgen/sgen-pinning.c"
      , "mono/sgen/sgen-pointer-queue.c"
      , "mono/sgen/sgen-protocol.c"
      , "mono/sgen/sgen-qsort.c"
      , "mono/sgen/sgen-simple-nursery.c"
      , "mono/sgen/sgen-split-nursery.c"
      , "mono/sgen/sgen-thread-pool.c"
      , "mono/sgen/sgen-workers.c"
      ]

deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CAlignmentSpecifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CArraySize a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CAssemblyOperand a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CAssemblyStatement a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CAttribute a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CBuiltinThing a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CCompoundBlockItem a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CConstant a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CDeclaration a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CDeclarationSpecifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CDeclarator a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CDerivedDeclarator a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CEnumeration a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CExpression a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CExternalDeclaration a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CFunctionDef a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CFunctionSpecifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CInitializer a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CPartDesignator a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CStatement a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CStorageSpecifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CStringLiteral a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CStructureUnion a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CTranslationUnit a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CTypeQualifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.AST.CTypeSpecifier a)
deriving instance (NFData a) => NFData (Language.C.Syntax.Constants.Flags a)
deriving instance Generic (Language.C.Syntax.AST.CAlignmentSpecifier a)
deriving instance Generic (Language.C.Syntax.AST.CArraySize a)
deriving instance Generic (Language.C.Syntax.AST.CAssemblyOperand a)
deriving instance Generic (Language.C.Syntax.AST.CAssemblyStatement a)
deriving instance Generic (Language.C.Syntax.AST.CAttribute a)
deriving instance Generic (Language.C.Syntax.AST.CBuiltinThing a)
deriving instance Generic (Language.C.Syntax.AST.CCompoundBlockItem a)
deriving instance Generic (Language.C.Syntax.AST.CConstant a)
deriving instance Generic (Language.C.Syntax.AST.CDeclaration a)
deriving instance Generic (Language.C.Syntax.AST.CDeclarationSpecifier a)
deriving instance Generic (Language.C.Syntax.AST.CDeclarator a)
deriving instance Generic (Language.C.Syntax.AST.CDerivedDeclarator a)
deriving instance Generic (Language.C.Syntax.AST.CEnumeration a)
deriving instance Generic (Language.C.Syntax.AST.CExpression a)
deriving instance Generic (Language.C.Syntax.AST.CExternalDeclaration a)
deriving instance Generic (Language.C.Syntax.AST.CFunctionDef a)
deriving instance Generic (Language.C.Syntax.AST.CFunctionSpecifier a)
deriving instance Generic (Language.C.Syntax.AST.CInitializer a)
deriving instance Generic (Language.C.Syntax.AST.CPartDesignator a)
deriving instance Generic (Language.C.Syntax.AST.CStatement a)
deriving instance Generic (Language.C.Syntax.AST.CStorageSpecifier a)
deriving instance Generic (Language.C.Syntax.AST.CStringLiteral a)
deriving instance Generic (Language.C.Syntax.AST.CStructureUnion a)
deriving instance Generic (Language.C.Syntax.AST.CTranslationUnit a)
deriving instance Generic (Language.C.Syntax.AST.CTypeQualifier a)
deriving instance Generic (Language.C.Syntax.AST.CTypeSpecifier a)
deriving instance Generic (Language.C.Syntax.Constants.Flags a)
deriving instance Generic Language.C.Data.Ident.Ident
deriving instance Generic Language.C.Data.Name.Name
deriving instance Generic Language.C.Data.Node.NodeInfo
deriving instance Generic Language.C.Syntax.AST.CAssignOp
deriving instance Generic Language.C.Syntax.AST.CBinaryOp
deriving instance Generic Language.C.Syntax.AST.CStructTag
deriving instance Generic Language.C.Syntax.AST.CUnaryOp
deriving instance Generic Language.C.Syntax.Constants.CChar
deriving instance Generic Language.C.Syntax.Constants.CFloat
deriving instance Generic Language.C.Syntax.Constants.CIntFlag
deriving instance Generic Language.C.Syntax.Constants.CIntRepr
deriving instance Generic Language.C.Syntax.Constants.CInteger
deriving instance Generic Language.C.Syntax.Constants.CString
deriving instance NFData Language.C.Data.Ident.Ident
deriving instance NFData Language.C.Data.Name.Name
deriving instance NFData Language.C.Data.Node.NodeInfo
deriving instance NFData Language.C.Syntax.AST.CAssignOp
deriving instance NFData Language.C.Syntax.AST.CBinaryOp
deriving instance NFData Language.C.Syntax.AST.CStructTag
deriving instance NFData Language.C.Syntax.AST.CUnaryOp
deriving instance NFData Language.C.Syntax.Constants.CChar
deriving instance NFData Language.C.Syntax.Constants.CFloat
deriving instance NFData Language.C.Syntax.Constants.CIntFlag
deriving instance NFData Language.C.Syntax.Constants.CIntRepr
deriving instance NFData Language.C.Syntax.Constants.CInteger
deriving instance NFData Language.C.Syntax.Constants.CString

{-
instance Generic Position where
  type Rep Position =
    (((Rec0 Int :*: Rec0 String) :*: (Rec0 Int :*: Rec0 Int))
    :+: U1) :+: (U1 :+: U1)

--   type 'Rep' (Tree a) =
--     'Rec0' a
--     ':+:'
--     ('Rec0' (Tree a) ':*:' 'Rec0' (Tree a))

  from pos
    | Position.isSourcePos pos
    = let
    offset = Position.posOffset pos
    file = Position.posFile pos
    row = Position.posRow pos
    column = Position.posColumn pos
    in L1 $ L1 $ (K1 offset :*: K1 file) :*: (K1 row :*: K1 column)

    | Position.isNoPos pos
    = let
    in L1 $ R1 U1

    | Position.isBuiltinPos pos
    = R1 $ L1 U1

    | Position.isInternalPos pos
    = R1 $ R1 U1

    | otherwise = error "Generic instance for Position is wrong"

  to
    (L1 (L1 ((K1 offset :*: K1 file) :*: (K1 row :*: K1 column))))
    = Position.position offset file row column

  to (L1 (R1 U1)) = Position.nopos
  to (R1 (L1 U1)) = Position.builtinPos
  to (R1 (R1 U1)) = Position.internalPos
-}

instance NFData Position
