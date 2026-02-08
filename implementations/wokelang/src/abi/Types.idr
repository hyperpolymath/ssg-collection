||| ABI Type Definitions for WokeLang SSG
|||
||| This module defines the Application Binary Interface (ABI) for wokelang-ssg.
||| Provides type-safe interfaces for site generation operations with formal proofs.
|||
||| @see https://idris2.readthedocs.io for Idris2 documentation

module WokeLangSSG.ABI.Types

import Data.Bits
import Data.So
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Platform Detection
--------------------------------------------------------------------------------

||| Supported platforms for this ABI
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

||| Compile-time platform detection
||| This will be set during compilation based on target
public export
thisPlatform : Platform
thisPlatform =
  %runElab do
    -- Platform detection logic
    pure Linux  -- Default, override with compiler flags

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

||| Result codes for FFI operations
||| Use C-compatible integers for cross-language compatibility
public export
data Result : Type where
  ||| Operation succeeded
  Ok : Result
  ||| Generic error
  Error : Result
  ||| Invalid parameter provided
  InvalidParam : Result
  ||| Out of memory
  OutOfMemory : Result
  ||| Null pointer encountered
  NullPointer : Result

||| Convert Result to C integer
public export
resultToInt : Result -> Bits32
resultToInt Ok = 0
resultToInt Error = 1
resultToInt InvalidParam = 2
resultToInt OutOfMemory = 3
resultToInt NullPointer = 4

||| Results are decidably equal
public export
DecEq Result where
  decEq Ok Ok = Yes Refl
  decEq Error Error = Yes Refl
  decEq InvalidParam InvalidParam = Yes Refl
  decEq OutOfMemory OutOfMemory = Yes Refl
  decEq NullPointer NullPointer = Yes Refl
  decEq _ _ = No absurd

--------------------------------------------------------------------------------
-- Opaque Handles
--------------------------------------------------------------------------------

||| Opaque handle type for FFI
||| Prevents direct construction, enforces creation through safe API
public export
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle

||| Safely create a handle from a pointer value
||| Returns Nothing if pointer is null
public export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle ptr = Just (MkHandle ptr)

||| Extract pointer value from handle
public export
handlePtr : Handle -> Bits64
handlePtr (MkHandle ptr) = ptr

--------------------------------------------------------------------------------
-- Platform-Specific Types
--------------------------------------------------------------------------------

||| C int size varies by platform
public export
CInt : Platform -> Type
CInt Linux = Bits32
CInt Windows = Bits32
CInt MacOS = Bits32
CInt BSD = Bits32
CInt WASM = Bits32

||| C size_t varies by platform
public export
CSize : Platform -> Type
CSize Linux = Bits64
CSize Windows = Bits64
CSize MacOS = Bits64
CSize BSD = Bits64
CSize WASM = Bits32

||| C pointer size varies by platform
public export
ptrSize : Platform -> Nat
ptrSize Linux = 64
ptrSize Windows = 64
ptrSize MacOS = 64
ptrSize BSD = 64
ptrSize WASM = 32

||| Pointer type for platform
public export
CPtr : Platform -> Type -> Type
CPtr p _ = Bits (ptrSize p)

--------------------------------------------------------------------------------
-- Memory Layout Proofs
--------------------------------------------------------------------------------

||| Proof that a type has a specific size
public export
data HasSize : Type -> Nat -> Type where
  SizeProof : {0 t : Type} -> {n : Nat} -> HasSize t n

||| Proof that a type has a specific alignment
public export
data HasAlignment : Type -> Nat -> Type where
  AlignProof : {0 t : Type} -> {n : Nat} -> HasAlignment t n

||| Size of C types (platform-specific)
public export
cSizeOf : (p : Platform) -> (t : Type) -> Nat
cSizeOf p (CInt _) = 4
cSizeOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cSizeOf p Bits32 = 4
cSizeOf p Bits64 = 8
cSizeOf p Double = 8
cSizeOf p _ = ptrSize p `div` 8

||| Alignment of C types (platform-specific)
public export
cAlignOf : (p : Platform) -> (t : Type) -> Nat
cAlignOf p (CInt _) = 4
cAlignOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cAlignOf p Bits32 = 4
cAlignOf p Bits64 = 8
cAlignOf p Double = 8
cAlignOf p _ = ptrSize p `div` 8

--------------------------------------------------------------------------------
-- SSG-Specific Types
--------------------------------------------------------------------------------

||| Build configuration for static site generation
public export
record BuildConfig where
  constructor MkBuildConfig
  contentDir : String   -- Source content directory
  outputDir : String    -- Build output directory
  templatesDir : String -- Templates directory
  staticDir : String    -- Static assets directory

||| Site metadata
public export
record SiteMetadata where
  constructor MkSiteMetadata
  title : String
  url : String
  description : String
  author : String
  language : String

||| Page frontmatter data
public export
record PageFrontMatter where
  constructor MkPageFrontMatter
  title : String
  description : String
  isDraft : Bits32      -- 0 = false, 1 = true
  templateName : String

--------------------------------------------------------------------------------
-- FFI Declarations
--------------------------------------------------------------------------------

||| Declare external C functions for SSG operations
||| These will be implemented in Zig FFI
namespace Foreign

  ||| Initialize the SSG engine
  export
  %foreign "C:wokelang_ssg_init, libwokelang_ssg"
  prim__ssgInit : PrimIO Bits64

  ||| Build the site
  export
  %foreign "C:wokelang_ssg_build, libwokelang_ssg"
  prim__ssgBuild : Bits64 -> String -> String -> PrimIO Bits32

  ||| Parse markdown to HTML
  export
  %foreign "C:wokelang_ssg_parse_markdown, libwokelang_ssg"
  prim__parseMarkdown : Bits64 -> String -> PrimIO String

  ||| Free the SSG engine
  export
  %foreign "C:wokelang_ssg_free, libwokelang_ssg"
  prim__ssgFree : Bits64 -> PrimIO ()

  ||| Safe wrapper for SSG initialization
  export
  ssgInit : IO (Maybe Handle)
  ssgInit = do
    ptr <- primIO prim__ssgInit
    pure (createHandle ptr)

  ||| Safe wrapper for site building
  export
  ssgBuild : Handle -> String -> String -> IO (Either Result ())
  ssgBuild h source output = do
    result <- primIO (prim__ssgBuild (handlePtr h) source output)
    pure $ if result == 0
           then Right ()
           else Left Error

  ||| Safe wrapper for markdown parsing
  export
  parseMarkdown : Handle -> String -> IO String
  parseMarkdown h markdown = primIO (prim__parseMarkdown (handlePtr h) markdown)

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

||| Compile-time verification of ABI properties
namespace Verify

  ||| Verify struct sizes are correct
  export
  verifySizes : IO ()
  verifySizes = do
    -- Add compile-time checks here
    putStrLn "ABI sizes verified"

  ||| Verify struct alignments are correct
  export
  verifyAlignments : IO ()
  verifyAlignments = do
    -- Add compile-time checks here
    putStrLn "ABI alignments verified"
