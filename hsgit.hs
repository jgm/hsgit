{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import Bindings.Libgit2
import Data.Typeable

data GitError = GitError String String
               deriving (Show,Typeable)

raiseGitError :: String -> CInt -> IO a
raiseGitError msg rc =
  c'git_strerror rc >>= peekCString >>= throw . GitError msg

instance Exception GitError

newtype Repo = Repo (Ptr C'git_repository)

newtype Object = Object (Ptr C'git_object)

newtype ObjectType = ObjectType CInt deriving (Eq)

instance Show ObjectType where
  show (ObjectType x) = unsafePerformIO $
    c'git_object_type2string x >>= peekCString

commitObject, treeObject :: ObjectType
blobObject = ObjectType c'GIT_OBJ_BLOB
commitObject = ObjectType c'GIT_OBJ_COMMIT
treeObject = ObjectType c'GIT_OBJ_TREE

newtype OID = OID (Ptr C'git_oid)

instance Eq OID where
  OID x == OID y = unsafePerformIO (c'git_oid_cmp x y) == 0

instance Show OID where
  show (OID x) = unsafePerformIO $ do
    str <- newCString $ replicate 41 '\0' -- the 41st is the null termination
    c'git_oid_fmt str x                   -- not provided by git_oid_fmt
    peekCString str

openRepo :: FilePath -> IO Repo
openRepo fp = alloca $ \ptr -> do
  fp' <- newCString fp
  rc <- c'git_repository_open ptr fp'
  if rc < 0
     then raiseGitError ("Cannot open repository " ++ fp) rc
     else Repo `fmap` peek ptr

freeRepo :: Repo -> IO ()
freeRepo (Repo repo) = c'git_repository_free repo

withRepo :: FilePath -> (Repo -> IO a) -> IO a
withRepo fp action = do
  repo <- openRepo fp
  val <- action repo
  freeRepo repo
  return val

mkOID :: String -> OID
mkOID raw = unsafePerformIO $ alloca $ \ptr -> do
  idstr <- newCString raw
  rc <- c'git_oid_mkstr ptr idstr
  if rc < 0
     then raiseGitError ("Cannot make OID with " ++ raw) rc
     else return $ OID ptr

lookupObject :: Repo -> OID -> ObjectType -> IO Object
lookupObject (Repo repo) (OID oid) (ObjectType typ) = alloca $ \ptr -> do
  rc <- c'git_object_lookup ptr repo oid typ
  if rc < 0
     then raiseGitError ("Cannot find " ++ show (ObjectType typ) ++
              " with OID " ++ show (OID oid)) rc
     else Object `fmap` peek ptr

closeObject :: Object -> IO ()
closeObject (Object obj) = c'git_object_close obj

withObject :: Repo -> OID -> ObjectType -> (Object -> IO a) -> IO a
withObject repo oid typ f = do
  obj <- lookupObject repo oid typ
  res <- f obj
  closeObject obj
  return res

objectType :: Object -> ObjectType
objectType (Object x) = unsafePerformIO $ ObjectType `fmap` c'git_object_type x

main = do
  withRepo "test.git" $ \repo -> do
    let oid1 = mkOID "10754a36c7e1e2b3cdf9d763a9e78ac35bcb56cc"
    let oid2 = mkOID "10754a36c7e1e2b3cdf9d763a9e78ac35bcb56cc"
    let oid3 = mkOID "90754a36c7e1e2b3cdf9d763a9e78ac35bcb56cc"
    print (oid1 == oid2)
    print (oid2 == oid3)
    print oid2
    o1 <- lookupObject repo oid2 commitObject
    withObject repo oid2 commitObject $ \obj -> print (objectType obj)

