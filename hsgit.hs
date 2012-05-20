{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
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

data Commit = Commit { commitId           :: OID
                     , commitMessage      :: String
                     , commitMessageShort :: String
                     , commitTime         :: UTCTime
                     , commitCommitter    :: Signature
                     , commitAuthor       :: Signature
                     , commitParents      :: [Commit]
                     , commitTree         :: Tree
                     } deriving Show

data Signature = Signature { signatureName   :: String
                           , signatureEmail  :: String
                           , signatureWhen   :: UTCTime
                           } deriving Show

data Tree = Tree { treeId              :: OID
                 , treeEntries         :: [Entry]
                 } deriving Show

data Entry = Entry { entryId         :: OID
                   , entryAttributes :: CUInt
                   , entryName       :: FilePath
                   } deriving Show

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

initRepo :: Bool -> FilePath -> IO Repo
initRepo bare fp = alloca $ \ptr -> do
  fp' <- newCString fp
  rc <- c'git_repository_init ptr fp' (if bare then 0 else 1)
  if rc < 0
     then raiseGitError ("Cannot initialize repository " ++ fp) rc
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

lookupCommit :: Repo -> OID -> IO Commit
lookupCommit (Repo repo) (OID oid) = alloca $ \ptr -> do
  rc <- c'git_commit_lookup ptr repo oid
  if rc < 0
     then raiseGitError ("Cannot find commit with OID " ++ show (OID oid)) rc
     else peek ptr >>= toCommit

toCommit :: Ptr (C'git_commit) -> IO Commit
toCommit commit = do
       id' <- c'git_commit_id commit
       time <- (posixSecondsToUTCTime . realToFrac) `fmap`
                  c'git_commit_time commit
       short <- c'git_commit_message_short commit >>= peekCString
       msg <- c'git_commit_message commit >>= peekCString
       committer <- c'git_commit_committer commit >>= peek >>= toSignature
       author <- c'git_commit_author commit >>= peek >>= toSignature
       numparents <- c'git_commit_parentcount commit
       let getParent n = alloca $ \par -> do
             rc <- c'git_commit_parent par commit n
             if rc < 0
                then raiseGitError ("Cannot get parent " ++ show n ++
                        " of commit " ++ show (OID id')) rc
                else peek par >>= toCommit
       parents <- mapM getParent (take (fromIntegral numparents) [0..])
       alloca $ \treePtr -> do
         rc' <- c'git_commit_tree treePtr commit
         tree <- if rc' < 0
                    then raiseGitError ("Cannot get tree from commit " ++
                                show (OID id')) rc'
                    else peek treePtr >>= toTree
         return Commit { commitId           = OID id'
                       , commitMessage      = msg
                       , commitMessageShort = short
                       , commitTime         = time
                       , commitCommitter    = committer
                       , commitAuthor       = author
                       , commitParents      = parents
                       , commitTree         = tree
                       }

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

toSignature :: C'git_signature -> IO Signature
toSignature (C'git_signature name email (C'git_time time _offset)) = do
  name' <- peekCString name
  email' <- peekCString email
  let time' = posixSecondsToUTCTime $ realToFrac time
  return   Signature{ signatureName = name'
                    , signatureEmail = email'
                    , signatureWhen = time' }


{-
makeTree :: Repo -> IO Tree
makeTree (Repo repo) = alloca $ \ptr -> do
  rc <- c'git_tree_new ptr repo
  if rc < 0
     then raiseGitError "Cannot make tree" rc
     else peek ptr >>= toTree
-}



toTree :: Ptr (C'git_tree) -> IO Tree
toTree ptr = do
  id' <- c'git_tree_id ptr
  entrycount <- fromIntegral `fmap` c'git_tree_entrycount ptr
  entries <- mapM (getEntry ptr) (take entrycount [0..])
  return Tree { treeId              = OID id'
              , treeEntries         = entries
              }

getEntry :: Ptr (C'git_tree) -> CInt -> IO Entry
getEntry ptr idx = do
  entry <- c'git_tree_entry_byindex ptr idx
  id' <- c'git_tree_entry_id entry
  attr <- c'git_tree_entry_attributes entry
  name <- c'git_tree_entry_name entry >>= peekCString
  return Entry{ entryId         = OID id'
              , entryAttributes = attr
              , entryName       = name
              }

main = do
  withRepo "test.git" $ \repo -> do
    let oid1 = mkOID "10754a36c7e1e2b3cdf9d763a9e78ac35bcb56cc"
    let oid2 = mkOID "49d40209afd138a37215915e51bed9e8079ba2fc"
    let oid3 = mkOID "90754a36c7e1e2b3cdf9d763a9e78ac35bcb56cc"
    print (oid1 == oid2)
    print (oid2 == oid3)
    print oid2
    o1 <- lookupObject repo oid2 commitObject
    o2 <- lookupCommit repo oid2
    print o2
    -- withObject repo oid2 commitObject $ \obj -> print (objectType obj)
    -- initRepo True "foo.git"
    return ()
