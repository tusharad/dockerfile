{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

-- | A Simple DSL for describing and generating Dockerfiles in Haskell
--
-- Compatible w/ Docker v18.03
--
-- @
-- main :: IO ()
-- main = putStrLn $
--   dockerfile $ do
--       from "debian:stable"
--       maintainer "creichert <creichert07@gmail.com>"
--       run "apt-get -y update"
--       run "apt-get -y upgrade"
--       cmd [ "echo", "hello world"]
-- @


module Data.Docker
       (
         -- * Types
         Docker
       , AddOpt (..)
       , CopyOpt(..)
       , FromOpt (..)
       , HealthCheckOpt (..)
       , RunOpt (..)
       , dockerfile
       , dockerfileWrite

         -- * Docker Instructions
       , from
       , fromAs
       , run
       , cmd
       , label
       , maintainer
       , expose
       , env
       , envs
       , add
       , addWithWhiteSpaces
       , copy
       , copyfrom
       , copychown
       , entrypoint
       , volume
       , user
       , workdir
       , arg
       , onbuild
       , stopsignal
       , healthcheck
       , shell
       ) where

import Control.Monad.Writer.Lazy


-- | A 'Dockerfile' is represented as a list of instructions.
type DockerFile = [Instruction]

-- | A 'Docker' writer monad for creating 'Dockerfile's
type Docker a = Writer DockerFile a

-- | Render a list of 'Docker' instructions to a 'String'.
dockerfile :: Docker a -> String
dockerfile = unlines . map prettyCmd . execWriter

-- | Render a list of 'Docker' instructions to a 'FilePath'.
dockerfileWrite :: FilePath -> Docker a -> IO ()
dockerfileWrite fp docker = do
    let content = "# this file was generated by the `dockerfile` haskell library"
                : ""
                : fmap prettyCmd (execWriter docker)
    writeFile fp (unlines content)

type Script     = String
type ScriptFile = FilePath
type Param      = String
type ImageName  = String
type As         = String
data Protocol   = TCP | UDP

instance Show Protocol where
  show TCP = "tcp"
  show UDP = "udp"

-- | Dockerfile instruction set
--
-- This data type is not exposed. All Docker commands/instructions are
-- exposed through combinator functions intended to be run from w/in
-- `dockerfile` and similar functions.
data Instruction

  = From ImageName (Maybe As) (Maybe FromOpt)
  | Run Script [RunOpt]  -- File [ScriptParam]
  | Cmd [ ScriptFile ]
  | Label [(String, String)]
  | Maintainer String
  | Expose Int (Maybe Protocol)
  | Env String String
  | Envs [(String, String)]
  | Add [FilePath] FilePath [AddOpt]
  | AddWithWhiteSpaces [FilePath] FilePath [AddOpt]
  | Copy [FilePath] FilePath [CopyOpt]
  | Entrypoint String [Param]
  | Volume [FilePath]
  | User String
  | WorkDir FilePath
  | Arg String (Maybe String)
  | OnBuild Instruction
  | StopSignal String
  | HealthCheck (Maybe ([HealthCheckOpt], String))
  | Shell [String]
  deriving Show

prettyCmd :: Instruction -> String
prettyCmd = \case
    From f mas mbPlatform          -> "FROM " ++ maybe "" renderDockerOpt mbPlatform ++ f ++ maybe "" (" AS " ++) mas
    Run scr opts                   -> "RUN " ++ renderOpts opts ++ (if null opts then "" else " ") ++ scr
    Cmd cmds                       -> "CMD " ++ show cmds
    Label kvs                      -> "LABEL " ++ unwords (fmap (\(k,v) -> show k ++ "=" ++ show v) kvs)
    Maintainer m                   -> "MAINTAINER " ++ m
    Expose p mbProtocol            -> "EXPOSE " ++ show p ++ maybe "" (\x -> "/" ++ show x) mbProtocol
    Env k v                        -> "ENV " ++ k ++ " " ++ v
    Envs kvs                       -> "ENV " ++ unwords (fmap (\(k,v) -> show k ++ "=" ++ show v) kvs)
    Add s d opts                   -> "ADD " ++ (if null opts then "" else renderOpts opts ++ " ")
                                             ++ unwords s
                                             ++ " "
                                             ++ d
    AddWithWhiteSpaces s d opts    -> "ADD " ++ (if null opts then "" else renderOpts opts ++ " ")
                                             ++ show (s ++ [d])
    Copy s d opts                  -> "COPY " ++ (if null opts then "" else renderOpts opts ++ " ")
                                              ++ unwords s
                                              ++ " "
                                              ++ d
    Entrypoint e ps                -> "ENTRYPOINT " ++ show (e:ps)
    Volume vs                      -> "VOLUME " ++ show vs
    User u                         -> "USER " ++ u
    WorkDir cwd                    -> "WORKDIR " ++ cwd
    Arg name mval                  -> "ARG " ++ name ++ maybe "" ("=" ++) mval
    OnBuild instr                 -> case instr of
                                        (OnBuild _) -> error "OnBuild on OnBuild is not supported"
                                        (From {}) -> error "From in OnBuild may not be supported"
                                        (Maintainer {}) -> error "MAINTAINER in OnBuild may not be supoprted"
                                        _ -> "ONBUILD " ++ prettyCmd instr
    StopSignal sig                 -> "STOPSIGNAL " ++ sig
    HealthCheck (Just (opts, c))   -> "HEALTHCHECK " ++ renderOpts opts ++ " CMD " ++ c
    HealthCheck Nothing            -> "HEALTHCHECK NONE"
    Shell cmds                     -> "SHELL " ++ show cmds

class DockerOpt a where
    renderDockerOpt :: a -> String

data CopyOpt = CopyOptFrom String
             | CopyOptChown [String]
             | CopyOptChmod String
             | CopyOptLink
             | CopyOptParents 
             | CopyOptExclude FilePath
             deriving Show

instance DockerOpt CopyOpt where
    renderDockerOpt = \case
        CopyOptFrom n       -> "--from=" ++ n
        CopyOptChown chowns -> unwords (fmap ("--chown=" ++) chowns)
        CopyOptChmod chmod -> "--chmod=" ++ chmod
        CopyOptLink -> "--link"
        CopyOptExclude filePath -> "--exclude=" ++ filePath
        CopyOptParents -> "--parents"

-- | The --chown and --chmod features are only supported on Dockerfiles used to build Linux containers
data AddOpt = AddOptChown [String]
            | AddOptChmod String
            | AddOptKeepGitDir Bool
            | AddOptChecksum String
            | AddOptLink
            | AddOptExclude FilePath
            deriving Show

instance DockerOpt AddOpt where
    renderDockerOpt = \case
        AddOptChown chowns -> unwords (fmap ("--chown=" ++) chowns)
        AddOptChmod chmod -> "--chmod=" ++ chmod
        AddOptKeepGitDir bool -> "--keep-git-dir=" ++ (if bool then "true" else "false")
        AddOptChecksum sha -> "--checksum=" ++ sha
        AddOptLink -> "--link"
        AddOptExclude filePath -> "--exclude=" ++ filePath

newtype FromOpt = FromOptPlatform String
    deriving Show

instance DockerOpt FromOpt where
    renderDockerOpt (FromOptPlatform platform) = "--platform=" ++ platform

data HealthCheckOpt = HealthCheckOptInterval String
                    | HealthCheckOptTimeout String
                    | HealthCheckOptStartPeriod String
                    | HealthCheckOptStartInterval String
                    | HealthCheckOptRetires Int
    deriving Show

instance DockerOpt HealthCheckOpt where
    renderDockerOpt = \case
        HealthCheckOptInterval duration -> "--interval=" ++ duration
        HealthCheckOptTimeout duration -> "--timeout=" ++ duration
        HealthCheckOptStartPeriod duration -> "--start-period=" ++ duration
        HealthCheckOptStartInterval duration -> "--start-interval=" ++ duration
        HealthCheckOptRetires retires -> "--retires=" ++ show retires

data RunOpt = RunOptMount String
            | RunOptNetwork String
            | RunOptSecurity String
    deriving Show

instance DockerOpt RunOpt where
    renderDockerOpt = \case
        RunOptMount mount -> "--mount=" ++ mount
        RunOptNetwork network -> "--network=" ++ network
        RunOptSecurity sec -> "--security=" ++ sec

renderOpts :: DockerOpt a => [a] -> String
renderOpts = unwords . fmap renderDockerOpt

-- * Instructions

-- | Note: Image name should contain version as well
from :: String -> Maybe FromOpt -> Docker ()
from f mbPlatform = tell [ From f Nothing mbPlatform ]

fromAs :: String -> As -> Maybe FromOpt -> Docker ()
fromAs f as mbPlatform = tell [ From f (Just as) mbPlatform]

run :: Script -> [RunOpt] -> Docker ()
run scr opts = tell [ Run scr opts]

cmd :: [ScriptFile] -> Docker ()
cmd cs = tell [ Cmd cs ]

label :: [(String, String)] -> Docker ()
label kvs = tell [ Label kvs ]

maintainer :: String -> Docker ()
maintainer m = tell [ Maintainer m ]

expose :: Int -> Maybe Protocol -> Docker ()
expose p mbProtocol = tell [ Expose p mbProtocol]

env :: String -> String -> Docker ()
env k v = tell [ Env k v ]

envs :: [(String, String)] -> Docker ()
envs lst = tell [Envs lst]

add :: [FilePath] -> FilePath -> [AddOpt] -> Docker ()
add k v mbOpts = tell [ Add k v mbOpts ]

addWithWhiteSpaces :: [FilePath] -> FilePath -> Docker ()
addWithWhiteSpaces srcList dest = tell [AddWithWhiteSpaces srcList dest []]

copy :: [FilePath] -> FilePath -> Docker ()
copy s d = tell [ Copy s d [] ]

copyfrom :: String -> [FilePath] -> FilePath -> Docker ()
copyfrom frm s d = tell [ Copy s d [CopyOptFrom frm] ]

copychown :: [String] -> [FilePath] -> FilePath -> Docker ()
copychown chowns s d = tell [ Copy s d [CopyOptChown chowns] ]

entrypoint :: FilePath -> [Param] -> Docker ()
entrypoint e ps = tell [ Entrypoint e ps ]

volume :: [FilePath] -> Docker ()
volume vs  = tell [ Volume vs ]

user :: String -> Docker ()
user u = tell [ User u ]

workdir :: FilePath -> Docker ()
workdir cwd = tell [ WorkDir cwd ]

arg :: String -> Maybe String -> Docker ()
arg name val = tell [ Arg name val ]

stopsignal :: String -> Docker ()
stopsignal c = tell [StopSignal c]

healthcheck :: Maybe ([HealthCheckOpt], String) -> Docker ()
healthcheck = \case
    Just (opts, cmd') -> tell [HealthCheck (Just (opts, cmd'))]
    Nothing           -> tell [HealthCheck Nothing]

shell :: [String] -> Docker ()
shell cmds = tell [Shell cmds]

onbuild :: Docker () -> Docker ()
onbuild instr = tell [OnBuild (head (execWriter instr))]
