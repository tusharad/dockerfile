#+AUTHOR: Christopher Reichert
#+TITLE: Haskell Dockerfile DSL

[[https://travis-ci.org/creichert/dockerfile][https://travis-ci.org/creichert/dockerfile.svg?branch=master]]

A simple Dockerfile DSL in Haskell

#+BEGIN_SRC haskell

{-# LANGUAGE OverloadedStrings #-}

import Data.Docker

main :: IO ()
main = do
  let df = dockerfile $ do
             from "debian:stable"
             maintainer "Christopher Reichert <creichert07@gmail.com>"
             run "apt-get -y update"
             run "apt-get -y upgrade"
             cmd ["echo", "hello world"]

  putStrLn df
#+END_SRC


** Motivation


- Type Safety


  Eliminates common errors and syntax mistakes.

  /More work to be done here, see Haddock comments/


- Composable

  Dockerfiles can be more easily composed and manipulated, making it
  easier to integrate into programs.

  =postgresDockerFile >> postgisDockerFile >> otherDockerFile=


- Support for "monadic" computation (mapM,forM,replicateM,etc)

