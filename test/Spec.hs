
import Control.Monad (forM_)
import Test.Hspec

import Data.Docker


main :: IO ()
main = hspec $ do

  describe "Data.Docker" $ do

    it "sanity checks test-suite" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    let tsts :: [(String, Docker (), String)]
        tsts = [
            ("FROM", from "ubuntu:trusty", "FROM ubuntu:trusty")
          , ("FROM .. AS .. ", fromas "ubuntu:trusty" "base", "FROM ubuntu:trusty AS base")
          , ("RUN shell form", run "echo hi", "RUN echo hi")
          , ("CMD exec form", cmd ["bash"], "CMD [\"bash\"]")
          , ("LABEL exec form", label [("version","v1.0")], "LABEL \"version\"=\"v1.0\"")
          , ("MAINTAINER", maintainer "Chris <chris@rbros.com>" , "MAINTAINER Chris <chris@rbros.com>")
          , ("EXPOSE", expose 3000, "EXPOSE 3000")
          , ("ENV", env "DEBIAN_FRONTEND" "noninteractive" , "ENV DEBIAN_FRONTEND noninteractive")
          , ("ADD", add ["package.yaml"] "/" [], "ADD package.yaml /")
          , ("ADD with owenership flags", add ["package.yaml"] "/" [AddOptChmod "644", AddOptChown ["cr:cr"]]
                                                    , "ADD --chmod=644 --chown=cr:cr package.yaml /")
          , ("ADD with keep git dir flag", add ["package.yaml"] "/" [AddOptKeepGitDir True]
                                                    , "ADD --keep-git-dir=true package.yaml /")
          , ("ADD with exclude, link & checksum flag"
                    , add ["package.yaml"] "/" [
                                                AddOptExclude "./someFile.txt"
                                              , AddOptLink 
                                              , AddOptChecksum "sha256:15481119c43b3cafd48dd869a9b2945d1036d1dc68d"
                                              ]
              , "ADD --exclude=./someFile.txt --link --checksum=sha256:15481119c43b3cafd48dd869a9b2945d1036d1dc68d package.yaml /")
          , ("ADD with white spaces", addWithWhiteSpaces ["hello world.txt"] "/app", "ADD [\"hello world.txt\",\"/app\"]")
          , ("COPY ", copy ["package.yaml"] "/", "COPY package.yaml /")
          , ("COPY --from", copyfrom "ci" ["file1.txt"] "/", "COPY --from=ci file1.txt /")
          , ("COPY --chown", copychown ["cr:cr", "1"] ["./package.yaml"] "/", "COPY --chown=cr:cr --chown=1 ./package.yaml /")

          , ("ENTRYPOINT", entrypoint "bash" ["/opt/custom.sh"], "ENTRYPOINT [\"bash\",\"/opt/custom.sh\"]")
          , ("VOLUME", volume ["/myvol"], "VOLUME [\"/myvol\"]")

          , ("USER", user "pat:wheels", "USER pat:wheels")
          , ("WORKDIR", workdir "/", "WORKDIR /")

          , ("ARG", arg "node_version" Nothing, "ARG node_version")
          , ("ARG def", arg "node_version" (Just "9.7.2"), "ARG node_version=9.7.2")

          , ("STOPSIGNAL", stopsignal "9", "STOPSIGNAL 9")

          , ("HEALTHCHECK NONE", healthcheck Nothing, "HEALTHCHECK NONE")
          , ("HEALTHCHECK", healthcheck (Just (["--interval=5m"], "curl -f http://localhost || exit 1")), "HEALTHCHECK --interval=5m CMD curl -f http://localhost || exit 1")

          -- TODO "SHELL"
          -- TODO "ONBUILD"
          ]

    forM_ tsts $ \(name, instr, fixture) -> do
        it ("supports " <> name <> " instruction") $ do
            dockerfile instr `shouldBe` (fixture <> "\n")
