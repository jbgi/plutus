{ runCommand, hlint, src, lib }:

let
  # just haskell sources and the hlint config file
  src' = lib.cleanSourceWith {
   inherit src;
   filter = with lib;
    name: type: let baseName = baseNameOf (toString name); in (
      (type == "regular" && hasSuffix ".hs" baseName) ||
      (type == "regular" && hasSuffix ".yaml" baseName) ||
      (type == "directory")
    );
  };
in
# we don't include plutus-prototype since it's old and nobody is interested in fixing all the hints
runCommand "plutus-hlint-check" { buildInputs = [ hlint ]; } ''
  set +e
  projects=("core-to-plc" "language-plutus-core" "plutus-core-interpreter" "plutus-exe" "plutus-th" "plutus-use-cases" "wallet-api")
  cd ${src'}
  hlint "''${projects[@]}"
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    echo '====================================================================='
    echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
    echo 'this in the source file:'
    tput bold
    echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
    tput sgr0
    echo 'You can also apply it just to a particular function, which is better:'
    tput bold
    echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
    tput sgr0
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
