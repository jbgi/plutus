{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}







-- This module defines the core types of a unification elaborator.

module PlutusCore.Elaborator where

import Utils.ABT
--import Utils.Env
--import Utils.Names
import Utils.Pretty
import qualified Utils.ProofDeveloper as PD
--import Utils.Unifier
--import Utils.Vars
import PlutusCore.ElabError
import PlutusCore.LanguageOptions
import PlutusCore.Program
import PlutusCore.Term
import PlutusCore.Judgments

--import qualified Control.Lens as L
--import Control.Monad.State















type Decomposer = PD.Decomposer LanguageOptions ElabError Judgment

type Elaborator = PD.Elaborator LanguageOptions ElabError Judgment

type TypeChecker = Elaborator

runElaborator :: LanguageOptions
              -> Elaborator a
              -> Either (PD.ElabError LanguageOptions ElabError Judgment)
                        a
runElaborator opts e =
  fst <$> (PD.runElaborator e [] opts)




instance PD.ShowElabError LanguageOptions ElabError Judgment where
  showElabError (PD.ElabError err _ ctx0 (PD.Any g0)) =
     "Could not prove " ++ prettyJudgment g0
      ++ "\nError message: " ++ prettyElabError err
      ++ "\nContext:" ++ go ctx0
    where
      go :: PD.Context (PD.Any Judgment) -> String
      go [] = ""
      go ((_,PD.Any (ElabProgramJ _)):gs) =
        go gs
      go ((i,PD.Any g):gs) =
        "\n\n  In subproblem " ++ show i ++ " of "
          ++ prettyJudgment g ++ go gs
      
      prettyJudgment :: Judgment r -> String
      prettyJudgment (ElabProgramJ _) =
        "<program>"
      prettyJudgment (ElabDeclJ _ decl) =
        case decl of
          DataDeclaration n _ _ ->
            "the data declaration for `" ++ n ++ "`"
          TypeDeclaration n _ ->
            "the type declaration for `" ++ n ++ "`"
          TermDeclaration n _ ->
            "the term declaration for `" ++ n ++ "`"
          TermDefinition n _ ->
            "the term definition for `" ++ n ++ "`"
      prettyJudgment (ElabAltJ _ (Alt c _) _) =
        "the constructor alternative for `" ++ c ++ "`"
      prettyJudgment (IsTypeJ _ a) =
        "checking that `"
          ++ pretty a
          ++ "` is a type"
      prettyJudgment (IsTypeValueJ a) =
        "checking that `"
          ++ pretty a
          ++ "` is a type value"
      prettyJudgment (IsTermValueJ m) =
        "checking that `"
          ++ pretty m
          ++ "` is a term value"
      prettyJudgment (SynthJ _ m) =
        "synthesizing the type of the program `" ++ pretty m ++ "`"
      prettyJudgment (CheckJ _ a m) =
        "checking that the type `" ++ pretty a
        ++ "` contains the program `" ++ pretty m ++ "`"
      prettyJudgment (ClauseJ _ _ _ _ (Clause c :$: _)) =
        "checking the clause for `"
        ++ c
        ++ "`"
      prettyJudgment (ClauseJ _ _ _ _ _) =
        error "Tried to check that a non-clause was a well-formed clause.\
              \ This should be impossible to reach."
      prettyJudgment (EqualJ _ a b) =
        "enforcing the equality of `"
        ++ pretty a
        ++ "` and `"
        ++ pretty b
        ++ "`"
      prettyJudgment (EqualAllJ _ a bs) =
        "enforcing the equality of `"
        ++ pretty a
        ++ "` and all of `"
        ++ unwords [ "`" ++ pretty b ++ "`" | b <- bs ]
        ++ "`"


instance PD.Metas LanguageOptions Judgment where
  substitute _ j = j