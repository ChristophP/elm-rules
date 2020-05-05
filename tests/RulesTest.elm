module RulesTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Rules
import Test exposing (..)


matchers : Test
matchers =
    describe "Matchers"
        [ describe "pass"
            [ test "matches always" <|
                \() ->
                    Rules.matchAlways ()
                        |> Expect.equal True
            ]
        , describe "not"
            [ test "inverts the outcome of the matcher" <|
                \() ->
                    Rules.matchNot (always True) ()
                        |> Expect.equal False
            ]
        , describe "all"
            [ test "true when all matchers are true" <|
                \() ->
                    Rules.matchAll [ always True, always True ] ()
                        |> Expect.equal True
            , test "false when at least one matcher is false" <|
                \() ->
                    Rules.matchAll [ always False, always True ] ()
                        |> Expect.equal False
            ]
        , describe "any"
            [ test "true when at least one matcher is true" <|
                \() ->
                    Rules.matchAny [ always True, always False ] ()
                        |> Expect.equal True
            , test "false when all matchers are false" <|
                \() ->
                    Rules.matchAny [ always False, always False ] ()
                        |> Expect.equal False
            ]
        ]
