module RulesTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string, tuple)
import Rules
import Test exposing (..)


passMatcher : a -> b -> Bool
passMatcher _ _ =
    True


failMatcher : a -> b -> Bool
failMatcher _ _ =
    False


twoNumFuzzer =
    tuple ( int, int )


matchEven : a -> Int -> Bool
matchEven _ y =
    modBy 2 y == 0


isTwoMatcher : a -> Int -> Bool
isTwoMatcher _ num =
    num == 2


matchers : Test
matchers =
    describe "Matchers"
        [ describe "pass"
            [ fuzz twoNumFuzzer "matches always" <|
                \( x, y ) ->
                    Rules.matchAlways x y
                        |> Expect.equal True
            ]
        , describe "not"
            [ fuzz twoNumFuzzer "inverts the outcome of the matcher" <|
                \( _, y ) ->
                    Rules.matchNot matchEven () y
                        |> Expect.equal (not (matchEven () y))
            ]
        , describe "all"
            [ fuzz twoNumFuzzer "true when all matchers are true" <|
                \( x, y ) ->
                    Rules.matchAll [ passMatcher, passMatcher ] x y
                        |> Expect.equal True
            , fuzz twoNumFuzzer "false when at least one matcher is false" <|
                \( x, y ) ->
                    Rules.matchAll [ failMatcher, passMatcher ] x y
                        |> Expect.equal False
            ]
        , describe "any"
            [ test "true when at least one matcher is true" <|
                \() ->
                    Rules.matchAny [ passMatcher, failMatcher ] () ()
                        |> Expect.equal True
            , test "false when all matchers are false" <|
                \() ->
                    Rules.matchAny [ failMatcher, failMatcher ] () ()
                        |> Expect.equal False
            ]
        ]


rules =
    describe "Rules"
        [ describe "run"
            [ test "acummulates a result" <|
                \() ->
                    Rules.run
                        (Rules.rule isTwoMatcher (\data value -> value + 1))
                        ()
                        2
                        |> Expect.equal 3
            , test "returns old result of matcher is false" <|
                \() ->
                    Rules.run
                        (Rules.rule failMatcher (\data value -> value + 1))
                        ()
                        2
                        |> Expect.equal 2
            ]
        , describe "combined rules"
            [ test "all matching rules are run when combined with all" <|
                \() ->
                    let
                        rule =
                            Rules.rule passMatcher (\data value -> value + 1)
                    in
                    Rules.run
                        (Rules.allOf [ rule, rule ])
                        ()
                        2
                        |> Expect.equal 4
            , test "only matching rules are run when combined with all" <|
                \() ->
                    let
                        rule =
                            Rules.rule isTwoMatcher (\data value -> value + 1)
                    in
                    Rules.run
                        (Rules.allOf [ rule, rule ])
                        ()
                        2
                        |> Expect.equal 3
            , test "no unmatching rules are run when combined with all" <|
                \() ->
                    let
                        rule =
                            Rules.rule failMatcher (\data value -> value + 1)
                    in
                    Rules.run
                        (Rules.allOf [ rule, rule ])
                        ()
                        2
                        |> Expect.equal 2
            ]
        ]
