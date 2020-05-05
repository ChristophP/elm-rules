module Rules exposing (..)

-- MATCHERS


type alias Matcher a =
    a -> Bool


matchAlways : Matcher a
matchAlways =
    always True


matchNot : Matcher a -> Matcher a
matchNot matcher =
    matcher >> Basics.not


matchAll : List (Matcher a) -> Matcher a
matchAll matchers data =
    List.all ((|>) data) matchers


matchAny : List (Matcher a) -> Matcher a
matchAny matchers data =
    List.any ((|>) data) matchers



-- RULES


type alias Action data result =
    data -> result -> result


type Rule data result
    = Simple (Matcher data) (Action data result)
    | All (List (Rule data result))
    | First (List (Rule data result))



--| Mapped (data -> newData) (Rule newData result)


simple : { matcher : Matcher data, action : Action data result } -> Rule data result
simple { matcher, action } =
    Simple matcher action


all : List (Rule data result) -> Rule data result
all =
    All


first : List (Rule data result) -> Rule data result
first =
    First



--map : (dataA -> dataB) -> Rule dataA result -> Rule dataB result
--map =
--Mapped
-- RUN RULES


run : Rule data result -> data -> result -> result
run rule data initialValue =
    let
        result =
            runHelp rule data { foundMatch = False, value = initialValue }
    in
    result.value


type alias State result =
    { foundMatch : Bool, value : result }


runHelp : Rule data result -> data -> State result -> State result
runHelp rule data state =
    case rule of
        All rules ->
            runAllMatchingRules rules data state

        First rules ->
            runAllMatchingRules rules data state

        Simple matcher action ->
            if matcher data then
                { foundMatch = True, value = action data state.value }

            else
                { foundMatch = False, value = state.value }


runAllMatchingRules : List (Rule data result) -> data -> State result -> State result
runAllMatchingRules rules facts state =
    let
        reducer =
            \currentRule currentState ->
                let
                    newState =
                        runHelp currentRule facts currentState
                in
                State (currentState.foundMatch || newState.foundMatch) newState.value
    in
    List.foldl reducer state rules


runFirstMatchingRule : List (Rule data result) -> data -> State result -> State result
runFirstMatchingRule rules facts state =
    case rules of
        [] ->
            state

        nextRule :: remainingRules ->
            let
                { foundMatch, value } =
                    runHelp nextRule facts state
            in
            if foundMatch then
                State foundMatch value

            else
                runFirstMatchingRule remainingRules facts state
