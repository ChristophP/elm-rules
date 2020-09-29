module Rules exposing
    ( Action
    , Matcher
    , Rule
    , allOf
    , map
    , matchAll
    , matchAlways
    , matchAny
    , matchNot
    , oneOf
    , rule
    , run
    )

-- MATCHERS


type alias Matcher data result =
    data -> result -> Bool


matchAlways : Matcher data result
matchAlways _ _ =
    True


matchNot : Matcher data result -> Matcher data result
matchNot matcher =
    \data value -> not (matcher data value)


matchAll : List (Matcher data result) -> Matcher data result
matchAll matchers =
    \data value -> List.all (\fn -> fn data value) matchers


matchAny : List (Matcher data result) -> Matcher data result
matchAny matchers =
    \data value -> List.any (\fn -> fn data value) matchers



-- RULES


type alias Action data result =
    data -> result -> result


type Rule data result
    = Simple (Matcher data result) (Action data result)
    | All (List (Rule data result))
    | First (List (Rule data result))



--| Mapped (data -> newData) (Rule newData result)


rule : Matcher data result -> Action data result -> Rule data result
rule matcher action =
    Simple matcher action


fromRecord : { matcher : Matcher data result, action : Action data result } -> Rule data result
fromRecord { matcher, action } =
    Simple matcher action


allOf : List (Rule data result) -> Rule data result
allOf =
    All


oneOf : List (Rule data result) -> Rule data result
oneOf =
    First


map : (dataB -> dataA) -> Rule dataA result -> Rule dataB result
map func rule_ =
    case rule_ of
        Simple matcher action ->
            Simple (func >> matcher) (\data result -> action (func data) result)

        All rules ->
            All (List.map (map func) rules)

        First rules ->
            First (List.map (map func) rules)



-- RUN RULES


run : Rule data result -> data -> result -> result
run rule_ data initialValue =
    let
        result =
            runHelp rule_ data (State False initialValue)
    in
    result.value


type alias State result =
    { foundMatch : Bool, value : result }


runHelp : Rule data result -> data -> State result -> State result
runHelp rule_ data state =
    case rule_ of
        All rules ->
            runAllMatchingRules rules data state

        First rules ->
            runAllMatchingRules rules data state

        Simple matcher action ->
            if matcher data state.value then
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
