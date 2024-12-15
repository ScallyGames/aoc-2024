module [AoCUtils, unwrap, listToTuple, posRem, sign, groupBy]

AoCUtils : {}

unwrap : Result a _ -> a
unwrap = \x ->
    when x is
        Ok a -> a
        Err _ -> crash "This should not ever happen"

listToTuple : List a -> (a, a)
listToTuple = \list ->
    when list is
        [a, b] -> (a, b)
        _ -> crash "Expected 2 Ok values"

posRem : Int a, Int a -> Int a
posRem = \a, b ->
    ((a % b) + b) % b

sign : Int _ -> Int _
sign = \x ->
    if x == 0 then
        0
    else if x > 0 then
        1
    else
        -1

groupBy : List a, (a -> b) -> Dict b (List a)
groupBy = \list, groupingFunction ->
    initial : Dict b (List a)
    initial = Dict.empty {}
    List.walk list initial \dict, a ->
        key = groupingFunction a
        Dict.update dict key \x ->
            when x is
                Ok v -> Ok (List.append v a)
                Err Missing -> Ok [a]
