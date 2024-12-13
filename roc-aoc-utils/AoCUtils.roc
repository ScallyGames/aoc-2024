module [AoCUtils, unwrap]

AoCUtils : {}

unwrap : Result a _ -> a
unwrap = \x ->
    when x is
        Ok a -> a
        Err _ -> crash "This should not ever happen"
