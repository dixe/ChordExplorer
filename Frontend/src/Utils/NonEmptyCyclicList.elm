module Utils.NonEmptyCyclicList exposing (NonEmptyCyclicList, cur, getAll, init, map, next)


type alias NonEmptyCyclicList a =
    { before : List a
    , current : a
    , after : List a
    }


init : a -> List a -> NonEmptyCyclicList a
init first rest =
    { before = [], current = first, after = rest }


next : NonEmptyCyclicList a -> NonEmptyCyclicList a
next ({ before, current, after } as pattern) =
    case after of
        a :: afters ->
            { pattern | before = before ++ [ current ], current = a, after = afters }

        [] ->
            case before of
                [] ->
                    pattern

                b :: bs ->
                    { pattern | current = b, before = [], after = bs ++ [ current ] }


cur : NonEmptyCyclicList a -> a
cur { current } =
    current


getAll : NonEmptyCyclicList a -> List a
getAll { before, current, after } =
    before ++ [ current ] ++ after


map : (a -> b) -> NonEmptyCyclicList a -> NonEmptyCyclicList b
map f ({ before, current, after } as l) =
    { before = List.map f before, current = f current, after = List.map f after }
