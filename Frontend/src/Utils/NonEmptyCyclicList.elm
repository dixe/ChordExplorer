module Utils.NonEmptyCyclicList exposing (NonEmptyCyclicList, add, advanceToNew, cur, delete, getAll, init, left, map, next, right, updateCurrent)


type alias NonEmptyCyclicList a =
    { before : List a
    , current : a
    , after : List a
    }


updateCurrent : a -> NonEmptyCyclicList a -> NonEmptyCyclicList a
updateCurrent a l =
    { l | current = a }


init : a -> List a -> NonEmptyCyclicList a
init first rest =
    { before = [], current = first, after = rest }


left : NonEmptyCyclicList a -> NonEmptyCyclicList a
left ({ before, current, after } as l) =
    case List.reverse before of
        b :: bs ->
            { l | before = List.reverse bs, current = b, after = current :: after }

        [] ->
            case List.reverse after of
                [] ->
                    l

                n :: ns ->
                    { l | before = current :: ns, current = n, after = [] }


right : NonEmptyCyclicList a -> NonEmptyCyclicList a
right =
    next


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


delete : NonEmptyCyclicList a -> NonEmptyCyclicList a
delete ({ before, current, after } as model) =
    case ( List.reverse before, after ) of
        ( [], [] ) ->
            model

        ( b :: bs, _ ) ->
            { model | before = List.reverse bs, current = b }

        ( _, n :: ns ) ->
            { model | after = ns, current = n }


advanceToNew : a -> NonEmptyCyclicList a -> NonEmptyCyclicList a
advanceToNew new ({ before, current } as model) =
    { model | before = before ++ [ current ], current = new }


add : a -> NonEmptyCyclicList a -> NonEmptyCyclicList a
add new ({ after } as model) =
    { model | after = after ++ [ new ] }


cur : NonEmptyCyclicList a -> a
cur { current } =
    current


getAll : NonEmptyCyclicList a -> List a
getAll { before, current, after } =
    before ++ [ current ] ++ after


map : (a -> b) -> NonEmptyCyclicList a -> NonEmptyCyclicList b
map f ({ before, current, after } as l) =
    { before = List.map f before
    , current = f current
    , after =
        List.map f
            after
    }
