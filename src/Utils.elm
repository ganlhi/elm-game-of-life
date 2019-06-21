module Utils exposing (removeDuplicates)


removeDuplicates : List a -> List a
removeDuplicates =
    List.foldr consIfNotMember []


consIfNotMember : a -> List a -> List a
consIfNotMember el list =
    if List.member el list then
        list

    else
        el :: list
