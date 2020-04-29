module Model.Cursor exposing
    ( empty, initFromArray, indexedEmpty
    , moveCursor, incrementCursor, decrementCursor
    , foldl, map, push
    , current, index
    , length
    , atMin, atMax
    )

{-| A representation of an individual and what they do during the day


# Updaters

@docs empty, initFromArray, indexedEmpty
@docs moveCursor, incrementCursor, decrementCursor
@docs foldl, map, push


# Queries

@docs current, index
@docs length
@docs atMin, atMax

-}

import Array exposing (Array)



{- Indexed list of content with a current individual cursor -}


type alias Cursored a =
    { current : Int
    , content : Array a
    }


{-| An empty cursored list
-}
empty : Cursored a
empty =
    initFromArray Array.empty


{-| An empty list with the cursor set to the same value as an existing list
-}
indexedEmpty : Cursored a -> Cursored a
indexedEmpty old =
    { current = old.current, content = Array.empty }


{-| Build a cursored individal from an indexed list of individual
-}
initFromArray : Array a -> Cursored a
initFromArray arrayVal =
    { current = 0, content = arrayVal }


{-| Change the current individual
-}
moveCursor : Cursored a -> Int -> Cursored a
moveCursor contentArray value =
    { contentArray | current = clamp (minIndex contentArray) (maxIndex contentArray) value }


{-| Decrease the cursor
-}
decrementCursor : Cursored a -> Cursored a
decrementCursor contentArray =
    moveCursor contentArray (contentArray.current - 1)


{-| Increase the cursor
-}
incrementCursor : Cursored a -> Cursored a
incrementCursor contentArray =
    moveCursor contentArray (contentArray.current + 1)


{-| Use the cursor value to extract the current individual from the set
of content supplied
-}
current : Cursored a -> Maybe a
current content =
    Array.get content.current content.content


index : Cursored a -> Int
index content =
    content.current


{-| Is the cursor at the minimum index?
-}
atMin : Cursored a -> Bool
atMin contentArray =
    contentArray.current == minIndex contentArray


{-| Is the cursor at the maximum index?
-}
atMax : Cursored a -> Bool
atMax contentArray =
    contentArray.current == maxIndex contentArray


{-| The length of the list of content
-}
length : Cursored a -> Int
length contentArray =
    Array.length contentArray.content


{-| The index of the individual at the end of the list of content
-}
maxIndex : Cursored a -> Int
maxIndex contentArray =
    length contentArray - 1


{-| The index of the individual at the beginning of the list of content
-}
minIndex : Cursored a -> Int
minIndex _ =
    0


{-| Send the time tick to each individual in the list
-}
map : (a -> a) -> Cursored a -> Cursored a
map mapper contentArray =
    { contentArray | content = Array.map mapper contentArray.content }


{-| Push an individal onto the end of the content list
-}
push : a -> Cursored a -> Cursored a
push elem contentArray =
    { contentArray | content = Array.push elem contentArray.content }


{-| Create an indexed list of content. Each individual will be paired with its index
-}
foldl : (a -> b -> b) -> b -> Cursored a -> b
foldl tagger acc contentArray =
    Array.foldl tagger acc contentArray.content
