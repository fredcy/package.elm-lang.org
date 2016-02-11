module Utils.ProximityTree (ProximityTree, fromList, toList, map, lookup, nearest, temperLinearly, temperLog) where


type ProximityTree a
  = Node (Entry a) (ProximityTree a) (ProximityTree a)
  | Empty


type alias Entry a =
  { fraction : Float
  , value : a
  }



-- CREATE


fromList : (a -> Float) -> List a -> ProximityTree a
fromList toNumber list =
  case list of
    [] ->
      Empty

    first :: rest ->
      let
        firstNumber =
          toNumber first

        remainingNumbers =
          List.map toNumber rest

        mini =
          List.foldl min firstNumber remainingNumbers

        maxi =
          List.foldl max firstNumber remainingNumbers

        toFraction value =
          (toNumber value - mini) / (maxi - mini)
      in
        list
          |> List.map (\x -> ( toFraction x, x ))
          |> List.sortBy fst
          |> fromListHelp


fromListHelp : List ( Float, a ) -> ProximityTree a
fromListHelp list =
  let
    half =
      List.length list // 2

    left =
      List.take half list
  in
    case List.drop half list of
      [] ->
        Empty

      ( fraction, value ) :: right ->
        Node (Entry fraction value) (fromListHelp left) (fromListHelp right)



-- FLATTEN


toList : ProximityTree a -> List ( Float, a )
toList tree =
  case tree of
    Empty ->
      []

    Node { fraction, value } left right ->
      toList left ++ [ ( fraction, value ) ] ++ toList right



-- UTILITIES


map : (a -> b) -> ProximityTree a -> ProximityTree b
map func tree =
  case tree of
    Empty ->
      Empty

    Node { fraction, value } left right ->
      Node (Entry fraction (func value)) (map func left) (map func right)


lookup : a -> ProximityTree a -> Float
lookup targetValue tree =
  lookupHelp targetValue (toList tree)


lookupHelp : a -> List ( Float, a ) -> Float
lookupHelp targetValue entries =
  case entries of
    [] ->
      Debug.crash "Trying to lookup an entry that does not exist in this ProximityTree"

    ( fraction, value ) :: rest ->
      if targetValue == value then
        fraction
      else
        lookupHelp targetValue rest


nearest : Float -> ProximityTree a -> ( Float, a )
nearest fraction tree =
  case tree of
    Empty ->
      Debug.crash "cannot have empty proximity trees"

    Node middle _ _ ->
      nearestHelp middle fraction tree


nearestHelp : Entry a -> Float -> ProximityTree a -> ( Float, a )
nearestHelp nearest fraction tree =
  case tree of
    Empty ->
      ( nearest.fraction, nearest.value )

    Node middle left right ->
      nearestHelp
        (minBy (distanceTo fraction) middle nearest)
        fraction
        (if fraction < middle.fraction then
          left
         else
          right
        )


distanceTo : Float -> Entry a -> Float
distanceTo fraction entry =
  abs (entry.fraction - fraction)


minBy : (a -> comparable) -> a -> a -> a
minBy toComp a b =
  if toComp a < toComp b then
    a
  else
    b


temperAccum : Float -> (Float, a) -> List (Float, a) -> List (Float, a)
temperAccum step ( fraction, version ) list =
  case list of
    [] ->
      [ ( 0, version ) ]

    ( ( f', _ ) as first ) :: rest ->
      ( f' + step, version ) :: first :: rest


temperLinearly : ProximityTree a -> ProximityTree a
temperLinearly tree =
  let
    list = toList tree |> Debug.log "list"

    step = 1 / toFloat (List.length list - 1)

    tempered =
      List.foldl (temperAccum step) [] list
        |> List.reverse
  in
    fromListHelp tempered

temperLog : ProximityTree a -> ProximityTree a
temperLog tree =
  let
    list = toList tree
    logDiffs = List.foldl temperLogAccum [] list
             |> List.map (\(d, _, v) -> (d, v))
             |> List.reverse
  in
    logDiffs |> normalize |> fromListHelp
                 
temperLogAccum : (Float, a) -> List (Float, Float, a) -> List (Float, Float, a)
temperLogAccum (fraction, version) list =
  case list of
    [] ->
      [ ( 0, 0, version ) ]

    ( dist', f', v' ) :: rest ->
      let
        logDiff = logBase 2 (1 + (fraction - f') * 100000)
        dist = dist' + logDiff
      in
        ( dist, fraction, version) :: ( dist', f', v') :: rest

normalize : List (Float, a) -> List (Float, a)
normalize list =
  let
    maxi =
      List.foldl (max << fst) 0 list
  in
    List.map (\ (x, v) -> (x/maxi, v)) list
        
    
