-- Jose Bojorquez
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

main = 
    div [ style [ ("font-family", "monospace") ] ]
    [ display "sort3" (sort3 3 1 2)
    , display "Sum Two biggest" (sumSqBig 3 1 2)
    , display "Adjacent pairs" (adjpairs (List.range 1 5))
    , display "mean" (mean (List.range 1 5))
    , display "merge" (merge [1,3,5] [2,4])
    , display "Natural to Int" (natToInt (Succ (Succ Zero)))
    , display "Integer to Natural" (intToNat 4)
    , display "Add Natural" (addNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
    , display "Multiply Natural" (mulNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]


sort3: Int -> Int -> Int -> (Int, Int, Int)
sort3 x y z  = 
    if x > y && y > z then
        (x, y, z)
    else if x > z && z > y then 
        (x, z, y)
    else if y > x && x > z then
        (y, x, z)
    else if y > z && z > x then
        (y, z, x)
    else if z > x && x > y then
        (z, x, y)
    else if z > y && y > x then 
        (z, y, x)
    else
        (0, 0, 0)

sumSqBig: Int -> Int -> Int -> Int
sumSqBig x y z =
  case sort3 x y z of
    (0, 0, 0) -> 0
    
    (a, b, c) ->
      (a^2 + b^2)

adjpairs: List Int -> List (Int, Int)
adjpairs list = 
  case list of 
    [] -> []
    
    [x] -> []
    
    first :: rest -> 
      List.append ((first, (case List.head rest of
                      Just x -> x
                      Nothing -> 0)) :: []) (adjpairs rest)


mean: List Int -> Float
mean list = 
  case list of
    [] -> toFloat(0)
    
    first :: rest ->
      toFloat(List.sum list) / toFloat(List.length list)
      
      
merge: List Int -> List Int -> List Int
merge list mist = 
  List.sort(List.append (List.sort list) (List.sort mist))
    
type Nat = Zero | Succ Nat

natToInt : Nat -> Int
natToInt n =
       case n of
          Zero   -> 0
          Succ n -> 1 + (natToInt n)
          
intToNat: Int -> Nat
intToNat r =
    case r of
      0 -> Zero
      
      r -> 
        Succ ( intToNat(r-1))
        
isZero: Nat -> Bool
isZero x = 
  case x of
    Zero -> True
    
    x -> False
    
pred: Nat -> Nat
pred z = 
  case z of 
    Zero -> Zero
  
    Succ x -> x

addNat: Nat -> Nat -> Nat
addNat n m =
  if isZero(n) then 
    m
  else if isZero(m) then 
    n
  else
    addNat (pred (n)) (Succ (m))
    
    
mulNat: Nat -> Nat -> Nat
mulNat n m =
  if isZero(n) then 
    m
  else if isZero(m) then 
    Zero
  else
    mulNat (pred (n)) (addNat n m)