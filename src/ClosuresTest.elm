module ClosuresTest exposing (..)


createIncrementer : Int -> Int -> Int
createIncrementer n =
 let incrementValue = n
 in ((+) incrementValue)

incrementByOne : Int -> Int 
incrementByOne = createIncrementer 1 

incrementByTwo : Int -> Int 
incrementByTwo = createIncrementer 2 


