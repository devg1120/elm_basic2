module RegexTest exposing (..)

import Array
import Regex
import Math

{--
ref

-regex
https://elmprogramming.com/regular-expression.html

-parser & math
https://github.com/dmy/elm-pratt-parser/blob/2.0.0/examples/Math.elm



--}

-- data

ref_to =  Array.fromList [3, 4, 5]  

text = "[0] + [1] *[2]"


-- component 

regex_ptn = "\\[\\s*\\d+\\s*\\]"
maybeRegex = Regex.fromString  regex_ptn

regex = Maybe.withDefault Regex.never maybeRegex

f = Regex.contains regex text

m = Regex.find regex text


regex_ptn2 = "\\[|\\]"
maybeRegex2 = Regex.fromString  regex_ptn2

regex2 = Maybe.withDefault Regex.never maybeRegex2

make_entry ref =
    let
      -- [ and ] string cutting 
      -- ex.  [3] to 3 
      num_str = Regex.replace regex2 (\_ -> "")  ref.match
      ref_num = String.toInt num_str
      ref_value = case ref_num of
                     Just n ->
                         let
                            ans = Array.get n ref_to
                         in
                         case ans of
                            Just x ->
                                 x
                            _ ->
                                 -1
                     _ ->
                          -1
    in
    {
      m = ref.match
     ,i = ref.index
     ,v = ref_value
    }

ml = List.map make_entry m

--m3 = String.split "[0]" text
--m4 = String.split "[1]" text

replace str n mlist =
   let
     marray = Array.fromList mlist
     new_str = if n > (Array.length marray) then
                  str
               else
                  let
                    entry = Array.get n marray
                    new_str_ = case entry of
                         Just entry_ ->
                             let
                               split_list = String.split entry_.m str   
                               split_array = Array.fromList split_list
                               left = case  (Array.get 0 split_array ) of
                                       Just str_ -> str_
                                       r_ -> ""
                               right = case  (Array.get 1 split_array ) of
                                       Just str_ -> str_
                                       r_ -> ""
                                               
                               mid = String.fromInt entry_.v 
                             in
                             left ++ mid ++ right
                         _ ->
                             str
                  in
                  replace new_str_ (n + 1) mlist
   in
   new_str

formula = replace text 0 ml

r =  Math.run formula
