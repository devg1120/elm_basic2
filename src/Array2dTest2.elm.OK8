module Array2dTest2 exposing (..)

import Array
import Array2D
import Array2dOp



{--

  defaultCell :  String
  defaultCell = "@"

    array2Dexpand 10 10  defaultCell array2d

    array2Dtrans 0 0  2 2 8 8   defaultCell n12

    array2Dcopy 0 0  2 2 8 8    n12

    array2Dclear 0 0  2 2  defaultCell n12

    array2Dget 0 0  2 2  defaultCell n12

    array2Dset 8 8  n16 n12

    array2Dhjoin   defaultCell n16 n16

    array2Dvjoin   defaultCell n16 n16

--}
---------------------------------------------------


array2d =
    Array2D.fromList
        [ [ "Row 1-Col 1", "Row 1-Col 2" ]
        , [ "Row 2-Col 1", "Row 2-Col 2" ]
        ]


defaultCell : String
defaultCell =
    "@"


n12 =
    Array2dOp.array2Dexpand 10 10 defaultCell array2d


n13 =
    Array2dOp.array2Dtrans 0 0 2 2 8 8 defaultCell n12



--array2Dtrans 0 0  2 2 1 1   defaultCell n12


n14 =
    Array2dOp.array2Dcopy 0 0 2 2 8 8 n12


n15 =
    Array2dOp.array2Dclear 0 0 2 2 defaultCell n12



--array2Dclear 0 0  2 2  "--" n12


n16 =
    Array2dOp.array2Dget 0 0 2 2 defaultCell n12


n17 =
    Array2dOp.array2Dset 8 8 n16 n12


n18 =
    Array2dOp.array2Dhjoin defaultCell n16 n16


n19 =
    Array2dOp.array2Dvjoin defaultCell n16 n16



---------------------------------------------------
---------------------------------------------------


type alias AutoArray2D a =
    { head : Array2D.Array2D (Maybe (Array2D.Array2D (Maybe a)))
    , head_size_x : Int
    , head_size_y : Int
    , surface_size : Int
    , defaultCell : a
    }



---------------------------------------------------


info : AutoArray2D a -> { defaultCell : a, head_size_x : Int, head_size_y : Int, surface_size : Int }
info a2d =
    { head_size_x = a2d.head_size_x
    , head_size_y = a2d.head_size_y
    , surface_size = a2d.surface_size
    , defaultCell = a2d.defaultCell
    }


initAutoArray2D : Int -> Int -> Int -> a -> AutoArray2D a
initAutoArray2D head_size_x head_size_y surface_size_ defaultCell_ =
    let
        head_ =
            Array2D.repeat head_size_x head_size_y Nothing
    in
    { head = head_
    , head_size_x = head_size_x
    , head_size_y = head_size_y
    , surface_size = surface_size_
    , defaultCell = defaultCell_
    }



{--
    head size x 6
    head size y 6
    surface size 2

    head                     surface
    -- -- -- -- -- --         -- --
   |@@|  |  |  |  |  |  @@=> |  |  |
    -- -- -- -- -- --         -- --
   |  |  |  |  |  |  |       |  |  |
    -- -- -- -- -- --         -- --
   |  |  |  |  |  |  |
    -- -- -- -- -- --
   |  |  |  |  |  |  |
    -- -- -- -- -- --
   |  |  |  |  |  |  |
    -- -- -- -- -- --
   |  |  |  |  |  |  |
    -- -- -- -- -- --

--}


headSetAutoArray2D : Int -> Int -> Array2D.Array2D (Maybe a) -> AutoArray2D a -> AutoArray2D a
headSetAutoArray2D x_ y_ array2d_ autoarray2d_ =
    let
        head_ =
            autoarray2d_.head

        new_head =
            Array2D.set x_ y_ (Maybe.Just array2d_) head_
    in
    { autoarray2d_
        | head = new_head
    }


headClearAutoArray2D : Int -> Int -> AutoArray2D a -> AutoArray2D a
headClearAutoArray2D x_ y_ autoarray2d_ =
    let
        head_ =
            autoarray2d_.head

        new_head =
            Array2D.set x_ y_ Maybe.Nothing head_
    in
    { autoarray2d_
        | head = new_head
    }



{--h  y => sueface --}


surfaceGetAutoArray2D : Int -> Int -> AutoArray2D a -> Maybe (Maybe (Array2D.Array2D (Maybe a)))
surfaceGetAutoArray2D x_ y_ autoarray2d_ =
    let
        head_ =
            autoarray2d_.head

        head_part =
            Array2D.get x_ y_ head_
    in
    head_part



{--cow row => sueface --}


relativeSurfaceGetAutoArray2D : Int -> Int -> AutoArray2D a -> Maybe (Maybe (Array2D.Array2D (Maybe a)))
relativeSurfaceGetAutoArray2D c_ r_ autoarray2d_ =
    let
        head_ =
            autoarray2d_.head

        x_ =
            c_ // autoarray2d_.surface_size

        y_ =
            r_ // autoarray2d_.surface_size

        head_part =
            Array2D.get x_ y_ head_
    in
    head_part


cellGetAutoArray2D : Int -> Int -> AutoArray2D a -> a
cellGetAutoArray2D c_ r_ autoarray2d_ =
    let
        head_ =
            autoarray2d_.head

        x_ =
            c_ // autoarray2d_.surface_size

        y_ =
            r_ // autoarray2d_.surface_size

        x_a =
            c_ - (x_ * autoarray2d_.surface_size)

        y_a =
            r_ - (y_ * autoarray2d_.surface_size)

        surface =
            Array2D.get x_ y_ head_

        te =
            case surface of
                Just surface2 ->
                    case surface2 of
                        Just surface3 ->
                            let
                                cell =
                                    Array2D.get x_a y_a surface3
                            in
                            case cell of
                                Just cell2 ->
                                    cell2

                                _ ->
                                    --defaultcell
                                    --autoarray2d_.defaultCell
                                    Nothing

                        _ ->
                            --defaultcell
                            --autoarray2d_.defaultCell
                            Nothing

                _ ->
                    --defaultcell
                    --autoarray2d_.defaultCell
                    Nothing
    in
    case te of
        Just te_ ->
            te_

        Nothing ->
            autoarray2d_.defaultCell


expandAutoArray2D :
    Int
    -> Int
    -> Array2D.Array2D (Maybe (Array2D.Array2D a))
    -> Array2D.Array2D (Maybe (Array2D.Array2D a))
expandAutoArray2D rowsnum colsnum orgArray2D =
    let
        _ =
            Debug.log "expand rows:" rowsnum

        _ =
            Debug.log "expand cols:" colsnum

        baseArray2D =
            Array2D.repeat rowsnum colsnum Maybe.Nothing

        cellReplace : Int -> Int -> Maybe (Array2D.Array2D a) -> Maybe (Array2D.Array2D a)
        cellReplace row col cell =
            let
                data =
                    Array2D.get row col orgArray2D
            in
            case data of
                Nothing ->
                    cell

                Just newcell ->
                    newcell
    in
    Array2D.indexedMap cellReplace baseArray2D


cellSetAutoArray2D :
    Int
    -> Int
    -> a
    -> AutoArray2D a
    -> AutoArray2D a
cellSetAutoArray2D c_ r_ cell autoarray2d_ =
    let
        --_ = Debug.log "--cellSetAutoArray2D"  0
        head_ =
            autoarray2d_.head

        x_ =
            c_ // autoarray2d_.surface_size

        y_ =
            r_ // autoarray2d_.surface_size

        x_a =
            c_ - (x_ * autoarray2d_.surface_size)

        y_a =
            r_ - (y_ * autoarray2d_.surface_size)

        surface =
            Array2D.get x_ y_ head_

        -- hard over to expand array2d --
        autoarray2d_2 =
            if x_ >= autoarray2d_.head_size_x && y_ >= autoarray2d_.head_size_y then
                let
                    _ =
                        Debug.log "*** surface size over x & y " 0

                    head_t =
                        expandAutoArray2D (x_ + 1) (y_ + 1) head_
                in
                { head = head_t
                , head_size_x = x_ + 1
                , head_size_y = y_ + 1
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else if x_ >= autoarray2d_.head_size_x then
                let
                    _ =
                        Debug.log "*** surface size over x " 0

                    head_t =
                        expandAutoArray2D (x_ + 1) autoarray2d_.head_size_y head_
                in
                { head = head_t
                , head_size_x = x_ + 1
                , head_size_y = autoarray2d_.head_size_y
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else if y_ >= autoarray2d_.head_size_y then
                let
                    _ =
                        Debug.log "*** surface size over y " 0

                    head_t =
                        expandAutoArray2D autoarray2d_.head_size_x (y_ + 1) head_
                in
                { head = head_t
                , head_size_x = autoarray2d_.head_size_x
                , head_size_y = y_ + 1
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else
                autoarray2d_

        head_2 =
            autoarray2d_2.head

        x_2 =
            c_ // autoarray2d_2.surface_size

        y_2 =
            r_ // autoarray2d_2.surface_size

        x_a2 =
            c_ - (x_2 * autoarray2d_2.surface_size)

        y_a2 =
            r_ - (y_2 * autoarray2d_2.surface_size)

        surface_ =
            Array2D.get x_2 y_2 head_2

        new_surface =
            case surface_ of
                Just surface2 ->
                    case surface2 of
                        Just surface3 ->
                            Array2D.set x_a2 y_a2 (Maybe.Just cell) surface3

                        _ ->
                            let
                                --surface4 = Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size autoarray2d_2.defaultCell
                                surface4 =
                                    Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size Nothing
                            in
                            Array2D.set x_a2 y_a2 (Maybe.Just cell) surface4

                _ ->
                    let
                        -- _ = Debug.log "*** surface set" x_
                        -- _ = Debug.log "*** surface set" y_
                        --surface5 = Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size autoarray2d_2.defaultCell
                        surface5 =
                            Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size Nothing
                    in
                    Array2D.set x_a2 y_a2 (Maybe.Just cell) surface5

        new_head =
            Array2D.set x_2 y_2 (Maybe.Just new_surface) head_2
    in
    { autoarray2d_2 | head = new_head }

-------------------------



{--
insertRowAutoArray2D :
    Int
    -> AutoArray2D a
    -> AutoArray2D a
insertRowAutoArray2D  r_  autoarray2d_ =
    let
        --_ = Debug.log "--cellSetAutoArray2D"  0
        head_ =
            autoarray2d_.head

        x_ =
            c_ // autoarray2d_.surface_size

        y_ =
            r_ // autoarray2d_.surface_size

        x_a =
            c_ - (x_ * autoarray2d_.surface_size)

        y_a =
            r_ - (y_ * autoarray2d_.surface_size)

        surface =
            Array2D.get x_ y_ head_

        -- hard over to expand array2d --
        autoarray2d_2 =
            if x_ >= autoarray2d_.head_size_x && y_ >= autoarray2d_.head_size_y then
                let
                    _ =
                        Debug.log "*** surface size over x & y" 0

                    head_t =
                        expandAutoArray2D (x_ + 1) (y_ + 1) head_
                in
                { head = head_t
                , head_size_x = x_ + 1
                , head_size_y = y_ + 1
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else if x_ >= autoarray2d_.head_size_x then
                let
                    _ =
                        Debug.log "*** surface size over  y" 0

                    head_t =
                        expandAutoArray2D (x_ + 1) autoarray2d_.head_size_y head_
                in
                { head = head_t
                , head_size_x = x_ + 1
                , head_size_y = autoarray2d_.head_size_y
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else if y_ >= autoarray2d_.head_size_y then
                let
                    _ =
                        Debug.log "*** surface size over x  " 0

                    head_t =
                        expandAutoArray2D autoarray2d_.head_size_x (y_ + 1) head_
                in
                { head = head_t
                , head_size_x = autoarray2d_.head_size_x
                , head_size_y = y_ + 1
                , surface_size = autoarray2d_.surface_size
                , defaultCell = autoarray2d_.defaultCell
                }

            else
                autoarray2d_

        head_2 =
            autoarray2d_2.head

        x_2 =
            c_ // autoarray2d_2.surface_size

        y_2 =
            r_ // autoarray2d_2.surface_size

        x_a2 =
            c_ - (x_2 * autoarray2d_2.surface_size)

        y_a2 =
            r_ - (y_2 * autoarray2d_2.surface_size)

        surface_ =
            Array2D.get x_2 y_2 head_2

        new_surface =
            case surface_ of
                Just surface2 ->
                    case surface2 of
                        Just surface3 ->
                            Array2D.set x_a2 y_a2 (Maybe.Just cell) surface3

                        _ ->
                            let
                                --surface4 = Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size autoarray2d_2.defaultCell
                                surface4 =
                                    Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size Nothing
                            in
                            Array2D.set x_a2 y_a2 (Maybe.Just cell) surface4

                _ ->
                    let
                        -- _ = Debug.log "*** surface set" x_
                        -- _ = Debug.log "*** surface set" y_
                        --surface5 = Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size autoarray2d_2.defaultCell
                        surface5 =
                            Array2D.repeat autoarray2d_2.surface_size autoarray2d_2.surface_size Nothing
                    in
                    Array2D.set x_a2 y_a2 (Maybe.Just cell) surface5

        new_head =
            Array2D.set x_2 y_2 (Maybe.Just new_surface) head_2
    in
    { autoarray2d_2 | head = new_head }

--}

{--
--foldlAutoArray2D : (a -> b -> b) -> b -> AutoArray2D a -> b
--foldlAutoArray2D : (Maybe (Array.Array (Maybe (Array2D.Array2D a))) -> b -> b) -> b -> AutoArray2D a -> b
foldlAutoArray2D : ((Int,Int) -> Array.Array (Maybe (Array2D.Array2D a)) -> b -> b) -> b -> AutoArray2D a -> b
foldlAutoArray2D   func acc autoarray2d_ =
    let
       row = Array2D.getRow 0 autoarray2d_.head
       new_head_ = Array2D.deleteRow 0 autoarray2d_.head
       new_autoarray2d_ = { autoarray2d_ | head = new_head_ }
    in
--       case Array2D.isEmpty new_head_ of
--               False ->
--                    foldlAutoArray2D func (func row acc) new_autoarray2d_
--               True ->
--                    acc
--
       case row of
               Just row_ ->
                    foldlAutoArray2D func (func row_ acc) new_autoarray2d_
               Nothing ->
                    acc
--}
--------------------------------------------------


foldlCellPartAutoArray2D : (( Int, Int ) -> a -> b -> b) -> b -> Array.Array (Maybe a) -> Int -> Int -> b
foldlCellPartAutoArray2D func acc array r_ c_ =
    let
        -- _ = Debug.log "                cellpart array:" array
        cell =
            Array.slice 0 1 array

        value =
            Array.get 0 cell

        new_array_ =
            Array.slice 1 (Array.length array) array
    in
    case Array.isEmpty cell of
        False ->
            case value of
                Just value_ ->
                    case value_ of
                        Just value_2 ->
                            let
                                acc2 =
                                    func ( r_, c_ ) value_2 acc
                            in
                            foldlCellPartAutoArray2D func acc2 new_array_ r_ c_

                        Nothing ->
                            acc

                Nothing ->
                    acc

        True ->
            acc


foldlCellAutoArray2D : (( Int, Int ) -> a -> b -> b) -> b -> Array.Array (Maybe a) -> Int -> Int -> b
foldlCellAutoArray2D func acc array r_ c_ =
    let
        --_ = Debug.log "                cell array:" array
        col =
            Array.slice 0 1 array

        new_array_ =
            Array.slice 1 (Array.length array) array

        acc2 =
            case Array.isEmpty col of
                False ->
                    --foldlCellAutoArray2D func (func col acc) new_array_
                    foldlCellPartAutoArray2D func acc col r_ c_

                --acc
                True ->
                    acc
    in
    case Array.isEmpty col of
        False ->
            --foldlCellAutoArray2D func (func col acc) new_array_
            foldlCellAutoArray2D func acc2 new_array_ r_ (c_ + 1)

        True ->
            acc2


foldlColAutoArray2D : (( Int, Int ) -> a -> b -> b) -> b -> Maybe (Array2D.Array2D (Maybe a)) -> Int -> Int -> b
foldlColAutoArray2D func acc array2d_ r_ c_ =
    case array2d_ of
        Just array2d_2 ->
            let
                row =
                    Array2D.getRow 0 array2d_2

                new_array2d_ =
                    Array2D.deleteRow 0 array2d_2
            in
            case row of
                Just row_ ->
                    --let _ = Debug.log "          full row Just" 0 in
                    let
                        acc2 =
                            foldlCellAutoArray2D func acc row_ r_ c_
                    in
                    foldlColAutoArray2D func acc2 (Just new_array2d_) (r_ + 1) c_

                Nothing ->
                    --let _ = Debug.log "          full row Nothing" 0 in
                    acc

        Nothing ->
            acc


foldlPartAutoArray2D : (( Int, Int ) -> a -> b -> b) -> b -> Array.Array (Maybe (Array2D.Array2D (Maybe a))) -> Int -> Int -> Int -> b
foldlPartAutoArray2D func acc array y_ x_ sfs =
    let
        --_ = Debug.log "                part array:" array
        col =
            Array.slice 0 1 array

        new_array_ =
            Array.slice 1 (Array.length array) array

        array2d_ =
            Array.get 0 array

        --_ = Debug.log "                part array:" array2d_
        acc2 =
            case array2d_ of
                Just array2d_2 ->
                    foldlColAutoArray2D func acc array2d_2 (y_ * sfs) (x_ * sfs)

                Nothing ->
                    acc
    in
    case Array.isEmpty col of
        False ->
            --foldlPartAutoArray2D func (func col acc) new_array_
            foldlPartAutoArray2D func acc2 new_array_ y_ (x_ + 1) sfs

        True ->
            acc2


foldlFullAutoArray2Dtop : (( Int, Int ) -> a -> b -> b) -> b -> AutoArray2D a -> Int -> Int -> b
foldlFullAutoArray2Dtop func acc autoarray2d_ y_ x_ =
    let
        --_ = Debug.log "          full" 0
        row =
            Array2D.getRow 0 autoarray2d_.head

        new_head_ =
            Array2D.deleteRow 0 autoarray2d_.head

        new_autoarray2d_ =
            { autoarray2d_ | head = new_head_ }

        acc2 =
            case row of
                Just row_ ->
                    --let _ = Debug.log "          full row Just" 0 in
                    foldlPartAutoArray2D func acc row_ y_ x_ autoarray2d_.surface_size

                Nothing ->
                    --let _ = Debug.log "          full row Nothing" 0 in
                    acc
    in
    case row of
        Just row_ ->
            foldlFullAutoArray2Dtop func acc2 new_autoarray2d_ (y_ + 1) x_

        Nothing ->
            acc2


foldlFullAutoArray2D : (( Int, Int ) -> a -> b -> b) -> b -> AutoArray2D a -> b
foldlFullAutoArray2D func acc autoarray2d_ =
    foldlFullAutoArray2Dtop func acc autoarray2d_ 0 0


cloneAutoArray2D :  AutoArray2D a -> AutoArray2D a
cloneAutoArray2D  aa2d_ =
    let
      new_aa2d_ = initAutoArray2D aa2d_.head_size_x
                                  aa2d_.head_size_y
                                  aa2d_.surface_size
                                  aa2d_.defaultCell

      new_aa2d_2 = { new_aa2d_ | head = aa2d_.head }

    in
    new_aa2d_2

insertRowAutoArray2D : Int -> AutoArray2D a -> AutoArray2D a
insertRowAutoArray2D  insert_row_index aa2d_ =
    let
      row_max = aa2d_.head_size_x * aa2d_.surface_size
      col_max = aa2d_.head_size_y * aa2d_.surface_size

      new_aa2d_ = initAutoArray2D aa2d_.head_size_x
                                  aa2d_.head_size_y
                                  aa2d_.surface_size
                                  aa2d_.defaultCell

      --
      -- inter function
      col_recur_walk insert_row_index_ r c new_aa2d__2 aa2d__2 =
              if c >= col_max then
                new_aa2d__2

              else
                let
                  cell = cellGetAutoArray2D r c aa2d__2

                  new_aa2d__3 = if r >=  insert_row_index_ then
                                   cellSetAutoArray2D (r+1) c cell new_aa2d__2
                                else
                                   cellSetAutoArray2D r c cell new_aa2d__2
                in
                col_recur_walk insert_row_index_ r  (c + 1) new_aa2d__3 aa2d__2

      row_recur_walk insert_row_index_ r c new_aa2d__ aa2d__ =
              if r >= row_max then
                new_aa2d__
              else
                let
                  new_aa2d__2 =  col_recur_walk insert_row_index_ r c new_aa2d__ aa2d__
                in
                row_recur_walk insert_row_index_ (r + 1) c new_aa2d__2 aa2d__

          
      result = row_recur_walk insert_row_index 0 0  new_aa2d_ aa2d_ 
    in
    result

insertColAutoArray2D : Int -> AutoArray2D a -> AutoArray2D a
insertColAutoArray2D  insert_col_index aa2d_ =
    let
      row_max = aa2d_.head_size_x * aa2d_.surface_size
      col_max = aa2d_.head_size_y * aa2d_.surface_size

      new_aa2d_ = initAutoArray2D aa2d_.head_size_x
                                  aa2d_.head_size_y
                                  aa2d_.surface_size
                                  aa2d_.defaultCell

      --
      -- inter function
      col_recur_walk insert_col_index_ r c new_aa2d__2 aa2d__2 =
              if c >= col_max then
                new_aa2d__2

              else
                let
                  cell = cellGetAutoArray2D r c aa2d__2

                  new_aa2d__3 = if c >=  insert_col_index_ then
                                   cellSetAutoArray2D r (c+1) cell new_aa2d__2
                                else
                                   cellSetAutoArray2D r c cell new_aa2d__2
                in
                col_recur_walk insert_col_index_ r  (c + 1) new_aa2d__3 aa2d__2

      row_recur_walk insert_col_index_ r c new_aa2d__ aa2d__ =
              if r >= row_max then
                new_aa2d__
              else
                let
                  new_aa2d__2 =  col_recur_walk insert_col_index_ r c new_aa2d__ aa2d__
                in
                row_recur_walk insert_col_index_ (r + 1) c new_aa2d__2 aa2d__

          
      result = row_recur_walk insert_col_index 0 0  new_aa2d_ aa2d_ 
    in
    result
---------------------------------------------------


p : String -> String
p str =
    Debug.log str
        "OK"



{--
d : Int -> Int -> AutoArray2D String -> (Maybe (Array2D.Array2D a))
d c r a_ =
   let
     ret = cellSetAutoArray2D c r "+" a_
     _ = Debug.log ret 0 
   in
    ret
--}
--------------------------------------------
-- BASIC TEST


a20 =
    initAutoArray2D 10 10 10 "_"


a21 =
    Array2D.repeat 10 10 (Just "@")


a22 =
    headSetAutoArray2D 0 0 a21 a20


a23 =
    headClearAutoArray2D 0 0 a22


a24 =
    cellSetAutoArray2D 0 0 "+" a23


a25 =
    cellSetAutoArray2D 99 99 "+" a24


a26 =
    cellSetAutoArray2D 99 199 "+" a25


a27 =
    cellSetAutoArray2D 199 99 "+" a24


a28 =
    cellSetAutoArray2D 199 199 "+" a26



{--
func_ (r,c) row acc =
    let
      --_ = Debug.log "func" 0
      acc_ = acc + 1
    in
    acc_


f  = foldlAutoArray2D     func_ 0 a25
s  = foldlAutoArray2D     func_ 0 a28
--}


func1_ ( r, c ) value acc =
    let
        acc_ =
            acc + 1

        _ =
            Debug.log "func:" acc_
    in
    acc_


f1 =
    foldlFullAutoArray2D func1_ 0 a26


s1 =
    foldlFullAutoArray2D func1_ 0 a28


func2_ ( r, c ) cell_value acc =
    let
        num_ =
            acc.cell_num + 1

        value_ =
            acc.cell_value ++ cell_value

        acc_ =
            { acc | cell_num = num_, cell_value = value_ }

        _ =
            Debug.log "func:" acc_
    in
    acc_


acc_base =
    { cell_num = 0
    , cell_value = ""
    }


f2 =
    foldlFullAutoArray2D func2_ acc_base a26


s2 =
    foldlFullAutoArray2D func2_ acc_base a28



----------------------------------------------------
-- ADVANCED TEST


type CellValue
    = IntCell Int
    | StringCell String


type alias Formula =
    { eval : Bool
    , f : String
    }


type alias Cell =
    { value : CellValue
    , formula : Formula
    , name : String
    }


defaultCell__ : Cell
defaultCell__ =
    { 
    --  value = IntCell 0
      value = StringCell "__"
    , name = "df"
    , formula =
        { eval = False
        , f = ""
        }
    }

type SetCellValue 
     = SetInt Int
     | SetString String

cellSetValueAutoArray2D :
    Int
    -> Int
    -> SetCellValue
    -> AutoArray2D Cell
    -> AutoArray2D Cell

cellSetValueAutoArray2D c_ r_ value autoarray2d_ =
    let
     value_ =  case value of
                 SetInt x ->
                      IntCell x
                 SetString s ->
                      StringCell s
    in
    cellSetAutoArray2D c_ r_ { name = "fc", value = value_, formula = { eval = False, f = "" } } autoarray2d_


g10 =
    initAutoArray2D 10 10 10 defaultCell__


g11 =
    cellSetAutoArray2D 0 0 { name = "fc", value = IntCell 5, formula = { eval = False, f = "" } } g10


g12 =
    cellSetAutoArray2D 99 99 { name = "sc", value = IntCell 7, formula = { eval = False, f = "" } } g11


g13 =
    cellSetAutoArray2D 100 100 { name = "sc", value = IntCell 0, formula = { eval = True, f = "= (0,0) + (99,99)" } } g12


func3_ ( r, c ) cell_value acc =
    let
        num =
            case cell_value.value of
                IntCell n ->
                    let
                        _ =
                            Debug.log "func3" ( r, c, n )
                    in
                    n

                _ ->
                    0

        num_ =
            acc.cell_num + num

        acc_ =
            { acc | cell_num = num_ }

        --_ = Debug.log "func:" acc_
    in
    acc_


g141 =
    foldlFullAutoArray2D func3_ acc_base g10


g142 =
    foldlFullAutoArray2D func3_ acc_base g11


g14 =
    foldlFullAutoArray2D func3_ acc_base g13


g15 =
    cellGetAutoArray2D 0 0 g13


g16 =
    cellGetAutoArray2D 99 99 g13


g17 =
    cellGetAutoArray2D 100 100 g13

--------------------------------------
--- row/col instart delete

d2 : AutoArray2D Cell -> String
d2  aa2d =
    let
        row_max = aa2d.head_size_x * aa2d.surface_size
        col_max = aa2d.head_size_y * aa2d.surface_size
        a2d = aa2d.head

        col_recur_walk r c array str =
              let
                _ = Debug.log "      col:" (c, col_max)
                col = Array.get c array

              in
                if c >= col_max then
                  str ++ " | "
                else
                  let
                    str2 = str ++ "** " ++ (String.fromInt r) ++  (String.fromInt c)
                  in
                  col_recur_walk r  (c + 1) array str2

        row_recur_walk r c aa str =
              let
                _ = Debug.log "row:" (r, row_max)
                row = Array2D.getRow r aa

              in
                if r >= row_max then
                  str 
                else
                  let
                    --str2 = str ++ "@@ " ++ (String.fromInt r)
                    _ = Debug.log "  row:" (r, row_max)
                    str2 = case row of
                             Just row_ ->
                                  col_recur_walk r c  row_  str
                             Nothing ->
                                  let
                                    _ = Debug.log "  Nothing:" (r, row_max)
                                  in
                                  str
                  in
                  row_recur_walk (r + 1) c aa str2

            
        result = row_recur_walk 0 0  a2d ""

    in
     result

d : AutoArray2D Cell -> String
d  aa2d =
    let
        row_max = aa2d.head_size_x * aa2d.surface_size
        col_max = aa2d.head_size_y * aa2d.surface_size

        col_recur_walk r c aa2d_ str =
              let
                --_ = Debug.log "      col:" (c, col_max)

                getvalue : Cell -> String
                getvalue cell =
                      case cell.value of
                          StringCell str_ ->
                                  str_
                          _ ->
                                  "n/a"
                          
              in
                if c >= col_max then
                  str ++ " | "

                else
                  let
                    cell = cellGetAutoArray2D r c aa2d_
                    --_ = Debug.log "cell:" (getvalue cell )
                    str2 = str ++ " " ++ (getvalue cell )
                  in
                  col_recur_walk r  (c + 1) aa2d_ str2

        row_recur_walk r c aa2d__ str =
              --let
              --  _ = Debug.log "row:" (r, row_max)

              --in
                if r >= row_max then
                  str  
                else
                  let
                    str2 =  col_recur_walk r c aa2d__ str
                  in
                  row_recur_walk (r + 1) c aa2d__ str2

            
        result = row_recur_walk 0 0  aa2d """ """

    in
     result


d2d : AutoArray2D Cell -> Array2D.Array2D Cell
d2d  aa2d =
    let

        d2d_defaultcell : Cell
        d2d_defaultcell =
            { 
              --value = StringCell "**"
              value = IntCell 0
            , name = "df"
            , formula =
                { eval = False
                , f = ""
                }
            }

        row_max = aa2d.head_size_x * aa2d.surface_size
        col_max = aa2d.head_size_y * aa2d.surface_size
        a2d = Array2D.repeat row_max col_max d2d_defaultcell

        col_recur_walk r c aa2d_ a2d_ =
              --let
              --  _ = Debug.log "      col:" (c, col_max)

              --in
                if c >= col_max then
                  a2d_

                else
                  let
                    cell = cellGetAutoArray2D r c aa2d_
                    --_ = Debug.log "cell:" (getvalue cell )
                    new_a2d_ = Array2D.set r c cell a2d_
                  in
                  col_recur_walk r  (c + 1) aa2d_ new_a2d_

        row_recur_walk r c aa2d__ a2d__ =
              --let
              --  _ = Debug.log "row:" (r, row_max)

              --in
                if r >= row_max then
                  a2d__
                else
                  let
                    new_a2d__ =  col_recur_walk r c aa2d__ a2d__
                  in
                  row_recur_walk (r + 1) c aa2d__ new_a2d__

            
        result = row_recur_walk 0 0  aa2d a2d

    in
     result

z10 =
    initAutoArray2D 2 2 2 defaultCell__

z11 =  cellSetValueAutoArray2D 0 0 (SetString "00")  
   <|  cellSetValueAutoArray2D 0 1 (SetString "01")  
   <|  cellSetValueAutoArray2D 0 2 (SetString "02")  
   <|  cellSetValueAutoArray2D 0 3 (SetString "03")  

   <|  cellSetValueAutoArray2D 1 0 (SetString "10")  
   <|  cellSetValueAutoArray2D 1 1 (SetString "11")  
   <|  cellSetValueAutoArray2D 1 2 (SetString "12")  
   <|  cellSetValueAutoArray2D 1 3 (SetString "13")  

   <|  cellSetValueAutoArray2D 2 0 (SetString "20")  
   <|  cellSetValueAutoArray2D 2 1 (SetString "21")  
   <|  cellSetValueAutoArray2D 2 2 (SetString "22")  
   <|  cellSetValueAutoArray2D 2 3 (SetString "23")  

   <|  cellSetValueAutoArray2D 3 0 (SetString "30")  
   <|  cellSetValueAutoArray2D 3 1 (SetString "31")  
   <|  cellSetValueAutoArray2D 3 2 (SetString "32")  
   <|  cellSetValueAutoArray2D 3 3 (SetString "33")  

   <|  z10

z12 = d z11
z13 = d2d z11

z14 = cloneAutoArray2D z11
z15 = insertRowAutoArray2D 1 z11
z16 = insertColAutoArray2D 1 z11

