structure Day10 =
struct
  val grid: int array array =
    let
      val lines: int array list ref = ref []
      fun toDigit ch = Char.ord ch - Char.ord #"0"
      val lineToDigits = Array.fromList o List.map toDigit o String.explode
    in
      Utils.readLinesTrimmed (fn line => lines := lineToDigits line :: !lines)
        "inputs/day10.txt";
      Array.fromList (List.rev (!lines))
    end

  type pos = int * int
  val pos = let open Generic in tuple2 (int, int) end

  structure PosHashSet =
    HashSetFn
      (type hash_key = pos
       val hashVal = Word32.toWord o Generic.hash pos
       fun sameKey (a, b) = a = b)

  fun inBounds (grid: int array array) (y, x) : bool =
    y >= 0 andalso y < Array.length grid andalso x >= 0
    andalso x < Array.length (Array.sub (grid, y))

  fun countReachableTrails (grid: int array array) ((y, x): pos) =
    let
      val numTrails: PosHashSet.set = PosHashSet.mkEmpty 100
      fun dfs currLevel ((y, x): pos) =
        let
          val nexts = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        in
          if Array.sub (Array.sub (grid, y), x) = currLevel then
            if currLevel = 9 then
              PosHashSet.add (numTrails, (y, x))
            else
              List.app (dfs (currLevel + 1)) (List.filter (inBounds grid) nexts)
          else
            ()
        end
    in
      dfs 0 (y, x);
      PosHashSet.numItems numTrails
    end

  val part1 =
    Array.foldli
      (fn (y, row, acc) =>
         Array.foldli
           (fn (x, digit, acc) =>
              if digit = 0 then acc + countReachableTrails grid (y, x) else acc)
           acc row) 0 grid

  fun countDistinctTrails (grid: int array array) ((y, x): pos) =
    let
      val distinctTrails = ref 0
      fun dfs currLevel ((y, x): pos) =
        let
          val nexts = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        in
          if Array.sub (Array.sub (grid, y), x) = currLevel then
            if currLevel = 9 then
              distinctTrails := !distinctTrails + 1
            else
              List.app (dfs (currLevel + 1)) (List.filter (inBounds grid) nexts)
          else
            ()
        end
    in
      dfs 0 (y, x);
      !distinctTrails
    end

  val part2 =
    Array.foldli
      (fn (y, row, acc) =>
         Array.foldli
           (fn (x, digit, acc) =>
              if digit = 0 then acc + countDistinctTrails grid (y, x) else acc)
           acc row) 0 grid

  val sample: int array array = Array.fromList
    [ Array.fromList [8, 9, 0, 1, 0, 1, 2, 3]
    , Array.fromList [7, 8, 1, 2, 1, 8, 7, 4]
    , Array.fromList [8, 7, 4, 3, 0, 9, 6, 5]
    , Array.fromList [9, 6, 5, 4, 9, 8, 7, 4]
    , Array.fromList [4, 5, 6, 7, 8, 9, 0, 3]
    , Array.fromList [3, 2, 0, 1, 9, 0, 1, 2]
    , Array.fromList [0, 1, 3, 2, 9, 8, 0, 1]
    , Array.fromList [1, 0, 4, 5, 6, 7, 3, 2]
    ]

  fun results () =
    ( print ("Part 1: " ^ Int.toString part1 ^ "\n")
    ; print ("Part 2: " ^ Int.toString part2 ^ "\n")
    )
end
