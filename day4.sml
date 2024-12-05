structure Day4 =
struct
  val input: string array =
    let
      val r: string list ref = ref []
    in
      Utils.readLines (fn line => r := line :: !r) "inputs/day4.txt";
      Array.fromList (List.rev (!r))
    end

  fun checkChoice (grid: string array) (s: string) (choice: int -> (int * int)) :
    bool =
    let
      fun go i =
        i >= String.length s
        orelse
        let
          val (y, x) = choice i
        in
          y < Array.length grid andalso y >= 0
          andalso x < String.size (Array.sub (grid, y)) andalso x >= 0
          andalso String.sub (s, i) = String.sub (Array.sub (grid, y), x)
          andalso go (i + 1)
        end
    in
      go 0
    end

  fun countXmas (grid: string array) ((y, x): int * int) : int =
    let
      val right = fn change => (y, x + change)
      val left = fn change => (y, x - change)
      val down = fn change => (y + change, x)
      val up = fn change => (y - change, x)
      val bottomRight = fn change => (y + change, x + change)
      val bottomLeft = fn change => (y + change, x - change)
      val topRight = fn change => (y - change, x + change)
      val topLeft = fn change => (y - change, x - change)
      val choices =
        [right, left, down, up, bottomRight, bottomLeft, topRight, topLeft]
    in
      List.foldl (fn (_, acc) => acc + 1) 0
        (List.filter (checkChoice grid "XMAS") choices)
    end

  fun countMasCross (grid: string array) ((y, x): int * int) : int =
    let
      val diagonals =
        List.concatMap (fn x => List.map (fn y => (y, x)) [~1, 1]) [~1, 1]
      fun mkOrdering f =
        let
          val (pos, neg) = List.partition f diagonals
          val arr = Array.fromList (pos @ neg)
        in
          fn i => let val (y', x') = Array.sub (arr, i) in (y + y', x + x') end
        end
      val yNegOne = mkOrdering (fn (y, _) => y = ~1)
      val yOne = mkOrdering (fn (y, _) => y = 1)
      val xNegOne = mkOrdering (fn (_, x) => x = ~1)
      val xOne = mkOrdering (fn (_, x) => x = 1)
      val choices = [yNegOne, yOne, xNegOne, xOne]
    in
      List.foldl (fn (_, acc) => acc + 1) 0
        (List.filter (checkChoice grid "MMSS") choices)
    end

  val sample = Array.fromList
    [ "MMMSXXMASM"
    , "MSAMXMSMSA"
    , "AMXSXMAAMM"
    , "MSAMASMSMX"
    , "XMASAMXAMM"
    , "XXAMMXXAMA"
    , "SMSMSASXSS"
    , "SAXAMASAAA"
    , "MAMMMXMMMM"
    , "MXMXAXMASX"
    ]

  val numXmas =
    Array.foldli
      (fn (y, s, acc) =>
         String.foldli
           (fn (x, ch, acc) =>
              if ch = #"X" then acc + countXmas input (y, x) else acc) acc s) 0
      input

  val numMasX =
    Array.foldli
      (fn (y, s, acc) =>
         String.foldli
           (fn (x, ch, acc) =>
              if ch = #"A" then acc + countMasCross input (y, x) else acc) acc s)
      0 input

  fun results () =
    ( print ("Part 1 number of XMAS: " ^ Int.toString numXmas ^ "\n")
    ; print ("Part 2 number of X-MAS: " ^ Int.toString numMasX ^ "\n")
    )
end
