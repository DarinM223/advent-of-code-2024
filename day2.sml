structure Day2 =
struct
  val input =
    let
      val input: int list list ref = ref []
    in
      Utils.readLines
        (fn line =>
           input := List.mapPartial Int.fromString (Utils.words line) :: !input)
        "inputs/day2.txt";
      List.rev (!input)
    end

  fun differences l =
    List.map (fn (a, b) => b - a) (ListPair.zip (l, List.tl l))

  val allIncreasing = List.all (fn diff => diff > 0)
  val allDecreasing = List.all (fn diff => diff < 0)

  fun isGradual diff =
    abs diff >= 0 andalso abs diff <= 3

  fun safeReport (l: int list) : bool =
    let
      val diffs = differences l
    in
      (allIncreasing diffs orelse allDecreasing diffs)
      andalso List.all isGradual diffs
    end

  val numSafeReports = List.foldl (fn (_, acc) => acc + 1) 0
    (List.filter safeReport input)

  fun removedLevels (l: int list) : int list list =
    let
      fun removeAt (_ :: t) 0 = t
        | removeAt (h :: t) i =
            h :: removeAt t (i - 1)
        | removeAt [] _ = []
    in
      List.map (removeAt l) (List.tabulate (List.length l, fn i => i))
    end

  fun safeReport' (l: int list) =
    List.exists safeReport (l :: removedLevels l)

  val numSafeReports' = List.foldl (fn (_, acc) => acc + 1) 0
    (List.filter safeReport' input)

  fun results () =
    ( print
        ("Part 1 number of safe reports: " ^ Int.toString numSafeReports ^ "\n")
    ; print
        ("Part 2 number of safe reports: " ^ Int.toString numSafeReports' ^ "\n")
    )
end
