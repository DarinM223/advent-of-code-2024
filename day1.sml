structure Day1 =
struct
  val (leftList, rightList) =
    let
      val left: int list ref = ref []
      val right: int list ref = ref []
    in
      Utils.readLines
        (fn line =>
           case List.mapPartial Int.fromString (Utils.words line) of
             [l, r] => (left := l :: !left; right := r :: !right)
           | _ => ()) "inputs/day1.txt";
      (List.rev (!left), List.rev (!right))
    end

  val sortedLeftList =
    ListMergeSort.sort (fn (a, b) => Int.compare (a, b) = GREATER) leftList
  val sortedRightList =
    ListMergeSort.sort (fn (a, b) => Int.compare (a, b) = GREATER) rightList
  val totalDistance = List.foldl (fn ((a, b), acc) => acc + abs (a - b)) 0
    (ListPair.zip (sortedLeftList, sortedRightList))

  val rightListOccurTable: int IntHashTable.hash_table =
    IntHashTable.mkTable (List.length rightList, LibBase.NotFound)
  val () =
    List.app
      (fn e =>
         if IntHashTable.inDomain rightListOccurTable e then
           IntHashTable.insert rightListOccurTable
             (e, IntHashTable.lookup rightListOccurTable e + 1)
         else
           IntHashTable.insert rightListOccurTable (e, 1)) rightList

  fun lookup e =
    if IntHashTable.inDomain rightListOccurTable e then
      IntHashTable.lookup rightListOccurTable e
    else
      0

  val totalSimilarityScore =
    List.foldl (fn (e, acc) => acc + e * lookup e) 0 leftList

  fun results () =
    ( print ("Part 1 total distance: " ^ Int.toString totalDistance ^ "\n")
    ; print
        ("Part 2 total similarity score: " ^ Int.toString totalSimilarityScore
         ^ "\n")
    )
end
