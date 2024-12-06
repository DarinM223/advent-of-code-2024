structure Day5 =
struct
  val (rules, updates) =
    let
      val updatesMode = ref false
      val rules: (int * int) list ref = ref []
      val updates: int list list ref = ref []

      val parseRule =
        List.mapPartial Int.fromString o String.tokens (fn ch => ch = #"|")
      fun handleRule line =
        case parseRule line of
          [beforee, after] => (beforee, after)
        | _ => raise Fail "invalid case"
      val handleUpdate =
        List.mapPartial Int.fromString o String.tokens (fn ch => ch = #",")
      fun handleLine line =
        if String.all (fn ch => ch = #" " orelse ch = #"\n") line then
          updatesMode := true
        else if !updatesMode then
          updates := handleUpdate line :: !updates
        else
          rules := handleRule line :: !rules
    in
      Utils.readLines handleLine "inputs/day5.txt";
      (List.rev (!rules), List.rev (!updates))
    end

  type multi_table = IntBinarySet.set IntHashTable.hash_table
  type table = int IntHashTable.hash_table

  fun buildGtRuleTable (rules: (int * int) list) : multi_table =
    let
      val table: multi_table =
        IntHashTable.mkTable (List.length rules, LibBase.NotFound)
    in
      List.app
        (fn (beforee, after) =>
           if IntHashTable.inDomain table beforee then
             IntHashTable.insert table (beforee, IntBinarySet.add
               (IntHashTable.lookup table beforee, after))
           else
             IntHashTable.insert table (beforee, IntBinarySet.singleton after))
        rules;
      table
    end
  val buildLtRuleTable = buildGtRuleTable o List.map (fn (a, b) => (b, a))

  val (gtRuleTable, ltRuleTable) =
    (buildGtRuleTable rules, buildLtRuleTable rules)

  fun checkUpdate (table: multi_table) (update: int list) : bool =
    let
      val updateTable: table =
        IntHashTable.mkTable (List.length update, LibBase.NotFound)
      val () =
        List.appi (fn (i, e) => IntHashTable.insert updateTable (e, i)) update
      fun check (i: int, e: int) =
        let
          val mustComeAfter: IntBinarySet.set =
            if IntHashTable.inDomain table e then IntHashTable.lookup table e
            else IntBinarySet.empty
        in
          IntBinarySet.all
            (fn after =>
               not (IntHashTable.inDomain updateTable after)
               orelse IntHashTable.lookup updateTable after > i) mustComeAfter
        end
    in
      List.alli check update
    end

  fun middleNumber (update: int list) : int =
    List.nth (update, List.length update div 2)

  val part1 =
    (List.foldl (op+) 0 o List.map middleNumber
     o List.filter (checkUpdate gtRuleTable)) updates

  fun greaterThan (gtTable: multi_table) (ltTable: multi_table) (a: int, b: int) :
    bool =
    case IntHashTable.find gtTable a of
      SOME greaterThans => IntBinarySet.member (greaterThans, b)
    | NONE =>
        (case IntHashTable.find ltTable b of
           SOME lessThans => IntBinarySet.member (lessThans, a)
         | NONE => not (greaterThan gtTable ltTable (b, a)))

  val sortUpdate = ListMergeSort.sort
    (not o greaterThan gtRuleTable ltRuleTable)

  val part2 =
    (List.foldl (op+) 0 o List.map (middleNumber o sortUpdate)
     o List.filter (not o checkUpdate gtRuleTable)) updates

  val sampleRules =
    [ (47, 53)
    , (97, 13)
    , (97, 61)
    , (97, 47)
    , (75, 29)
    , (61, 13)
    , (75, 53)
    , (29, 13)
    , (97, 29)
    , (53, 29)
    , (61, 53)
    , (97, 53)
    , (61, 29)
    , (47, 13)
    , (75, 47)
    , (97, 75)
    , (47, 61)
    , (75, 61)
    , (47, 29)
    , (75, 13)
    , (53, 13)
    ]

  val sampleUpdates =
    [ [75, 47, 61, 53, 29]
    , [97, 61, 53, 29, 13]
    , [75, 29, 13]
    , [75, 97, 47, 61, 53]
    , [61, 13, 29]
    , [97, 13, 75, 29, 47]
    ]

  fun results () =
    ( print
        ("Part 1 added middle pages of ordered: " ^ Int.toString part1 ^ "\n")
    ; print
        ("Part 2 added middle pages of fixed unordered: " ^ Int.toString part2
         ^ "\n")
    )
end
