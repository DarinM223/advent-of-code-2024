structure Day6 =
struct
  val grid: char array array =
    let
      val grid: char array list ref = ref []
    in
      Utils.readLinesTrimmed
        (fn line => grid := Array.fromList (String.explode line) :: !grid)
        "inputs/day6.txt";
      Array.fromList (List.rev (!grid))
    end

  fun guardPosition grid =
    Array.foldli
      (fn (y, row, acc) =>
         Array.foldli (fn (x, ch, acc) => if ch = #"^" then (y, x) else acc) acc
           row) (0, 0) grid

  datatype direction = UP | DOWN | LEFT | RIGHT
  val showDirection =
    fn UP => "UP" | DOWN => "DOWN" | LEFT => "LEFT" | RIGHT => "RIGHT"
  val compareDirection =
    fn (UP, UP) => EQUAL
     | (UP, DOWN) => LESS
     | (UP, LEFT) => LESS
     | (UP, RIGHT) => LESS
     | (DOWN, UP) => GREATER
     | (DOWN, DOWN) => EQUAL
     | (DOWN, LEFT) => LESS
     | (DOWN, RIGHT) => LESS
     | (LEFT, UP) => GREATER
     | (LEFT, DOWN) => GREATER
     | (LEFT, LEFT) => EQUAL
     | (LEFT, RIGHT) => LESS
     | (RIGHT, UP) => GREATER
     | (RIGHT, DOWN) => GREATER
     | (RIGHT, LEFT) => GREATER
     | (RIGHT, RIGHT) => EQUAL

  fun move (y, x) UP = (y - 1, x)
    | move (y, x) DOWN = (y + 1, x)
    | move (y, x) LEFT = (y, x - 1)
    | move (y, x) RIGHT = (y, x + 1)

  fun hitObstacle UP = RIGHT
    | hitObstacle RIGHT = DOWN
    | hitObstacle DOWN = LEFT
    | hitObstacle LEFT = UP

  infix 5 !
  fun op! (grid, (y, x)) =
    Array.sub (Array.sub (grid, y), x)

  fun inGrid grid (y, x) : bool =
    y >= 0 andalso y < Array.length grid andalso x >= 0
    andalso x < Array.length (Array.sub (grid, y))

  type pos = int * int
  val showPos = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [Int.toString t0, Int.toString t1] ^ ")"
  val comparePos = fn ((t0, t1), (t2, t3)) =>
    (case Int.compare (t0, t2) of
       EQUAL => Int.compare (t1, t3)
     | ? => ?)

  type posAndDirection = pos * direction
  val showPosAndDirection = fn (t0, t1) =>
    "(" ^ String.concatWith ", " [showPos t0, showDirection t1] ^ ")"
  val comparePosAndDirection = fn ((t0, t1), (t2, t3)) =>
    (case comparePos (t0, t2) of
       EQUAL => compareDirection (t1, t3)
     | ? => ?)

  structure PosSet = RedBlackSetFn (type ord_key = pos val compare = comparePos)
  structure PosDirectionSet =
    RedBlackSetFn
      (type ord_key = posAndDirection val compare = comparePosAndDirection)

  fun foldSimulation (grid: char array array) (initPos: pos)
    (initDirection: direction) (f: pos -> direction -> 'a -> 'a) (initState: 'a) :
    'a =
    let
      fun go pos direction state =
        let
          val state = f pos direction state
          val nextPos = move pos direction
        in
          if not (inGrid grid nextPos) then
            state
          else if grid ! nextPos = #"#" then
            go pos (hitObstacle direction) state
          else
            go nextPos direction state
        end
    in
      go initPos initDirection initState
    end

  fun go pos _ set =
    if PosSet.member (set, pos) then set else PosSet.add (set, pos)
  val path = PosSet.listItems
    (foldSimulation grid (guardPosition grid) UP go PosSet.empty)
  val part1 = List.length path

  fun setGridChar grid (y, x) ch =
    Array.update (Array.sub (grid, y), x, ch)

  (* For every position the guard visits, attempt to place an obstacle there and see
     if the guard loops *)
  fun findLoops grid startingPos (pos, addedObstacles) =
    let
      exception Success
      val oldCh = grid ! pos
      val () = setGridChar grid pos #"#"
      fun findCycle pos direction set =
        if PosDirectionSet.member (set, (pos, direction)) then raise Success
        else PosDirectionSet.add (set, (pos, direction))
      (* To check for cycles you have to start from the starting position, attempting to
         start at one of the traversed positions will cause overcounting of loops *)
      val canAddObstacle =
        ( foldSimulation grid startingPos UP findCycle PosDirectionSet.empty
        ; false
        )
        handle Success => true
      val () = setGridChar grid pos oldCh
    in
      if canAddObstacle then PosSet.add (addedObstacles, pos)
      else addedObstacles
    end

  val part2 = PosSet.numItems
    (List.foldl (findLoops grid (guardPosition grid)) PosSet.empty path)

  val sampleGrid = Array.fromList (List.map (Array.fromList o String.explode)
    [ "....#....."
    , ".........#"
    , ".........."
    , "..#......."
    , ".......#.."
    , ".........."
    , ".#..^....."
    , "........#."
    , "#........."
    , "......#..."
    ])

  fun results () =
    ( print
        ("Part 1 number of positions the guard visits: " ^ Int.toString part1
         ^ "\n")
    ; print
        ("Part 2 number of obstacles that create loops: " ^ Int.toString part2
         ^ "\n")
    )
end
