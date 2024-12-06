structure Day6 =
struct
  val grid: char array array =
    let
      val grid: char array list ref = ref []
    in
      Utils.readLines
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

  fun inGrid grid (y, x) : bool =
    y >= 0 andalso y < Array.length grid andalso x >= 0
    andalso x < Array.length (Array.sub (grid, y))

  infix 5 !
  fun op! (grid, (y, x)) =
    Array.sub (Array.sub (grid, y), x)

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
  val part1 = PosSet.numItems
    (foldSimulation grid (guardPosition grid) UP go PosSet.empty)

  structure PosDirectionSet =
    RedBlackSetFn
      (type ord_key = posAndDirection val compare = comparePosAndDirection)

  fun setGridChar grid (y, x) ch =
    let val subarr = Array.sub (grid, y)
    in Array.update (subarr, x, ch)
    end

  fun findLoops grid startingPos pos direction addedObstacles =
    let
      val nextPos = move pos direction
    in
      if
        not (inGrid grid nextPos) orelse nextPos = startingPos
        orelse grid ! nextPos = #"#"
      then
        addedObstacles
      else
        let
          exception Success
          val oldCh = grid ! nextPos
          val () = setGridChar grid nextPos #"#"
          fun findCycle pos direction set =
            if PosDirectionSet.member (set, (pos, direction)) then raise Success
            else PosDirectionSet.add (set, (pos, direction))
          val canAddObstacle =
            ( foldSimulation grid pos direction findCycle PosDirectionSet.empty
            ; false
            )
            handle Success => true
          val () = setGridChar grid nextPos oldCh
        in
          if canAddObstacle then PosSet.add (addedObstacles, nextPos)
          else addedObstacles
        end
    end

  (* Needs to be 1796 *)
  val part2 = PosSet.numItems
    (foldSimulation grid (guardPosition grid) UP
       (findLoops grid (guardPosition grid)) PosSet.empty)

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
  val samplepart2 = PosSet.listItems
    (foldSimulation sampleGrid (guardPosition sampleGrid) UP
       (findLoops sampleGrid (guardPosition sampleGrid)) PosSet.empty)

end
