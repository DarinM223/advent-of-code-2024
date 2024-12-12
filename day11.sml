structure Day11 =
struct
  val input: int list =
    let
      val nums: int list ref = ref []
    in
      Utils.readLinesTrimmed
        (fn line => nums := List.mapPartial Int.fromString (Utils.words line))
        "inputs/day11.txt";
      !nums
    end

  fun numDigits (n: int) : int =
    let
      fun go count n =
        if n = 0 then count else go (count + 1) (n div 10)
    in
      go 0 n
    end

  fun splitDigit (n: int) : int * int =
    let
      fun go build (n: int) (tensExp: int) (digits: int) : int * int =
        case digits of
          0 => (n, build)
        | _ =>
            go ((n mod 10) * tensExp + build) (n div 10) (tensExp * 10)
              (digits - 1)
    in
      go 0 n 1 (numDigits n div 2)
    end

  fun blinkStep (stone :: rest) =
        if stone = 0 then
          1 :: blinkStep rest
        else if numDigits stone mod 2 = 0 then
          let val (left, right) = splitDigit stone
          in left :: right :: blinkStep rest
          end
        else
          stone * 2024 :: blinkStep rest
    | blinkStep [] = []

  val sample = List.length (List.foldl (fn (_, acc) => blinkStep acc) [125, 17]
    (List.tabulate (25, fn i => i)))

  val part1 = List.length (List.foldl (fn (_, acc) => blinkStep acc) input
    (List.tabulate (25, fn i => i)))

  type params = int * int
  val params = let open Generic in tuple2 (int, int) end

  (* Dynamic programming problem, memoize over stone and current step *)
  fun countStone self (stone, step) =
    if step = 75 then
      1
    else if stone = 0 then
      self (1, step + 1)
    else if numDigits stone mod 2 = 0 then
      let val (left, right) = splitDigit stone
      in self (left, step + 1) + self (right, step + 1)
      end
    else
      self (stone * 2024, step + 1)

  val countStone' =
    Utils.memoize (Word32.toWord o Generic.hash params, op=) countStone

  val part2 = List.foldl (fn (num, acc) => acc + countStone' (num, 0)) 0 input

  fun results () =
    ( print ("Part 1 stones after 25 blinks: " ^ Int.toString part1 ^ "\n")
    ; print ("Part 2 stones after 75 blinks: " ^ Int.toString part2 ^ "\n")
    )
end
