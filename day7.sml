structure Day7 =
struct
  val input: (int * int list) list =
    let
      val input: (int * int list) list ref = ref []
    in
      Utils.readLines
        (fn line =>
           case String.tokens (fn ch => ch = #":") line of
             [target, nums] =>
               (case
                  ( Int.fromString target
                  , List.mapPartial Int.fromString (Utils.words nums)
                  )
                of
                  (SOME target, nums) => input := (target, nums) :: !input
                | _ => raise Fail "Couldn't parse numbers")
           | _ => raise Fail "Invalid line") "inputs/day7.txt";
      List.rev (!input)
    end

  fun countDigits num =
    let
      fun go build num =
        if num = 0 then build
        else go (if build = 0 then 10 else build * 10) (num div 10)
    in
      go 0 num
    end

  infix ||
  fun a || b =
    a * countDigits b + b

  fun canProduceTarget (ops: ((int * int) -> int) list) target (e :: es) =
        let
          fun go build (e :: es) =
                List.exists (fn f => go (f (build, e)) es) ops
            | go build [] = build = target
        in
          go e es
        end
    | canProduceTarget _ target [] = target = 0

  val part1 = List.foldl op+ 0 (List.map #1
    (List.filter (fn (target, nums) => canProduceTarget [op+, op*] target nums)
       input))

  val part2 = List.foldl op+ 0 (List.map #1
    (List.filter
       (fn (target, nums) => canProduceTarget [op+, op*, op||] target nums)
       input))

  fun results () =
    ( print ("Part 1 total calibration result: " ^ Int.toString part1 ^ "\n")
    ; print ("Part 2 total calibration result: " ^ Int.toString part2 ^ "\n")
    )
end
