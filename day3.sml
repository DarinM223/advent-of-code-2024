structure Day3 =
struct
  val input =
    let
      val lines: string list ref = ref []
    in
      Utils.readLines (fn line => lines := line :: !lines) "inputs/day3.txt";
      String.concat (List.rev (!lines))
    end

  val pos = ref 0

  fun getCh () : char =
    String.sub (input, !pos)
  fun inc () =
    pos := !pos + 1

  fun expect (s: string) : unit option =
    let
      fun go (si: int) : bool =
        (si >= String.size s)
        orelse
        let
          val sch = String.sub (s, si)
          val ech = getCh ()
        in
          sch = ech andalso (inc (); go (si + 1))
        end
    in
      if go 0 then SOME () else NONE
    end

  fun parseNumber () : int option =
    let
      fun go (acc: int) =
        let
          val ch = getCh ()
          val digit = Char.ord ch - Char.ord #"0"
        in
          if (Char.isDigit ch) then (inc (); go (acc * 10 + digit))
          else SOME acc
        end
    in
      if not (Char.isDigit (getCh ())) then NONE else go 0
    end

  infix >>=
  fun op>>= (m, f) =
    case m of
      SOME a => f a
    | NONE => NONE

  fun parseMul () : int option =
    expect "mul("
    >>=
    (fn () =>
       parseNumber ()
       >>=
       (fn a =>
          expect ","
          >>=
          (fn () =>
             parseNumber () >>= (fn b => expect ")" >>= (fn () => SOME (a * b))))))


  val part1 =
    let
      val sumOfMults = ref 0
    in
      while !pos < String.size input do
        case parseMul () of
          SOME mult => sumOfMults := mult + !sumOfMults
        | NONE => if getCh () <> #"m" then inc () else ();
      !sumOfMults
    end

  (* Reset position back to 0 for parsing part2 *)
  val () = pos := 0

  val part2 =
    let
      val sumOfMults = ref 0
      val enabled = ref true
    in
      while !pos < String.size input do
        case getCh () of
          #"m" =>
            (case parseMul () of
               SOME mult =>
                 if !enabled then sumOfMults := mult + !sumOfMults else ()
             | NONE => ())
        | #"d" =>
            let
              val savedPos = !pos
            in
              case expect "do()" of
                SOME () => enabled := true
              | NONE =>
                  ( pos := savedPos
                  ; case expect "don't()" of
                      SOME () => enabled := false
                    | NONE => ()
                  )
            end
        | _ => inc ();
      !sumOfMults
    end

  fun results () =
    ( print ("Part 1 number of multiplications: " ^ Int.toString part1 ^ "\n")
    ; print ("Part 2 number of multiplications: " ^ Int.toString part2 ^ "\n")
    )
end
