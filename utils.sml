structure Utils =
struct
  val trim =
    let
      fun trim ch = ch = #" " orelse ch = #"\n"
    in
      Substring.string o Substring.dropr trim o Substring.dropl trim
      o Substring.full
    end

  fun readLines (f: string -> unit) (path: string) =
    let
      val ins = TextIO.openIn path
      fun loop ins =
        case TextIO.inputLine ins of
          SOME line => (f line; loop ins)
        | NONE => ()
    in
      loop ins
    end
  fun readLinesTrimmed (f: string -> unit) (path: string) =
    let
      val ins = TextIO.openIn path
      fun loop ins =
        case TextIO.inputLine ins of
          SOME line => (f (trim line); loop ins)
        | NONE => ()
    in
      loop ins
    end

  val words = String.tokens (fn ch => ch = #" ")

  fun printList (show: 'a -> string) (l: 'a list) =
    List.app (fn e => print (show e ^ "\n")) l
end
