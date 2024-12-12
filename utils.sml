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

  fun memoize (hash: 'a -> word, sameKey: 'a * 'a -> bool)
    (f: ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
    let
      exception NotFound
      val cache: ('a, 'b) HashTable.hash_table =
        HashTable.mkTable (hash, sameKey) (1000, NotFound)
      fun g x =
        HashTable.lookup cache x
        handle NotFound => let val result = f g x
                           in HashTable.insert cache (x, result); result
                           end
    in
      g
    end
end
