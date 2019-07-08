(* Assembles MIXAL instructions to MIX words *)

signature ASSEMBLER =
sig
  val assemble : string list list -> Cell.t Array.array
end

structure Assembler :> ASSEMBLER =
struct
  structure H = HashTable

  exception Lookup_error

  val op_table =
    let
      val t = H.mkTable (HashString.hashString, op=) (55, Lookup_error)
    in
      H.insert t ("NOP", (fn x => ()));
      t
    end

  fun assemble str_list =
    let 
      val msg =
        case H.find op_table "NOP" of
          SOME f => "OK.\n"
        | NONE => "Oh no.\n"
    in
      print msg;
      Array.fromList []
    end

end
