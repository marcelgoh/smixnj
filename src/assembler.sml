(* Assembles MIXAL instructions to MIX words *)

signature ASSEMBLER =
sig
  val print_memory : unit -> unit
  val assemble : string list list -> Cell.t Array.array
end

structure Assembler :> ASSEMBLER =
struct
  structure A = Array
  structure H = HashTable

  exception Lookup_error

  (* the simulated MIX memory, all initialised to empty cells *)
  val memory = A.array (4000, Cell.empty)
  val curr_loc = ref 0

  (* print all non-empty cells in memory *)
  fun print_memory () =
    let
      (* iterate over memory locations from 0 to 3999 *)
      fun iter i =
        if i >= 4000 then
          ()
        else
          let
            val c = A.sub (memory, i)
          in
            if not (Cell.is_empty c) then (
              print (Format.format "%04d: " [Format.INT i]);
              print (Cell.to_string c);
              print "\n";
              iter (i + 1)
            ) else
              iter (i + 1)
          end
    in
      iter 0
    end

  (* each of the following functions assembles a line of MIXAL by modifying
   * the memory array at location !curr_loc
   *)
  fun nop_asm _ = ()

  (* the table that selects which function will assemble the MIXAL line *)
  val op_table : (string, (string list -> unit)) H.hash_table =
    let
      val t = H.mkTable (HashString.hashString, op=) (55, Lookup_error)
    in
      H.insert t ("NOP", nop_asm);
      t
    end

  fun assemble _ =
    let
      val _ = A.update (memory, 3, Cell.make (Cell.Minus, 34, 34, 21, 00, 01))
      val _ = print_memory ()
    in
      memory
    end

end
