(* Assembles MIXAL instructions to MIX words *)

signature ASSEMBLER =
sig
  exception Assembler_error

  val print_memory : unit -> unit
  val assemble : string list list -> (Cell.t Array.array) * int
end

structure Assembler :> ASSEMBLER =
struct
  structure A = Array
  structure H = HashTable

  exception Assembler_error
  (* local exceptions *)
  exception End_of_file
  exception Bad_symbol of string * int   (* offending string and line *)
  exception Missing_symbol of int        (* line of missing symbol *)
  exception Lookup_error
  exception Undefined_symbol of string * int (* string and line *)

  (* the simulated MIX memory, all initialised to empty cells *)
  val memory = A.array (4000, Cell.empty)
  (* the starting location of the program *)
  val start_loc = ref 0

  (* keeps track of known symbols and the lines they indicate *)
  val symbols : (string, int) H.hash_table =
    H.mkTable (HashString.hashString, op=) (20, Lookup_error)
  fun print_entry (s, i) =
    print (Format.format "%s is line %04d\n" [Format.STR s, Format.INT i])
  (* current memory location at which we are assembling *)
  val curr_loc = ref 0

  (* print all non-empty cells in memory *)
  fun print_memory () =
    let
      (* iterate over memory locations from 0 to 3999 *)
      fun iter i =
        if i >= 4000 then ()
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
  fun nop_asm ss =
    let
      fun print_list l = map (fn s => (print s; print " ")) l
    in
      print_list ss;
      print "\n"
    end

  (* this table indicates the function that will assemble the MIXAL line *)
  val op_table : (string, (string list -> unit)) H.hash_table =
    let
      val t = H.mkTable (HashString.hashString, op=) (55, Lookup_error)
    in
      H.insert t ("NOP", nop_asm);    (* C = 00 *)
      H.insert t ("ADD", nop_asm);    (* C = 01 *)
      H.insert t ("SUB", nop_asm);    (* C = 02 *)
      H.insert t ("MUL", nop_asm);    (* C = 03 *)
      H.insert t ("DIV", nop_asm);    (* C = 04 *)
      H.insert t ("NUM", nop_asm);    (* C = 05 *)
      H.insert t ("CHAR", nop_asm);
      H.insert t ("HLT", nop_asm);
      H.insert t ("SLA", nop_asm);    (* C = 06 *)
      H.insert t ("SRA", nop_asm);
      H.insert t ("SLAX", nop_asm);
      H.insert t ("SRAX", nop_asm);
      H.insert t ("SLC", nop_asm);
      H.insert t ("SRC", nop_asm);
      H.insert t ("MOVE", nop_asm);   (* C = 07 *)

      H.insert t ("LDA", nop_asm);    (* C = 08 *)
      H.insert t ("LD1", nop_asm);    (* C = 09 *)
      H.insert t ("LD2", nop_asm);    (* C = 10 *)
      H.insert t ("LD3", nop_asm);    (* C = 11 *)
      H.insert t ("LD4", nop_asm);    (* C = 12 *)
      H.insert t ("LD5", nop_asm);    (* C = 13 *)
      H.insert t ("LD6", nop_asm);    (* C = 14 *)
      H.insert t ("LDX", nop_asm);    (* C = 15 *)

      H.insert t ("LDAN", nop_asm);   (* C = 16 *)
      H.insert t ("LD1N", nop_asm);   (* C = 17 *)
      H.insert t ("LD2N", nop_asm);   (* C = 19 *)
      H.insert t ("LD3N", nop_asm);   (* C = 19 *)
      H.insert t ("LD4N", nop_asm);   (* C = 20 *)
      H.insert t ("LD5N", nop_asm);   (* C = 21 *)
      H.insert t ("LD6N", nop_asm);   (* C = 22 *)
      H.insert t ("LDXN", nop_asm);   (* C = 23 *)

      H.insert t ("STA", nop_asm);    (* C = 24 *)
      H.insert t ("ST1", nop_asm);    (* C = 25 *)
      H.insert t ("ST2", nop_asm);    (* C = 26 *)
      H.insert t ("ST3", nop_asm);    (* C = 27 *)
      H.insert t ("ST4", nop_asm);    (* C = 28 *)
      H.insert t ("ST5", nop_asm);    (* C = 29 *)
      H.insert t ("ST6", nop_asm);    (* C = 30 *)
      H.insert t ("STX", nop_asm);    (* C = 31 *)

      H.insert t ("STJ", nop_asm);    (* C = 32 *)
      H.insert t ("STZ", nop_asm);    (* C = 33 *)
      H.insert t ("JBUS", nop_asm);   (* C = 34 *)
      H.insert t ("IOC", nop_asm);    (* C = 35 *)
      H.insert t ("IN", nop_asm);     (* C = 36 *)
      H.insert t ("OUT", nop_asm);    (* C = 37 *)
      H.insert t ("JRED", nop_asm);   (* C = 38 *)
      H.insert t ("JMP", nop_asm);    (* C = 39 *)
      H.insert t ("JSJ", nop_asm);
      H.insert t ("JOV", nop_asm);
      H.insert t ("JNOV", nop_asm);
      H.insert t ("JL", nop_asm);
      H.insert t ("JE", nop_asm);
      H.insert t ("JG", nop_asm);
      H.insert t ("JGE", nop_asm);
      H.insert t ("JNE", nop_asm);
      H.insert t ("JLE", nop_asm);

      H.insert t ("JAN", nop_asm);    (* C = 40 *)
      H.insert t ("JAZ", nop_asm);
      H.insert t ("JAP", nop_asm);
      H.insert t ("JANN", nop_asm);
      H.insert t ("JANZ", nop_asm);
      H.insert t ("JANP", nop_asm);

      H.insert t ("J1N", nop_asm);    (* C = 41 *)
      H.insert t ("J1Z", nop_asm);
      H.insert t ("J1P", nop_asm);
      H.insert t ("J1NN", nop_asm);
      H.insert t ("J1NZ", nop_asm);
      H.insert t ("J1NP", nop_asm);

      H.insert t ("J2N", nop_asm);    (* C = 42 *)
      H.insert t ("J2Z", nop_asm);
      H.insert t ("J2P", nop_asm);
      H.insert t ("J2NN", nop_asm);
      H.insert t ("J2NZ", nop_asm);
      H.insert t ("J2NP", nop_asm);

      H.insert t ("J3N", nop_asm);    (* C = 43 *)
      H.insert t ("J3Z", nop_asm);
      H.insert t ("J3P", nop_asm);
      H.insert t ("J3NN", nop_asm);
      H.insert t ("J3NZ", nop_asm);
      H.insert t ("J3NP", nop_asm);

      H.insert t ("J4N", nop_asm);    (* C = 44 *)
      H.insert t ("J4Z", nop_asm);
      H.insert t ("J4P", nop_asm);
      H.insert t ("J4NN", nop_asm);
      H.insert t ("J4NZ", nop_asm);
      H.insert t ("J4NP", nop_asm);

      H.insert t ("J5N", nop_asm);    (* C = 45 *)
      H.insert t ("J5Z", nop_asm);
      H.insert t ("J5P", nop_asm);
      H.insert t ("J5NN", nop_asm);
      H.insert t ("J5NZ", nop_asm);
      H.insert t ("J5NP", nop_asm);

      H.insert t ("J6N", nop_asm);    (* C = 46 *)
      H.insert t ("J6Z", nop_asm);
      H.insert t ("J6P", nop_asm);
      H.insert t ("J6NN", nop_asm);
      H.insert t ("J6NZ", nop_asm);
      H.insert t ("J6NP", nop_asm);

      H.insert t ("JXN", nop_asm);    (* C = 47 *)
      H.insert t ("JXZ", nop_asm);
      H.insert t ("JXP", nop_asm);
      H.insert t ("JXNN", nop_asm);
      H.insert t ("JXNZ", nop_asm);
      H.insert t ("JXNP", nop_asm);

      H.insert t ("INCA", nop_asm);   (* C = 48 *)
      H.insert t ("DECA", nop_asm);
      H.insert t ("ENTA", nop_asm);
      H.insert t ("ENNA", nop_asm);

      H.insert t ("INC1", nop_asm);   (* C = 49 *)
      H.insert t ("DEC1", nop_asm);
      H.insert t ("ENT1", nop_asm);
      H.insert t ("ENN1", nop_asm);

      H.insert t ("INC2", nop_asm);   (* C = 50 *)
      H.insert t ("DEC2", nop_asm);
      H.insert t ("ENT2", nop_asm);
      H.insert t ("ENN2", nop_asm);

      H.insert t ("INC3", nop_asm);   (* C = 51 *)
      H.insert t ("DEC3", nop_asm);
      H.insert t ("ENT3", nop_asm);
      H.insert t ("ENN3", nop_asm);

      H.insert t ("INC4", nop_asm);   (* C = 52 *)
      H.insert t ("DEC4", nop_asm);
      H.insert t ("ENT4", nop_asm);
      H.insert t ("ENN4", nop_asm);

      H.insert t ("INC5", nop_asm);   (* C = 53 *)
      H.insert t ("DEC5", nop_asm);
      H.insert t ("ENT5", nop_asm);
      H.insert t ("ENN5", nop_asm);

      H.insert t ("INC6", nop_asm);   (* C = 54 *)
      H.insert t ("DEC6", nop_asm);
      H.insert t ("ENT6", nop_asm);
      H.insert t ("ENN6", nop_asm);

      H.insert t ("INCX", nop_asm);   (* C = 55 *)
      H.insert t ("DECX", nop_asm);
      H.insert t ("ENTX", nop_asm);
      H.insert t ("ENNX", nop_asm);

      H.insert t ("CMPA", nop_asm);   (* C = 56 *)
      H.insert t ("CMP1", nop_asm);   (* C = 57 *)
      H.insert t ("CMP2", nop_asm);   (* C = 58 *)
      H.insert t ("CMP3", nop_asm);   (* C = 59 *)
      H.insert t ("CMP4", nop_asm);   (* C = 60 *)
      H.insert t ("CMP5", nop_asm);   (* C = 61 *)
      H.insert t ("CMP6", nop_asm);   (* C = 62 *)
      H.insert t ("CMPX", nop_asm);   (* C = 63 *)

      H.insert t ("ALF", nop_asm);    (* non-instruction words *)
      H.insert t ("CON", nop_asm);

      H.insert t ("ORIG", nop_asm);
      (* END, EQU handled separately *)

      t (* return the filled table *)
    end

  fun add_symbol str = H.insert symbols (str, !curr_loc)

  fun add_equ line = ()

  (* takes a list of lists of strings and return the modified MIX memory *)
  fun assemble list_list =
    let
      (* get index of first occurrence of str in l, or NONE if not found *)
      fun first_opt acc l str =
        case l of
          [] => NONE
        | s :: ss =>
            if s = str then SOME acc
            else first_opt (acc + 1) ss str
      (* iterate down the list of lines *)
      fun iter line_no ll =
        let
          fun print_line_no () = print (Format.format "%d\n" [Format.INT line_no])
          fun print_list_list ls =
            map (fn l =>
                   let
                     val _ = print "["
                     val _ = map (fn s => print (Format.format "\"%s\"," [Format.STR s])) l
                   in
                     print "]\n"
                   end)
                ls
          (* increment location then call iter on rest *)
          fun next rest =
            let
              val _ = curr_loc := !curr_loc + 1
            in
              iter (line_no + 1) rest
            end
        in
        case ll of
          [] => raise End_of_file
        | l :: ls =>
            if List.exists (fn x => x = "EQU") l then (
              add_equ l;
              next ls
            )
            else
              (case first_opt 0 l "END" of
                 SOME i =>
                   (case List.drop (l, i + 1) of
                      [] => raise (Bad_symbol ("END", line_no))
                    | sym :: _ =>
                        (case H.find symbols sym of
                           SOME loc =>
                             start_loc := loc (* CLEAN EXIT *)
                         | NONE =>
                             raise (Undefined_symbol (sym, line_no))))
               | NONE =>
                   (case l of
                      [] => next ls (* line is empty *)
                    | s1 :: drop1 =>
                        (
                        if String.size s1 > 0 andalso String.sub (s1, 0) = #"*" then
                          next ls (* ignore line if asterisk at start *)
                        else
                          (case drop1 of
                             [] => (* line has only one string *)
                               (case H.find op_table s1 of
                                  SOME f => (
                                    print "Case 1: ";
                                    f l;  (* call f on the whole line *)
                                    next ls
                                  )
                                  | NONE => raise (Missing_symbol line_no))
                           | s2 :: _ =>
                               (case H.find op_table s1 of
                                  SOME f =>
                                    (case H.find op_table s2 of
                                       SOME g => raise (Bad_symbol (s1, line_no))
                                     | NONE =>
                                         print "Case 2: ";
                                         f l; (* no symbol, just assemble line *)
                                         next ls)
                                | NONE =>
                                    (case H.find op_table s2 of
                                       SOME f => (
                                         add_symbol s1;
                                         print "Case 3: ";
                                         f drop1; (* assemble line without symbol *)
                                         next ls
                                       )
                                     | NONE =>
                                         raise (Missing_symbol line_no))))))
                         )
        end
    in
      iter 1 list_list;
      H.appi print_entry symbols;
      (memory, !start_loc)
    end
    (* print appropriate error message then signal that assembly has failed *)
    handle
      End_of_file =>
        let
          val _ = print "End of file reached before finding END.\n"
        in
          raise Assembler_error
        end
    | Bad_symbol (s, i) =>
        let
          val _ = print (Format.format "Bad symbol `%s` on line %d.\n"
                                       [Format.STR s, Format.INT i])
        in
          raise Assembler_error
        end
    | Missing_symbol i =>
        let
          val _ = print (Format.format "Missing symbol on line %d.\n"
                                       [Format.INT i])
        in
          raise Assembler_error
        end
    | Lookup_error =>
        let
          val _ = print "Hashtable lookup failed."
        in
          raise Assembler_error
        end
    | Undefined_symbol (s, i) =>
        let
          val _ = print (Format.format "Undefined symbol `%s` on line %d.\n"
                                       [Format.STR s, Format.INT i])
        in
          raise Assembler_error
        end
end
