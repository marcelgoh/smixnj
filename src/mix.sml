(* Front-end driver *)

signature MIX =
sig
  val mix : string * string list -> OS.Process.status
end

structure Mix :> MIX =
struct
  fun mix (arg0, argv) =
    let
      val argc = List.length argv
      val out_strings =
        case argv of
          [filename] =>
            let
              val in_stream = TextIO.openIn filename
              fun loop ins =
                case TextIO.inputLine ins of
                  SOME line => line :: (loop ins)
                | NONE => []
            in
              loop in_stream before TextIO.closeIn in_stream
            end
        | _ =>
            ["Usage: ./mix <filename>\n"]
      fun is_space c = c = #" " orelse c = #"\t" orelse c = #"\r"
      val split_strings = map (String.tokens is_space) out_strings
      fun print_list l = map (fn s => (print s; print " ")) l
    in
      map print_list split_strings; (* throwing away result of map *)
      Assembler.assemble split_strings;
      let
        val c1 = Cell.make_cell (Cell.Minus, 34, 23, 45, 23, 00)
        val c2 = Cell.make_cell (Cell.Minus, 20, 26, 07, 51, 23)
        val c3 = Cell.make_cell (Cell.Plus, 14, 17, 00, 52, 15)
      in
        print (Cell.str_of_cell c1);
        print "\n";
        print (Cell.str_of_cell c2);
        print "\n";
        print (Cell.str_of_cell c3);
        print "\n"
      end;
      OS.Process.success
    end

  val _ = SMLofNJ.exportFn("_build/mix", mix)
end
