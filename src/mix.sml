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
      (* MIX does not distinguish between uppercase and lowercase letters *)
      val all_upper = map (fn s => String.map Char.toUpper s) out_strings
      (* a line of MIXAL is split into tokens on whitespace *)
      val split_strings = map (String.tokens is_space) all_upper
      (* print a list of strings -- for debugging *)
      fun print_list l = map (fn s => (print s; print " ")) l
    in
      map print_list split_strings; (* throwing away result of map *)
      Assembler.assemble split_strings;
      OS.Process.success
    end

  val _ = SMLofNJ.exportFn("_build/mix", mix)
end
