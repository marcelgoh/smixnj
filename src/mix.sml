(* Front-end driver *)

signature MIX =
sig
  val mix : string * string list -> OS.Process.status
end

structure Mix :> MIX =
struct
  exception File_error

  fun mix (arg0, argv) =
    let
      val argc = List.length argv
      fun is_space c = c = #" " orelse c = #"\t" orelse c = #"\r"
      (* TextIO.inputLine returns strings that are guaranteed to terminate
       * with newline, so this function removes it *)
      fun remove_last_char s =
        let
          val len = String.size s
        in
          String.substring (s, 0, len - 1)
        end
      (* reads file and returns list of lists of strings *)
      fun read_file () =
        case argv of
          [filename] =>
            let
              val in_stream = TextIO.openIn filename
              fun loop ins =
                case TextIO.inputLine ins of
                  SOME line => line :: (loop ins)
                | NONE => []
              val out_strings = loop in_stream before TextIO.closeIn in_stream
              (* MIX does not distinguish between uppercase and lowercase letters *)
              val all_upper = map (fn s => String.map Char.toUpper
                                                      (remove_last_char s))
                                  out_strings
            in
              (* a line of MIXAL is split into tokens on whitespace *)
              map (fn line =>
                     List.filter (fn s => s <> "") (String.tokens is_space line))
                  all_upper
            end
        | _ => (
            print "Usage: ./mix <filename>\n";
            raise File_error
          )
    in
      Assembler.assemble (read_file ());  (* read file and then assemble *)
      OS.Process.success
    end
    handle
      File_error => OS.Process.failure

  val _ = SMLofNJ.exportFn("_build/mix", mix)
end
