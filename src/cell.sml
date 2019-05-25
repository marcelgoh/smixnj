(* Assembles MIXAL instructions to MIX words *)

structure Cell =
struct
   datatype sign = Plus | Minus

   (* represents a single MIX memory cell *)
   type t = {
     s : sign,  (* sign bit *)
     a1 : int,  (* first byte -- part of address field *)
     a2 : int,  (* second byte -- part of address field *)
     i : int,   (* index specification *)
     f : int,   (* field specification *)
     c : int    (* operation code *)
   }

  fun str_of_cell (cell : t) =
    let
      val sign_bit =
        case (#s cell : sign) of
          Plus => "+ "
        | Minus => "- "
      fun print_field i = Int.toString i ^ " "
    in
      case cell of
        { s = _, a1 = f1, a2 = f2, i = f3, f = f4, c = f5 } =>
          foldl op ^ sign_bit ((map print_field [f1, f2, f3, f4, f5]) : string list)
    end
end
