(* Assembles MIXAL instructions to MIX words *)

structure Cell =
struct
   datatype sign = Plus | Minus

   exception Cell_error of string

   (* represents a single MIX memory cell *)
   type t = {
     s : sign,  (* sign bit *)
     a1 : int,  (* first byte -- part of address field *)
     a2 : int,  (* second byte -- part of address field *)
     i : int,   (* index specification *)
     f : int,   (* field specification *)
     c : int    (* operation code *)
   }

  fun make_cell (f0 : sign, f1, f2, f3, f4, f5) =
    if f1 < 0 orelse f1 > 63 then
      raise (Cell_error "Invalid byte 1: MAKE_CELL")
    else if f2 < 0 orelse f2 > 63 then
      raise (Cell_error "Invalid byte 2: MAKE_CELL")
    else if f3 < 0 orelse f3 > 63 then
      raise (Cell_error "Invalid byte 3: MAKE_CELL")
    else if f4 < 0 orelse f4 > 63 then
      raise (Cell_error "Invalid byte 4: MAKE_CELL")
    else if f5 < 0 orelse f5 > 63 then
      raise (Cell_error "Invalid byte 5: MAKE_CELL")
    else
      { s = f0, a1 = f1, a2 = f2, i = f3, f = f4, c = f5 }

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
          sign_bit ^ (foldl op ^ "" ((map print_field [f1, f2, f3, f4, f5]) : string list))
    end
end
