(* Assembles MIXAL instructions to MIX words *)

signature CELL =
sig
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

  val empty : t

  val is_empty : t -> bool
  val make : sign * int * int * int * int * int -> t
  val value : int * int -> t -> int
  val to_string : t -> string
end

structure Cell : CELL =
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

  val empty = { s = Plus, a1 = 0, a2 = 0, i = 0, f = 0, c = 0 }

  (* checks if a cell is positive zero *)
  fun is_empty c =
    case c of
      { s = sign, a1 = f1, a2 = f2, i = f3, f = f4, c = f5 } =>
        sign = Plus andalso f1 = 0 andalso f2 = 0 andalso
        f3 = 0 andalso f4 = 0 andalso f5 = 0

  fun make (f0 : sign, f1, f2, f3, f4, f5) =
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

  fun int_exp b n = if n = 0 then 1 else (int_exp b (n - 1)) * b

  fun value (n1, n2) (cell : t) =
    if n1 > n2 orelse n1 < 0 orelse n2 > 5 then
      raise (Cell_error "Invalid slice: VALUE_OF_CELL")
    else
      case cell of
        { s = sign, a1 = f1, a2 = f2, i = f3, f = f4, c = f5 } =>
          let
            val pos_neg = if n1 = 0 then sign else Plus
            val place_vals = [f5, f4, f3, f2, f1]
            fun iter acc idx pow pvs =
              case pvs of
                [] => acc
              | (v :: vs) =>
                  if idx >= n1 andalso idx <= n2 then
                    iter ((int_exp 64 pow) * v + acc) (idx - 1) (pow + 1) vs
                  else
                    iter acc (idx - 1) (pow + 1) vs
            val value = iter 0 5 0 place_vals
          in
            case pos_neg of Plus => value | Minus => ~value
          end

  fun to_string (cell : t) =
    let
      val sign_bit =
        case (#s cell : sign) of
          Plus => "+"
        | Minus => "-"
      fun print_field i = Format.format "%02d " [Format.INT i]
    in
      case cell of
        { s = _, a1 = f1, a2 = f2, i = f3, f = f4, c = f5 } =>
          sign_bit ^ " " ^
          (foldr op ^ "" (map print_field [f1, f2, f3, f4, f5])) ^
          "(" ^ sign_bit ^ (Format.format "%010d" [Format.INT (value (1, 5) cell)]) ^ ")"
    end
end
