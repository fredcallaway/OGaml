(* handles traveling, going to shop, going to battle *)

open Player
open Zone


type t = {
  zones : Zone.t list;
  player : Player.t;
}