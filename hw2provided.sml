(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Problem 1: a *)
fun all_except_option (_, []) = NONE
  | all_except_option (key, x::xs) =
    if key = x then SOME(xs) else
    case all_except_option(key, xs)
     of NONE => NONE
      | SOME v => SOME(x::v);

(* Problem 1:b *)
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (x::xs, key) =
    case all_except_option(key, x)
     of NONE => get_substitutions1(xs, key)
      | SOME v => v @ get_substitutions1(xs, key)

(* Problem 1:c *)
fun get_substitutions2 (data, key) =
    let
        fun tail ([], lst) = lst
          | tail (x::xs, lst) =
            case all_except_option(key, x)
             of NONE => tail(xs, lst)
              | SOME v => tail(xs, lst @ v)
    in
        tail(data, [])
    end;

(* Problem 1:d *)
fun similar_names (data, name) =
     let
         val {first = a, middle = b, last = c} = name;
         fun tail ([], lst) = lst
           | tail (x::xs, lst) =
             tail(xs, lst @ [{first = x, middle = b, last = c}]);
     in
         name::tail(get_substitutions2(data, a), [])
     end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(* Problem 2:a *)
fun card_color (data, _) =
    case data
     of Diamonds => Red
      | Hearts => Red
      | _ => Black;

(* Problem 2:b *)
fun card_value (_, data) =
    case data
     of Num x => x
      | Ace => 11
      | _ => 10;

(* Problem 2:c *)
fun remove_card (cs, c, e) =
    case all_except_option(c, cs)
     of NONE => raise e
     | SOME a => a


(* Problem 2:d *)
fun  all_same_color ([]) = true
   | all_same_color (_::[]) = true
   | all_same_color (x::y::cs) =
     case card_color x = card_color y
      of true => all_same_color(y::cs)
       | false => false

(* Problem 2:e *)
fun sum_cards cs =
    let
        fun aux ([], acc) = acc
          | aux (x::xs, acc) =
            aux(xs, card_value x + acc)
    in
        aux(cs, 0)
    end;

(* Problem 2:f *)
fun p_score (cs, goal, ace) =
    let
        val sum = (sum_cards cs) - ace * 10;
        val preliminary =
            case sum > goal
             of true => (sum - goal) * 3
              | false => goal - sum;
    in
        case all_same_color cs
         of true => preliminary div 2
          | false => preliminary
    end;

fun score (cs, goal) = p_score(cs, goal, 0);

(* Problem 3:a-1 *)
fun count_ace ([], ace) = ace
  | count_ace ((_, Ace)::cards, ace) =
    count_ace(cards, ace + 1)
  | count_ace (_::cards, ace) =
    count_ace(cards, ace);

fun score_challenge (cs, goal) =
    let
        fun try_score (result, ace) =
            if ace < 0 then result else
            try_score (Int.min(result, p_score(cs, goal, ace)), ace - 1);
        val count = count_ace(cs, 0)
    in
        try_score(p_score(cs, goal, count), count - 1)
    end;

(* Problem 2:g *)
fun run (cards, holds, moves, goal, sum_fn, score_fn) =
    case (cards, moves, sum_fn(holds) > goal)
     of (card::cards', Draw::moves', false) => run(cards', card::holds, moves', goal, sum_fn, score_fn)
      | (_, Discard card::moves', false) => run(cards, remove_card(holds, card, IllegalMove), moves', goal, sum_fn, score_fn)
      | _ => score_fn(holds, goal)

fun officiate (card_list, move_list, goal) =
    run(card_list, [], move_list, goal, sum_cards, score);

(* Problem 3:a-2 *)
fun min_sum (cs) = sum_cards cs - count_ace (cs, 0) * 10;

fun officiate_challenge (card_list, move_list, goal) =
    run(card_list, [], move_list, goal, min_sum, score_challenge);
