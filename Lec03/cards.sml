datatype suit = Club | Diamond | Heart | Spade
datatype card_value = Jack | Queen | King 
                    | Ace | Num of int

datatype card = Card of suit * card_value

val hands = [Card(Club, Jack), Card(Club, Num(10)), Card(Club, Ace)]
val hands2 = [Card(Club, Jack), Card(Diamond, Num(10)), Card(Club, Ace)]
val hands3 = [Card(Diamond, Num(10)), Card(Club, Ace), Card(Club, Jack)]
val hands4 = [Card(Diamond, Num(10)), Card(Club, Ace), Card(Club, Jack), Card(Spade, Ace)]

(* assume hand is not empty *)
(* returns true if suits of all cards are same 
*  e.g. [(Club, 10), (Club, King), (Club, Ace)] ==> true
*       [(Club, 10), (Diamond, King)] ==> false
    datatype card = Card of suit * card_value
*)
fun is_flush (hand: card list):bool =
	case hand of
       Card(_, _)::[] => true               (*base case *)
     | Card(s1, _)::Card(s2, v2)::rest =>    (* recursion *)
          s1=s2 andalso is_flush(Card(s2, v2)::rest)
     | [] =>  true                            (*base case *)



(* Ace=11, King, Queen, Jack=10, other cards=numbers *)
fun simpleSum(hand: card list):int =
  case hand of
    Card (_, Ace)::rest => 11 + simpleSum(rest)
  | Card (_, Num(i))::rest => i + simpleSum(rest)
  | Card (_, _)::rest     => 10 +simpleSum(rest)
  | [] => 0

(* card list -> bool *)
fun hasAce (hand: card list) =
    case hand of
    Card (_, Ace)::rest => true
  | Card (_, _)::rest   => hasAce(rest)
  | [] => false


(* ten king ace ace 9 8 ==> ten king ace 9 8 *)
(* card list -> card list *)
fun removeFirstAce (hand: card list):card list = 
    case hand of
    Card (_, Ace)::rest => rest
  | Card (s, v)::rest   => Card(s, v)::removeFirstAce(rest)
  | [] => []


(* blackjack first try.
*  If the sum > 21, try removing an ace card and calculate sum again.
*  We use hasAce and removeFirstAce function
*)
fun blackjack(val threshold:int, hand: card list):int =
let val score = simpleSum(hand)
in
  if score > threshold andalso hasAce hand
  then 1+blackjack(threshold-1, removeFirstAce hand)
  else score
end



(* second try *)
fun blackjack(score:int, hand: card list) =
    case hand of
     [] => score
   | Card(_, Ace)::rest => let val try1= blackjack(11+score, rest)
                           in
                              if try1<=21
                              then try1
                              else blackjack(1+score, rest)
                           end
   | Card(_, Num(i))::rest => blackjack(i+score, rest)
   | Card(_, _)::rest => blackjack(10+score, rest)


