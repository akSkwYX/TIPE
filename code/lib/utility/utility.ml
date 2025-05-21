let string_tail s = String.sub s 1 (String.length s - 1)

let string_of_option = function
  | Some s -> s
  | None -> ""

let string_without_x_first_char x s =
  String.sub s x (String.length s - x)

let count f l =
  List.fold_left ( fun acc x -> if f x then acc+1 else acc) 0 l

let join_2 f l1 l2 =
  List.rev (
    List.fold_left (
      fun res x ->
        List.fold_left (
          fun res2 y ->
            f x y :: res2
        ) res l2
    ) [] l1
  )

(*TODO : probably don't working well*)
let join_3 f l1 l2 l3 =
  List.fold_left (
    fun res x ->
      List.fold_left (
        fun res2 y ->
          List.fold_left (
            fun res3 z ->
               f x y z :: res3
          ) res2 l3
      ) res l2
  ) [] l1
