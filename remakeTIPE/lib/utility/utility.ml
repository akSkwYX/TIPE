let string_tail s = String.sub s 1 (String.length s - 1)

let string_of_option = function
  | Some s -> s
  | None -> ""

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
