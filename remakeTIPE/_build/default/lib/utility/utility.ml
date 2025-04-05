let string_tail s = String.sub s 1 (String.length s - 1)

let string_of_option = function
  | Some s -> s
  | None -> ""
