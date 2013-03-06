type 'a arbitrary =
    { generate    : (unit -> 'a)
    ; to_string   : ('a -> string)
    ; description : string
(*    ; shrink    : ('a -> 'a list)*)
    }

type property = unit -> unit

exception Counter_example of (string * string) list

let forall domain predicate () =
  let value = domain.generate () in
  try predicate value ()
  with Counter_example l ->
    let str  = domain.to_string value in
    let desc = domain.description in
    raise (Counter_example ((str,desc)::l))

let check_property property =
  try
    for i = 1 to 1000 do property () done;
    `OkAsFarAsIKnow
  with Counter_example l ->
    `CounterExample l

let check b () =
  if not b then raise (Counter_example [])

let ( ^$ ) f x = f x

module Arbitrary = struct
  let unit =
    let generate () = ()
    and to_string () = "()"
    and description = "unit"
    in {generate; to_string; description}

  let bool =
    let generate () = Random.bool ()
    and to_string = function true -> "true" | false -> "false"
    and description = "bool"
    in
    {generate; to_string; description}

  let list a =
    let generate () =
        let length = Random.int 20 in
        let rec loop acc = function
          | 0 -> acc
          | n -> loop (a.generate () :: acc) (n-1)
        in
        loop [] length
    and to_string l =
      "[" ^ String.concat "," (List.map a.to_string l) ^ "]"
    and description = a.description ^ " list"
    in
    {generate; to_string; description}

  let char =
    let generate () = Char.chr (Random.int 256)
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "char" in
    {generate; to_string; description}

  let printable_ascii_char =
    let generate () = Char.chr (Random.int (128 - 32) + 32)
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "printable_ascii_char" in
    {generate; to_string; description}

  let string =
    let generate () =
      let length = Random.int 20 in
      let str    = String.create length in
      for i = 0 to length-1 do
        str.[i] <- char.generate ()
      done;
      str
    and to_string s = "\"" ^ String.escaped s ^ "\""
    and description = "string" in
    {generate; to_string; description}

  let printable_ascii_string =
    let generate () =
      let length = Random.int 20 in
      let str    = String.create length in
      for i = 0 to length-1 do
        str.[i] <- printable_ascii_char.generate ()
      done;
      str
    and to_string s = String.escaped s
    and description = "printable_ascii_string" in
    {generate; to_string; description}

  let array a =
    let generate () =
      let length = Random.int 20 in
      Array.init length (fun _ -> a.generate ())
    and to_string arr =
      let b = Buffer.create (Array.length arr * 10) in
      Buffer.add_string b "[|";
      Array.iteri
        (fun i x ->
          if i <> 0 then Buffer.add_char b ',';
          Buffer.add_string b (a.to_string x))
        arr;
      Buffer.add_string b "|]";
      Buffer.contents b
    and description = a.description ^ " array"
    in
    {generate; to_string; description}

  let option a =
    let generate () =
      if Random.int 10 = 0 then None else Some (a.generate ())
    and to_string = function
      | None -> "None"
      | Some v -> "Some (" ^ a.to_string v ^ ")"
    and description = a.description ^ " option"
    in
    {generate;to_string;description}

  let int_range minimum maximum =
    let generate () =
      (* FIXME: overflow *)
      Random.int (maximum - minimum + 1) + minimum
    and to_string i = string_of_int i
    and description =
      Printf.sprintf "int_range(%d,%d)" minimum maximum
    in
    {generate; to_string; description}
end
