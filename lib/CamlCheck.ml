module Generator : sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (<$>)  : ('a -> 'b) -> 'a t -> 'b t
  val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t

  val fail : 'a t
  val bits : int t
  val int : int -> int t
  val int32 : int32 -> int32 t
  val nativeint : nativeint -> nativeint t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val bool : bool t
  val array : int -> (int -> 'a t) -> 'a array t

  val run : 'a t -> Random.State.t -> 'a option
end = struct
  type 'a t = Random.State.t -> 'a option

  let run generator random_state =
    generator random_state

  let fail random_state =
    None

  let return a random_state = Some a
  let (>>=) c f random_state =
    match c random_state with
      | None   -> None
      | Some a -> f a random_state
  let (<$>) f x random_state =
    match x random_state with
      | None -> None
      | Some a -> Some (f a)
  let (<*>) f x random_state =
    match f random_state with
      | None -> None
      | Some f ->
        match x random_state with
          | None -> None
          | Some a -> Some (f a)

  let bits random_state =
    Some (Random.State.bits random_state)

  let int bound random_state =
    Some (Random.State.int random_state bound)

  let int32 bound random_state =
    Some (Random.State.int32 random_state bound)

  let nativeint bound random_state =
    Some (Random.State.nativeint random_state bound)

  let int64 bound random_state =
    Some (Random.State.int64 random_state bound)

  let float bound random_state =
    Some (Random.State.float random_state bound)

  let bool random_state =
    Some (Random.State.bool random_state)

  exception Fail

  let array length f random_state =
    try
      Some (Array.init length
              (fun i ->
                match f i random_state with
                  | None -> raise Fail
                  | Some x -> x))
    with
      | Fail -> None
end

module Arbitrary = struct
  type 'a t =
      { generator    : 'a Generator.t
      ; to_string    : ('a -> string)
      ; description  : string
      ; shrink       : ('a -> 'a list)
      }

  let make ~generator ?shrink ~to_string ~description () =
    match shrink with
      | None ->
        { generator; to_string; description; shrink = (fun _ -> []) }
      | Some shrink ->
        { generator; to_string; description; shrink }

  let to_generator a = a.generator

  let to_shrinker a = a.shrink

  let to_description a = a.description

  let to_string a = a.to_string

  let unit =
    let generator = Generator.return ()
    and to_string () = "()"
    and description = "unit"
    and shrink () = []
    in {generator; to_string; description; shrink}

  let bool =
    let generator = Generator.bool
    and to_string = function true -> "true" | false -> "false"
    and description = "bool"
    and shrink b = []
    in
    {generator; to_string; description; shrink}

  let list a =
    let generator =
      let open Generator in
      let rec loop acc = function
        | 0 -> return acc
        | n -> a.generator >>= fun x -> loop (x::acc) (n-1)
      in int 20 >>= loop []
    and to_string l =
      "[" ^ String.concat "," (List.map a.to_string l) ^ "]"
    and description = a.description ^ " list"
    and shrink l =
      (* Return all the lists obtained by removing a single element
         from [l] *)
      let rec gather lists before after = match after with
        | [] -> lists
        | x::xs ->
          gather ((List.rev before @ xs) :: lists) (x::before) xs
      in
      gather [] [] l
    in
    {generator; to_string; description;shrink}

  let char =
    let generator =
      let open Generator in
      Char.chr <$> int 256
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "char"
    and shrink c = []
    in
    {generator; to_string; description; shrink}

  let printable_ascii_char =
    let generator =
      let open Generator in
      (fun x -> Char.chr (x+32)) <$> int (128-32)
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "printable_ascii_char"
    and shrink c = []
    in
    {generator; to_string; description; shrink}

  let string =
    let generator =
      let open Generator in
      int 20 >>= fun length ->
      let str = String.create length in
      let rec loop i =
        if i = length then return str
        else begin
          char.generator >>= fun c ->
          str.[i] <- c;
          loop (i+1)
        end
      in
      loop 0
    and to_string s = "\"" ^ String.escaped s ^ "\""
    and description = "string"
    and shrink s =
      let l = String.length s in
      let ss = ref [] in
      for i = 0 to l - 1 do
        let s' = String.create (l - 1) in
        String.blit s 0 s' 0 i;
        String.blit s (i+1) s' i (l-i-1);
        ss := s' :: !ss
      done;
      !ss
    in
    {generator; to_string; description; shrink}

  let printable_ascii_string =
    let generator =
      let open Generator in
      int 20 >>= fun length ->
      let str = String.create length in
      let rec loop i =
        if i = length then return str
        else begin
          printable_ascii_char.generator >>= fun c ->
          str.[i] <- c;
          loop (i+1)
        end
      in
      loop 0
    and to_string s = String.escaped s
    and description = "printable_ascii_string"
    and shrink s =
      let l = String.length s in
      let ss = ref [] in
      for i = 0 to l - 1 do
        let s' = String.create (l - 1) in
        String.blit s 0 s' 0 i;
        String.blit s (i+1) s' i (l-i-1);
        ss := s' :: !ss
      done;
      !ss
    in
    {generator; to_string; description; shrink}

  let array a =
    let generator =
      let open Generator in
      int 20 >>= fun length ->
      array length (fun _ -> a.generator)
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
    and shrink a =
      let l = Array.length a in
      let arrays = ref [] in
      for i = 0 to l - 1 do
        let a' =
          Array.init (l-1) (fun j -> if j < i then a.(j) else a.(j+1))
        in
        arrays := a' :: !arrays
      done;
      !arrays
    in
    {generator; to_string; description; shrink}

  let option a =
    let generator =
      let open Generator in
      int 10 >>= fun c ->
      if c = 0 then
        return None
      else
        (a.generator >>= fun x -> return (Some x))
    and to_string = function
      | None -> "None"
      | Some v -> "Some (" ^ a.to_string v ^ ")"
    and description = a.description ^ " option"
    and shrink = function
      | None -> []
      | Some v ->
        None :: List.map (fun x -> Some x) (a.shrink v)
    in
    {generator; to_string; description; shrink}

  let int_range minimum maximum =
    let generator =
      (* FIXME: overflow *)
      let open Generator in
      int (maximum - minimum + 1) >>= fun x ->
      return (x + minimum)
    and to_string i = string_of_int i
    and description =
      Printf.sprintf "int_range(%d,%d)" minimum maximum
    and shrink i =
      if i > minimum then [i-1] else []
    in
    {generator; to_string; description; shrink}

  let float_range minimum maximum =
    (* FIXME: this is wrong... *)
    let generator =
      let open Generator in
      float (maximum -. minimum) >>= fun f ->
      return (f +. minimum)
    and to_string = string_of_float
    and description =
      Printf.sprintf "float(%f,%f)" minimum maximum
    in
    make ~generator ~to_string ~description ()

  let element_of ~to_string l =
    let generator =
      let open Generator in
      return () >>= fun () ->
      let len = List.length l in
      if len = 0 then
        fail
      else
        int len >>= fun i -> return (List.nth l i)
    and description =
      Printf.sprintf "element_of [%s]"
        (String.concat ";" (List.map to_string l))
    in
    make ~generator ~to_string ~description ()
end

module Property = struct
  open Arbitrary

  type t =
      Random.State.t ->
      [ `Ok
      | `CounterExample of ((string * string) list * string)
      | `PreconditionFailed
      ]

  let forall domain predicate random_state =
    let initial_value = Generator.run domain.generator random_state in
    let saved_random_state = Random.State.copy random_state in
    let make_report value =
      domain.to_string value,
      domain.description
    in
    let rec try_shrinkings = function
      | [] ->
        None
      | value::values ->
        let random_state = Random.State.copy saved_random_state in
        match predicate value random_state with
          | `PreconditionFailed
          | `Ok ->
            try_shrinkings values
          | `CounterExample report ->
            Some (try_to_shrink report value)
    and try_to_shrink (l,msg) value =
      match try_shrinkings (domain.shrink value) with
        | None        -> (make_report value::l, msg)
        | Some report -> report
    in
    match initial_value with
      | None -> `Ok
      | Some initial_value ->
        match predicate initial_value random_state with
          | `Ok ->
            `Ok
          | `CounterExample report ->
            `CounterExample (try_to_shrink report initial_value)
          | `PreconditionFailed ->
            `PreconditionFailed

  let check property =
    let random_state = Random.State.make_self_init () in
    let rec run counter successful =
      if counter = 10_000 then
        if successful < 7_500 then
          `GivenUp (counter, successful)
        else
          `OkAsFarAsIKnow
      else match property random_state with
        | `Ok ->
          run (counter+1) (successful+1)
        | `PreconditionFailed ->
          run (counter+1) successful
        | `CounterExample report ->
          `CounterExample report
    in
    run 0 0

  let true_that b _ =
    if b then
      `Ok
    else
      `CounterExample ([], "falsified")

  let equal ~to_string ?cmp x1 x2 _ =
    let x1_equals_x2 =
      match cmp with None -> x1 = x2 | Some eq -> eq x1 x2
    in
    if x1_equals_x2 then
      `Ok
    else
      (* FIXME: better message formatting *)
      let message = "Not equal: " ^ to_string x1 ^ " and " ^ to_string x2 in
      `CounterExample ([], message)

  let (==>) b property random_state =
    if b then
      property random_state
    else
      `PreconditionFailed

  let raises exn f _ =
    (* FIXME: better message *)
    try f (); `CounterExample ([], "")
    with
      | exn' when exn = exn' -> `Ok
      | _ -> `CounterExample ([], "")

  let to_test property =
    OUnit.TestCase (fun () ->
      match check property with
        | `OkAsFarAsIKnow -> ()
        | `CounterExample (l, msg) ->
          let message =
            (* FIXME: better rendering of the result *)
            "Counterexample: " ^ msg ^ "\n"
            ^ String.concat "\n" (List.map (fun (x,y) -> "(" ^ x ^ "," ^ y ^ ")") l)
          in
          OUnit.assert_failure message
        | `GivenUp _ ->
          OUnit.assert_failure "too many failed preconditions")
end
