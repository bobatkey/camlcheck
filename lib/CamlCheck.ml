module Generator : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t

  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val bits : int t
  val int : int -> int t
  val int32 : int32 -> int32 t
  val nativeint : nativeint -> nativeint t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val bool : bool t

  module Array : sig
    val init : int -> (int -> 'a t) -> 'a array t
  end

  val run : 'a t -> Random.State.t -> 'a
end = struct
  type 'a t = Random.State.t -> 'a

  let run generator random_state =
    generator random_state

  let return a random_state = a
  let bind c f random_state =
    f (c random_state) random_state
  let (>>=) c f random_state =
    f (c random_state) random_state

  let lift2 f g1 g2 =
    bind g1 begin fun x1 ->
      bind g2 begin fun x2 ->
        return (f x1 x2)
      end
    end

  let lift f g =
    bind g begin fun x -> return (f x) end

  let bits random_state =
    Random.State.bits random_state

  let int bound random_state =
    Random.State.int random_state bound

  let int32 bound random_state =
    Random.State.int32 random_state bound

  let nativeint bound random_state =
    Random.State.nativeint random_state bound

  let int64 bound random_state =
    Random.State.int64 random_state bound

  let float bound random_state =
    Random.State.float random_state bound

  let bool random_state =
    Random.State.bool random_state

  module Array = struct
    let init length f random_state =
      Array.init length (fun i -> f i random_state)
  end
end

module Domain = struct
  type 'a t =
      { generate    : 'a Generator.t
      ; to_string   : ('a -> string)
      ; description : string
      ; shrink    : ('a -> 'a list)
      }

  let make ~generate ?shrink ~to_string ~description () =
    match shrink with
      | None ->
        { generate; to_string; description; shrink = (fun _ -> []) }
      | Some shrink ->
        { generate; to_string; description; shrink }

  let generator a = a.generate

  let shrinker a = a.shrink

  let description a = a.description

  let to_string a = a.to_string
end

module PrimitiveDomains = struct
  open Domain

  let unit =
    let generate = Generator.return ()
    and to_string () = "()"
    and description = "unit"
    and shrink () = []
    in {generate; to_string; description; shrink}

  let bool =
    let generate = Generator.bool
    and to_string = function true -> "true" | false -> "false"
    and description = "bool"
    and shrink b = []
    in
    {generate; to_string; description; shrink}

  let list a =
    let generate =
      let open Generator in
      let rec loop acc = function
        | 0 -> return acc
        | n -> a.generate >>= fun x -> loop (x::acc) (n-1)
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
    {generate; to_string; description;shrink}

  let char =
    let generate =
      let open Generator in
      lift Char.chr (int 256)
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "char"
    and shrink c = []
    in
    {generate; to_string; description; shrink}

  let printable_ascii_char =
    let generate =
      let open Generator in
      lift (fun x -> Char.chr (x+32)) (int (128-32))
    and to_string c = "\"" ^ Char.escaped c ^ "\""
    and description = "printable_ascii_char"
    and shrink c = []
    in
    {generate; to_string; description; shrink}

  let string =
    let generate =
      let open Generator in
      int 20 >>= fun length ->
      let str = String.create length in
      let rec loop i =
        if i = length then return str
        else begin
          char.generate >>= fun c ->
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
    {generate; to_string; description; shrink}

  let printable_ascii_string =
    let generate =
      let open Generator in
      int 20 >>= fun length ->
      let str = String.create length in
      let rec loop i =
        if i = length then return str
        else begin
          printable_ascii_char.generate >>= fun c ->
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
    {generate; to_string; description; shrink}

  let array a =
    let generate =
      let open Generator in
      int 20 >>= fun length ->
      Array.init length (fun _ -> a.generate)
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
    {generate; to_string; description; shrink}

  let option a =
    let generate =
      let open Generator in
      int 10 >>= fun c ->
      if c = 0 then
        return None
      else
        (a.generate >>= fun x -> return (Some x))
    and to_string = function
      | None -> "None"
      | Some v -> "Some (" ^ a.to_string v ^ ")"
    and description = a.description ^ " option"
    and shrink = function
      | None -> []
      | Some v ->
        None :: List.map (fun x -> Some x) (a.shrink v)
    in
    {generate; to_string; description; shrink}

  let int_range minimum maximum =
    let generate =
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
    {generate; to_string; description; shrink}

  let float_range minimum maximum =
    (* FIXME: this is wrong... *)
    Domain.make
      ~generate:begin
        let open Generator in
        float (maximum -. minimum) >>= fun f -> return (f +. minimum)
      end
      ~to_string:string_of_float
      ~description:(Printf.sprintf "float(%f,%f)" minimum maximum)
      ()
end

module Property = struct
  open Domain

  type t =
      Random.State.t ->
      [ `Ok | `CounterExample of ((string * string) list * string) ]

  let forall domain predicate random_state =
    let initial_value = Generator.run domain.generate random_state in
    let saved_random_state = Random.State.copy random_state in
    let make_report value =
      (domain.to_string value,
       domain.description)
    in
    let rec try_shrinkings = function
      | [] -> None
      | value::values ->
        let random_state = Random.State.copy saved_random_state in
        match predicate value random_state with
          | `Ok               -> try_shrinkings values
          | `CounterExample l -> Some (try_to_shrink l value)
    and try_to_shrink (l,msg) value =
      match try_shrinkings (domain.shrink value) with
        | None        -> (make_report value::l, msg)
        | Some report -> report
    in
    match predicate initial_value random_state with
      | `Ok ->
        `Ok
      | `CounterExample report ->
        `CounterExample (try_to_shrink report initial_value)

  let check property =
    let random_state = Random.State.make_self_init () in
    let rec run i =
      if i = 0 then `OkAsFarAsIKnow
      else match property random_state with
        | `Ok ->
          run (i-1)
        | `CounterExample report ->
          `CounterExample report
    in
    run 10000

  let true_that b _ =
    if b then `Ok else `CounterExample ([], "")

  let equal ~to_string x1 x2 _ =
    if x1 = x2 then
      `Ok
    else
      let message = "Not equal: " ^ to_string x1 ^ " and " ^ to_string x2 in
      `CounterExample ([], message)

  let raises exn f _ =
    (* FIXME: better message *)
    try f (); `CounterExample ([], "")
    with
      | exn' when exn = exn' -> `Ok
      | _ -> `CounterExample ([], "")

  let to_test property () =
    match check property with
      | `OkAsFarAsIKnow -> ()
      | `CounterExample (l, msg) ->
        let message =
          (* FIXME: better rendering of the result *)
          "Counterexample: " ^ msg ^ "\n"
          ^ String.concat "\n" (List.map (fun (x,y) -> "(" ^ x ^ "," ^ y ^ ")") l)
        in
        OUnit.assert_failure message
end
