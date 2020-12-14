(* Read file from argv[1], decompress, write to argv[2] *)
let ( =| ) r e =
  r := e::!r

(* On 64 bit platforms, bigarray isn't used since max_string is big. If
 * this is set, we use bigarray always, for testing... *)
let test_bigarray = false

(* We used to have a type called "bytes" before OCaml did.  In order
not to break client code using e.g. Pdfio.bytes, we keep the name
but expose an alias caml_bytes for the built-in type. *)
type caml_bytes = bytes

(* External type for big streams of bytes passed to C*)
type rawbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* But for speed, we use strings of length < Sys.max_string_length *)
type bytes =
  | Long of rawbytes
  | Short of caml_bytes

(* Find the size of a stream. *)
let bytes_size = function
  | Short s -> Bytes.length s
  | Long b -> Bigarray.Array1.dim b

let bigarray_unsafe_get =
  Bigarray.Array1.unsafe_get

let bigarray_unsafe_set =
  Bigarray.Array1.unsafe_set

let bget_unsafe s n =
  match s with
  | Short s -> int_of_char (Bytes.unsafe_get s n)
  | Long s -> bigarray_unsafe_get s n

let bset s n v =
  match s with
  | Short s -> Bytes.set s n (Char.unsafe_chr v)
  | Long s -> Bigarray.Array1.set s n v

let bget s n =
  match s with
  | Short s -> int_of_char (Bytes.get s n)
  | Long s -> Bigarray.Array1.get s n

(* Make a stream of a given size. *)
let mkbytes l =
  if l <= (if test_bigarray then max_int else Sys.max_string_length)
    then Short (Bytes.create l)
    else Long (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout l)

let bset_unsafe s n v =
  match s with
  | Short s -> Bytes.unsafe_set s n (Char.unsafe_chr v)
  | Long s -> bigarray_unsafe_set s n v

(* Make a bytes from a list of strings by taking the contents, in order
from the items, in order. *)
let rec total_length n = function
 | [] -> n
 | h::t -> total_length (n + String.length h) t

let bytes_of_strings_rev strings =
  let len = total_length 0 strings in
    let s = mkbytes len
    and pos = ref (len - 1) in
      List.iter
        (fun str ->
           for x = String.length str - 1 downto 0 do
             bset_unsafe s !pos (int_of_char (String.unsafe_get str x));
             decr pos
           done)
        strings;
      s

let flate_process f data =
  let strings = ref []
  and pos = ref 0
  and inlength = bytes_size data in
    let input =
      (fun buf ->
         let s = Bytes.length buf in
           let towrite = min (inlength - !pos) s in
             for x = 0 to towrite - 1 do
               Bytes.unsafe_set
                 buf x (Char.unsafe_chr (bget_unsafe data !pos));
                 incr pos
             done;
             towrite)
    and output =
      (fun buf length ->
         if length > 0 then strings =| Bytes.sub_string buf 0 length)
    in
      f input output;
      bytes_of_strings_rev !strings

let decode_flate stream =
  if bytes_size stream = 0 then mkbytes 0 else (* Accept the empty stream. *)
    try flate_process Pdfflate.uncompress stream with
      Pdfflate.Error (a, b) ->
        (*if !debug then debug_stream stream;*)
        raise (Failure ("Flate" ^ " " ^ a ^ " " ^ b ^ " length " ^ string_of_int (bytes_size stream)))

let stream_of_file f =
  let fh = open_in_bin f in
  let l = in_channel_length fh in
  let b = mkbytes l in
    for x = 0 to l - 1 do
      bset b x (input_byte fh)
    done;
    close_in fh;
    b

let stream_to_file f s =
  let fh = open_out_bin f in
  for x = 0 to bytes_size s - 1 do
    output_byte fh (bget s x)
  done;
  close_out fh

let go i o =
  stream_to_file o (decode_flate (stream_of_file i))

let _ =
  match Sys.argv with
    [|_; input; output|] -> go input output
  | _ -> prerr_string "Format: main in out\n"; exit 2
