open! Core

let bucket_bounds =
  List.map
    [ 100
    ; 250
    ; 500
    ; 1_000
    ; 2_500
    ; 5_000
    ; 10_000
    ; 25_000
    ; 50_000
    ; 100_000
    ; 250_000
    ; 500_000
    ; 1_000_000
    ; 2_500_000
    ; 5_000_000
    ; 10_000_000
    ]
    ~f:Time_ns.Span.of_int_us
;;

let bounds = Array.of_list bucket_bounds

type entry =
  { mutable count : int
  ; mutable errors : int
  ; mutable total : Time_ns.Span.t
  ; mutable max : Time_ns.Span.t
  ; buckets : int array
  }

type t = (string, entry) Hashtbl.t

let create () = Hashtbl.create (module String)

let bucket_index latency =
  match
    Array.binary_search
      bounds
      ~compare:Time_ns.Span.compare
      `First_greater_than_or_equal_to
      latency
  with
  | Some i -> i
  | None -> Array.length bounds
;;

let record t ~label ~latency ~ok =
  let e =
    Hashtbl.find_or_add t label ~default:(fun () ->
      { count = 0
      ; errors = 0
      ; total = Time_ns.Span.zero
      ; max = Time_ns.Span.zero
      ; buckets = Array.create ~len:(Array.length bounds + 1) 0
      })
  in
  e.count <- e.count + 1;
  if not ok then e.errors <- e.errors + 1;
  e.total <- Time_ns.Span.( + ) e.total latency;
  e.max <- Time_ns.Span.max e.max latency;
  let i = bucket_index latency in
  e.buckets.(i) <- e.buckets.(i) + 1
;;

module Label_stats = struct
  type t =
    { label : string
    ; count : int
    ; errors : int
    ; total : Time_ns.Span.t
    ; max : Time_ns.Span.t
    ; buckets : int list
    }
  [@@deriving sexp_of]

  let quantile_upper_bound t q =
    if t.count = 0
    then None
    else (
      let target =
        Float.iround_up_exn (Float.of_int t.count *. Float.clamp_exn q ~min:0. ~max:1.)
      in
      let target = Int.max 1 target in
      let rec go i seen = function
        | [] -> None
        | n :: rest ->
          let seen = seen + n in
          if seen >= target
          then if i < Array.length bounds then Some bounds.(i) else None
          else go (i + 1) seen rest
      in
      go 0 0 t.buckets)
  ;;
end

let snapshot t =
  Hashtbl.to_alist t
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.map ~f:(fun (label, (e : entry)) ->
    { Label_stats.label
    ; count = e.count
    ; errors = e.errors
    ; total = e.total
    ; max = e.max
    ; buckets = Array.to_list e.buckets
    })
;;

let reset t = Hashtbl.clear t

let escape_label s =
  String.concat_map s ~f:(function
    | '\\' -> "\\\\"
    | '"' -> "\\\""
    | '\n' -> "\\n"
    | c -> String.of_char c)
;;

let to_prometheus ?(prefix = "pg") t =
  let b = Buffer.create 1024 in
  let line fmt =
    ksprintf
      (fun s ->
        Buffer.add_string b s;
        Buffer.add_char b '\n')
      fmt
  in
  let stats = snapshot t in
  line "# TYPE %s_queries_total counter" prefix;
  List.iter stats ~f:(fun s ->
    line "%s_queries_total{label=\"%s\"} %d" prefix (escape_label s.label) s.count);
  line "# TYPE %s_query_errors_total counter" prefix;
  List.iter stats ~f:(fun s ->
    line "%s_query_errors_total{label=\"%s\"} %d" prefix (escape_label s.label) s.errors);
  line "# TYPE %s_query_duration_seconds histogram" prefix;
  List.iter stats ~f:(fun s ->
    let label = escape_label s.label in
    let cumulative = ref 0 in
    List.iteri s.buckets ~f:(fun i n ->
      cumulative := !cumulative + n;
      let le =
        if i < Array.length bounds
        then sprintf "%g" (Time_ns.Span.to_sec bounds.(i))
        else "+Inf"
      in
      line
        "%s_query_duration_seconds_bucket{label=\"%s\",le=\"%s\"} %d"
        prefix
        label
        le
        !cumulative);
    line
      "%s_query_duration_seconds_sum{label=\"%s\"} %.6f"
      prefix
      label
      (Time_ns.Span.to_sec s.total);
    line "%s_query_duration_seconds_count{label=\"%s\"} %d" prefix label s.count);
  Buffer.contents b
;;
