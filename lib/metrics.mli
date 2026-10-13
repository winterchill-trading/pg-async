(** Per-label query counters and latency histograms.

    Every query run through a connection that carries a [Metrics.t] is recorded under its
    [?label] (default ["query"]). Labels should be a small fixed set (one per statement
    kind), never built from data. *)

open! Core

type t

val create : unit -> t

(** Upper bounds of the histogram buckets; the last bucket is unbounded. *)
val bucket_bounds : Time_ns.Span.t list

val record : t -> label:string -> latency:Time_ns.Span.t -> ok:bool -> unit

module Label_stats : sig
  type t =
    { label : string
    ; count : int
    ; errors : int
    ; total : Time_ns.Span.t
    ; max : Time_ns.Span.t
    ; buckets : int list
    (** Count per bucket of [bucket_bounds] plus the overflow bucket, not cumulative. *)
    }
  [@@deriving sexp_of]

  (** Latency at quantile [q] (0 to 1), as the upper bound of the bucket holding it;
      [None] for the overflow bucket or an empty label. *)
  val quantile_upper_bound : t -> float -> Time_ns.Span.t option
end

(** One entry per label, sorted by label. *)
val snapshot : t -> Label_stats.t list

val reset : t -> unit

(** Prometheus text exposition: [<prefix>_queries_total], [<prefix>_query_errors_total]
    and the [<prefix>_query_duration_seconds] histogram, labelled by [label]. *)
val to_prometheus : ?prefix:string -> t -> string
