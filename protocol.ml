open! Core

module Host = struct
  module Name : Identifiable = String
  module Info = struct
    type t = 
      { hostname: Name.t
      ; address: Unix.Inet_addr.t
      ; boot_time: Time.t
      }
  end
end

module Check = struct
  module Name : Identifiable = String
  module Outcome = struct
    type t = Passed | Failed of string
  end

  module Event = struct
    type t =
      | Register   of Host.Name.t * Name.t
      | Unregister of Host.Name.t * Name.t
      | Report of { check: Name.t; when_checked: Time.t; outcome: Outcome.t }
  end
end

module Event = struct
  type t = 
    | Check of Check.Event.t
    | Host_info of Host.Info.t
end

(*

   - start with something that doesn't require maps; say, a small
   fixed universe of hosts. This isn't going to use incremental in
   an especially interesting way, but it's a good place to start.

   - Maybe even have an open universe of hosts, but have part of the
   query be to select which hosts you actually look at. That's static
   enough to avoid maps. You'll need an imperatively managed table of
   data on the outside, though.

   - Various queries to push the model more, in particular:

   - find set of checks of a certain kind that failed (filter_map)

   - find set of stale nodes (clock + filter_map')

   - Rank queries? (nth-worst host?, median host?)

   - Merging examples? Drop rates on hosts with most highly loaded
   switches?
*)


(* 
   Other thoughts:

   - Benchmarks! Make sure that we have all-at-once computations for
   each of these.

*)


(* Next things:

   - Put this up online to work on

   - Talk to Dlo and Perl about useful queries to implement?

 *)
