open! Core

module Host = struct
  module Name : Identifiable = String
  module Info = struct
    type t = 
      { name : Name.t
      ; address : Unix.Inet_addr.Blocking_sexp.t
      ; boot_time : Time.t
      }
      [@@deriving sexp, bin_io]
  end
end

module Check = struct
  module Name : Identifiable = String
  module Outcome = struct
    type t = Passed | Failed of string
    [@@deriving sexp, bin_io]
  end

  module Event = struct
    type t =
      | Register   of 
          { host  : Host.Name.t 
          ; check : Name.t }
      | Unregister of
          { host  : Host.Name.t
          ; check : Name.t }
      | Report of
          { host         : Host.Name.t
          ; check        : Name.t
          ; when_checked : Time.t
          ; outcome      : Outcome.t }
    [@@deriving sexp, bin_io]
  end
end

module Event = struct
  type event = 
    | Check of Check.Event.t
    | Host_info of Host.Info.t
  [@@deriving sexp, bin_io]

  type t = { time: Time.t; ev: event }
  [@@deriving sexp, bin_io]

  let create time ev = { time; ev }
end

let events = 
  Async.Rpc.Pipe_rpc.create ()
    ~name:"events"
    ~version:0 (* Unversioned *)
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Event.t]
    ~bin_error:[%bin_type_class: unit]
