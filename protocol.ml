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
  end
end

module Event = struct
  type t = 
    | Check of Check.Event.t
    | Host_info of Host.Info.t
end

