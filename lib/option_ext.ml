open Base

let or_else ~(f: unit -> 'a option) = function
  | None -> f()
  | Some _ as opt -> opt
