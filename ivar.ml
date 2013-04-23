type 'a t = {
  m : Mutex.t;
  full : Condition.t;
  value : 'a option ref
}

exception IVar_full

let from_some = function
  | Some x -> x
  | None -> raise Not_found

let empty ivar = 
  match !(ivar.value) with
  | None -> true
  | Some _ -> false

let full ivar = 
  not (empty ivar)

let create () = {
  m = Mutex.create ();
  full = Condition.create ();
  value = ref None
}

let write ivar x =
  Mutex.lock ivar.m;
  if full ivar then (Mutex.unlock ivar.m; raise IVar_full);
  ivar.value := Some x;
  Condition.broadcast ivar.full;
  Mutex.unlock ivar.m

let read ivar =
  Mutex.lock ivar.m;
  while empty ivar do
    Condition.wait ivar.full ivar.m
  done;
  let contents = from_some !(ivar.value) in
  Mutex.unlock ivar.m;
  contents
