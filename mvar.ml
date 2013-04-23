type 'a t = {
  m : Mutex.t;
  empty : Condition.t;
  full : Condition.t;
  value : 'a option ref
}

let from_some = function
  | Some x -> x
  | None -> raise Not_found

let empty mvar = 
  match !(mvar.value) with
  | None -> true
  | Some _ -> false

let full mvar = 
  not (empty mvar)

let create () = {
  m = Mutex.create ();
  empty = Condition.create ();
  full = Condition.create ();
  value = ref None
}

let rec put mvar x =
  Mutex.lock mvar.m;
  while full mvar do
    Condition.wait mvar.empty mvar.m;
  done;
  mvar.value := Some x;
  Condition.signal mvar.full;
  Mutex.unlock mvar.m

let take mvar =
  Mutex.lock mvar.m;
  while empty mvar do
    Condition.wait mvar.full mvar.m
  done;
  let contents = from_some !(mvar.value) in
  mvar.value := None;
  Condition.signal mvar.empty;
  Mutex.unlock mvar.m;
  contents
