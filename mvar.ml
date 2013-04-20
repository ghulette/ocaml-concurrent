type 'a t = {
  m : Mutex.t;
  c : Condition.t;
  value : 'a option ref
}

let from_some = function
  | Some x -> x
  | None -> raise Not_found

let create () = {
  m = Mutex.create ();
  c = Condition.create ();
  value = ref None
}

let put mvar x =
  Mutex.lock mvar.m;
  while !(mvar.value) <> None do
    Condition.wait mvar.c mvar.m
  done;
  mvar.value := Some x;
  Condition.signal mvar.c;
  Mutex.unlock mvar.m

let take mvar =
  Mutex.lock mvar.m;
  while !(mvar.value) == None do
    Condition.wait mvar.c mvar.m
  done;
  let x = from_some !(mvar.value) in
  mvar.value := None;
  Condition.signal mvar.c;
  Mutex.unlock mvar.m;
  x
