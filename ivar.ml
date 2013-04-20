type 'a t = {
  m : Mutex.t;
  c : Condition.t;
  value : 'a option ref
}

exception IVar_full

let from_some = function
  | Some x -> x
  | None -> raise Not_found

let create () = {
  m = Mutex.create ();
  c = Condition.create ();
  value = ref None
}

let write ivar x =
  Mutex.lock ivar.m;
  if !(ivar.value) <> None then begin
    Mutex.unlock ivar.m;
    raise IVar_full
  end;
  ivar.value := Some x;
  Condition.signal ivar.c;
  Mutex.unlock ivar.m

let read ivar =
  Mutex.lock ivar.m;
  while !(ivar.value) == None do
    Condition.wait ivar.c ivar.m
  done;
  let x = from_some !(ivar.value) in
  Condition.signal ivar.c;
  Mutex.unlock ivar.m;
  x
