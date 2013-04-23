let m = Mvar.create ()

let worker id = 
  Thread.yield ();
  let x = Mvar.take m in
  Thread.yield ();
  Mvar.put m (succ x);
  Thread.yield ();
  Printf.printf "Thread #%d: %d\n" id x

let rec range i j = 
  if i >= j then [] else i :: range (succ i) j

let () =
  let ids = range 0 100 in
  let ts = List.map (Thread.create worker) ids in
  Mvar.put m 0;
  List.iter Thread.join ts
