let all = (ary, fn) => List.for_all(fn, ary);

let any = (ary, fn) => List.exists(fn, ary);

let bifurcate = (ary, filter: list(bool)) =>
  List.combine(ary, filter)
  |> List.partition(((_, b)) => b)
  |> ((x, y)) => (List.map(((a, _)) => a, x), List.map(((a, _)) => a, y));

