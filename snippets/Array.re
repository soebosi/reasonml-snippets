let all = (ary, fn) => List.for_all(fn, ary);

let any = (ary, fn) => List.exists(fn, ary);

module Tuple2 = {
  let first = ((a, _)) => a;
  let second = ((_, b)) => b;
};

let bifurcate = (ary, filter: list(bool)) =>
  List.combine(ary, filter)
  |> List.partition(Tuple2.second)
  |> ((x, y)) => (List.map(Tuple2.first, x), List.map(Tuple2.first, y));

let bifurcateBy = (ary, fn) => List.partition(fn, ary);

