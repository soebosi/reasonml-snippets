let all = List.for_all;

let any = List.exists;

module Tuple2 = {
  let first = ((a, _)) => a;
  let second = ((_, b)) => b;
};

let bifurcate = (filter: list(bool), ary) =>
  List.combine(ary, filter)
  |> List.partition(Tuple2.second)
  |> ((x, y)) => (List.map(Tuple2.first, x), List.map(Tuple2.first, y));

let bifurcateBy = List.partition;

let take = {
  let rec take_ = (i, ary, acc) => {
    switch(i, ary) {
    | (i, _) when i <= 0 => acc
    | (_, []) => acc
    | (_, [x, ...y]) => take_(i - 1, y, List.append(acc, [x]))
    }
  };
  (i, ary) => take_(i, ary, [])
};

let takeLast = (i, ary) => List.rev(ary) |> take(i) |> List.rev;

let chunk = {
  let rec chunk_ = (i, ary, acc) => {
    let len = List.length(ary);
    if (i <= 0) {
      []
    } else if (len <= i) {
      List.append(acc, [ary])
    } else {
      chunk_(i, takeLast(len - i, ary), List.append(acc, [take(i, ary)]))
    }
  };
  (i, ary) => chunk_(i, ary, [])
};
