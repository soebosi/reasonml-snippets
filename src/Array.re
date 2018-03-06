let all = List.for_all;

let any = List.exists;

let bifurcate = (filter: list(bool), ary) =>
  List.combine(ary, filter)
  |> List.partition(snd)
  |> (((x, y)) => (List.map(fst, x), List.map(fst, y)));

let bifurcateBy = List.partition;

let take = {
  let rec take_ = (i, ary, acc) =>
    switch (i, ary) {
    | (i, _) when i <= 0 => acc
    | (_, []) => acc
    | (_, [x, ...y]) => take_(i - 1, y, acc @ [x])
    };
  (i, ary) => take_(i, ary, []);
};

let takeLast = (i, ary) => List.rev(ary) |> take(i) |> List.rev;

let chunk = {
  let rec chunk_ = (i, ary, acc) => {
    let len = List.length(ary);
    if (i <= 0) {
      [];
    } else if (len <= i) {
      acc @ [ary];
    } else {
      chunk_(i, takeLast(len - i, ary), acc @ [take(i, ary)]);
    };
  };
  (i, ary) => chunk_(i, ary, []);
};
