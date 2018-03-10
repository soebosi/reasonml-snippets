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

let takeLast = (i, ary) => List.(rev(ary) |> take(i) |> rev);


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

let countOccurrences = {
  let rec countOccurrences_ = (value, arr, acc) =>
    switch(arr) {
    | [] => acc
    | [a] when a == value => acc + 1
    | [_] => acc
    | [a, ...b] when a == value => countOccurrences_(value, b, acc + 1)
    | [_, ...b] => countOccurrences_(value, b, acc)
    };
  (value, arr) => countOccurrences_(value, arr, 0);
};

let drop = (i, ary) => takeLast(List.length(ary) - i, ary);

let dropRight = (i, ary) => take(List.length(ary) - i, ary);
