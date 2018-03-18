module List = {
  include List;
  let all = for_all;
  let any = exists;
  let bifurcate = (filter: list(bool), ary) =>
    combine(ary, filter)
    |> partition(snd)
    |> (((x, y)) => (map(fst, x), map(fst, y)));
  let bifurcateBy = partition;
  let take = {
    let rec take_ = (i, ary, acc) =>
      switch (i, ary) {
      | (i, _) when i <= 0 => acc
      | (_, []) => acc
      | (_, [x, ...y]) => take_(i - 1, y, acc @ [x])
      };
    (i, ary) => take_(i, ary, []);
  };
  let takeLast = (i, ary) => rev(ary) |> take(i) |> rev;
  let takeWhile = {
    let rec takeWhile_ = (func, ary, acc) =>
      switch (ary) {
      | [head, ...tail] when func(head) =>
        takeWhile_(func, tail, acc @ [head])
      | _ => acc
      };
    (func, ary) => takeWhile_(func, ary, []);
  };
  let takeLastWhile = (func, ary) => rev(ary) |> takeWhile(func) |> rev;
  let chunk = {
    let rec chunk_ = (i, ary, acc) => {
      let len = length(ary);
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
      switch (arr) {
      | [] => acc
      | [a] when a == value => acc + 1
      | [_] => acc
      | [a, ...b] when a == value => countOccurrences_(value, b, acc + 1)
      | [_, ...b] => countOccurrences_(value, b, acc)
      };
    (value, arr) => countOccurrences_(value, arr, 0);
  };
  let drop = (i, ary) => takeLast(length(ary) - i, ary);
  let dropRight = (i, ary) => take(length(ary) - i, ary);
  let dropRightWhile = takeWhile;
  let everyNth = {
    let rec everyNth_ = (nth, ary, acc, index) => {
      let skip = index mod nth != 0;
      switch (ary) {
      | [] => acc
      | [hd, ...tl] =>
        (skip ? acc : acc @ [hd]) |> everyNth_(nth, tl, _, index + 1)
      };
    };
    (nth, ary) => everyNth_(nth, ary, [], 1);
  };
  let findLast = (fn, ary) => rev(ary) |> find(fn);
  let findLastIndex = (fn, ary) =>
    mapi((i, elm) => (i, elm), ary)
    |> rev
    |> find(((_i, elm)) => fn(elm))
    |> fst;
  let head = hd;
  let filteri = (fn, ary) => mapi((i, a) => (i, a), ary) |> filter(fn);
  let indexOfAll = (elm, ary) =>
    mapi((i, e) => (i, e), ary)
    |> filter(((_, e)) => e == elm)
    |> map(fst);
  let initial = ary => rev(ary) |> tl |> rev;
  let range = (s, e, step) => {
    let rec range_ = (s, e, step, acc) =>
      if (s >= e) {
        acc;
      } else {
        range_(s + step, e, step, append(acc, [s]));
      };
    range_(s, e, step, []);
  };
  let initialize2DArray = (w, h, val_) =>
    range(0, w, 1)
    |> map((_) => range(val_, h + val_, 1))
    |> map(elm => map((_) => val_, elm));
  let initializeArrayWithRange = range;
};
