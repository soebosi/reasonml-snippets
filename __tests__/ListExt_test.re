open Jest;

open Expect;

open ListExt;

describe("List.all", () => {
  test("should be true", () =>
    expect(List.all([1, 2, 3], x => x > 0)) |> toBe(true)
  );
  test("should be false", () =>
    expect(List.all([1, 2, 3], x => x > 2)) |> toBe(false)
  );
});

describe("List.any", () => {
  test("should be true", () =>
    expect(List.any([1, 2, 3], x => x > 2)) |> toBe(true)
  );
  test("should be false", () =>
    expect(List.any([1, 2, 3], x => x < 0)) |> toBe(false)
  );
});

describe("List.bifurcate", () =>
  test("should extract \"foo\"", () =>
    expect(
      List.bifurcate(
        ["beep", "boop", "foo", "bar"],
        [true, true, false, true],
      ),
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("List.bifurcateBy", () =>
  test("should extract \"foo\"", () =>
    expect(
      List.bifurcateBy(["beep", "boop", "foo", "bar"], str =>
        String.contains(str, 'b')
      ),
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("List.takeLast", () => {
  test("should extract \"beep\"", () =>
    expect(List.takeLast(["beep", "boop", "foo", "bar"], 3))
    |> toEqual(["boop", "foo", "bar"])
  );
  test("should return empty list when i > List.length()", () =>
    expect(List.takeLast(["beep", "boop", "foo", "bar"], 5)) |> toEqual([])
  );
  test("should return empty list when i <= 0", () =>
    expect(List.takeLast(["beep", "boop", "foo", "bar"], -1))
    |> toEqual([])
  );
});

describe("List.takeWhile", () =>
  test("should extract [3, 1]", () =>
    expect(List.takeWhile([1, 2, 3, 1], i => i < 3)) |> toEqual([1, 2])
  )
);

describe("List.takeLastWhile", () =>
  test("should extract [1, 2, 3]", () =>
    expect(List.takeLastWhile([1, 2, 3, 1], i => i < 3)) |> toEqual([1])
  )
);

describe("List.chunk", () => {
  test("should return 2 elements list", () =>
    expect(List.chunk(["beep", "boop", "foo", "bar"], 2))
    |> toEqual([["beep", "boop"], ["foo", "bar"]])
  );
  test("should return rest elements", () =>
    expect(List.chunk(["beep", "boop", "foo", "bar"], 3))
    |> toEqual([["beep", "boop", "foo"], ["bar"]])
  );
  test("should return full list when size <= 0", () =>
    expect(List.chunk(["beep", "boop", "foo", "bar"], -1))
    |> toEqual([["beep", "boop", "foo", "bar"]])
  );
});

describe("List.compact", () =>
  test("should remove None elements", () =>
    expect(List.compact([Some("beep"), None, Some("boop"), None]))
    |> toEqual([Some("beep"), Some("boop")])
  )
);

module IntCmp =
  Belt.Id.MakeComparable(
    {
      type t = int;
      let cmp = (a, b) => Pervasives.compare(a, b);
    },
  );

describe("List.countBy", () => {
  let m =
    List.countBy(
      ["one", "two", "three"],
      String.length,
      ~id=(module IntCmp),
    );
  test("should have (3, 2)", () =>
    expect(Belt.Map.get(m, 3)) |> toEqual(Some(2))
  );
  test("should have (5, 1)", () =>
    expect(Belt.Map.get(m, 5)) |> toEqual(Some(1))
  );
  test("should have (0, *)", () =>
    expect(Belt.Map.get(m, 0)) |> toEqual(None)
  );
});

describe("List.countOccurrences", () => {
  test("should return 3", () =>
    expect(List.countOccurrences([1, 1, 2, 1, 2, 3], 1)) |> toEqual(3)
  );
  test("should return 0 when there is no value", () =>
    expect(List.countOccurrences([1, 1, 2, 1, 2, 3], -1)) |> toEqual(0)
  );
  test("should return 0 when arr is empty", () =>
    expect(List.countOccurrences([], 1)) |> toEqual(0)
  );
});

describe("List.deepFlatten", () =>
  test("should return [1, 2, 3, 4]", () =>
    expect(
      List.deepFlatten(
        Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf))),
      ),
    )
    |> toEqual([1, 2, 3, 4])
  )
);

describe("List.difference", () =>
  test("should return {3} = {1, 2, 3} \\ {1, 2, 4} ", () =>
    expect(List.difference([1, 2, 3], [1, 2, 4], ~id=(module IntCmp)))
    |> toEqual([3])
  )
);

describe("List.drop", () => {
  test("should drop 1", () =>
    expect(List.drop([1, 2, 3], 1)) |> toEqual([2, 3])
  );
  test("should drop 1, 2", () =>
    expect(List.drop([1, 2, 3], 2)) |> toEqual([3])
  );
  test("should return empty list when i > ary.length", () =>
    expect(List.drop([1, 2, 3], 42)) |> toEqual([])
  );
  test("should not drop when i == 0", () =>
    expect(List.drop([1, 2, 3], 0)) |> toEqual([1, 2, 3])
  );
  test("should return empty list when i < 0", () =>
    expect(List.drop([1, 2, 3], -10)) |> toEqual([])
  );
});

describe("List.dropRight", () => {
  test("should drop 3", () =>
    expect(List.dropRight([1, 2, 3], 1)) |> toEqual([1, 2])
  );
  test("should drop 2, 3", () =>
    expect(List.dropRight([1, 2, 3], 2)) |> toEqual([1])
  );
  test("should return empty list when i > ary.length", () =>
    expect(List.dropRight([1, 2, 3], 42)) |> toEqual([])
  );
  test("should not drop when i == 0", () =>
    expect(List.dropRight([1, 2, 3], 0)) |> toEqual([1, 2, 3])
  );
  test("should return empty when i < 0", () =>
    expect(List.dropRight([1, 2, 3], -10)) |> toEqual([])
  );
});

describe("List.dropRightWhile", () =>
  test("should drop [3, 4]", () =>
    expect(List.dropRightWhile([1, 2, 3, 4], n => n < 3)) |> toEqual([1, 2])
  )
);

describe("List.everyNth", () => {
  test("should skip 1, 3, 5", () =>
    expect(List.everyNth([1, 2, 3, 4, 5, 6], 2)) |> toEqual([2, 4, 6])
  );
  test("should skip 1, 2, 4, 5", () =>
    expect(List.everyNth([1, 2, 3, 4, 5, 6], 3)) |> toEqual([3, 6])
  );
});

describe("List.findLast", () =>
  test("should skip 4", () =>
    expect(List.findLast([1, 2, 3, 4], n => n mod 2 == 1)) |> toEqual(3)
  )
);

describe("List.findLastIndex", () =>
  test("should skip 4", () =>
    expect(List.findLastIndex([1, 2, 3, 4], n => n mod 2 == 1))
    |> toEqual(2)
  )
);

describe("List.head", () =>
  test("should get 1", () =>
    expect(List.head([1, 2, 3, 4])) |> toEqual(1)
  )
);

describe("List.indexOfAll", () => {
  test("should get indices", () =>
    expect(List.indexOfAll([1, 2, 3, 1, 2, 3], 1)) |> toEqual([0, 3])
  );
  test("should get empty list", () =>
    expect(List.indexOfAll([1, 2, 3, 1, 2, 3], 4)) |> toEqual([])
  );
});

describe("List.initial", () =>
  test("should drop last element", () =>
    expect(List.initial([1, 2, 3])) |> toEqual([1, 2])
  )
);

describe("List.initialize2DArray", () =>
  test("should generate 2 x 2 List filled with 0", () =>
    expect(List.initialize2DArray(2, 2, 0)) |> toEqual([[0, 0], [0, 0]])
  )
);

describe("List.initializeArrayWithRange", () => {
  test("should generate natural number list", () =>
    expect(List.initializeArrayWithRange(0, 5, 1))
    |> toEqual([0, 1, 2, 3, 4])
  );
  test("should generate even number list", () =>
    expect(List.initializeArrayWithRange(0, 9, 2))
    |> toEqual([0, 2, 4, 6, 8])
  );
});

describe("List.initializeArrayWithRangeRight", () => {
  test("should generate natural number list", () =>
    expect(List.initializeArrayWithRangeRight(5, 0, 1))
    |> toEqual([4, 3, 2, 1, 0])
  );
  test("should generate even number list", () =>
    expect(List.initializeArrayWithRangeRight(9, 0, 2))
    |> toEqual([8, 6, 4, 2, 0])
  );
});

describe("List.initializeArrayWithValues", () =>
  test("should generate natural number list", () =>
    expect(List.initializeArrayWithValues(5, 2)) |> toEqual([2, 2, 2, 2, 2])
  )
);

describe("List.initializeArrayWithValues", () =>
  test("should generate natural number list", () =>
    expect(List.join(["pen", "pineapple", "apple", "pen"], ","))
    |> toEqual("pen,pineapple,apple,pen")
  )
);

describe("List.intersection", () =>
  test("should true", () =>
    expect(List.intersection([1, 2, 3], [4, 3, 2], ~id=(module IntCmp)))
    |> toEqual([2, 3])
  )
);

describe("List.last", () =>
  test("should get last element", () =>
    expect(List.last([1, 2, 3])) |> toEqual(3)
  )
);

describe("List.forEachRight", () =>
  test("should get last element", () => {
    let ret = ref([]);
    List.forEachRight([1, 2, 3, 4], (. val_) => ret := [val_, ...ret^]);
    expect(ret^) |> toEqual([1, 2, 3, 4]);
  })
);

describe("List.none", () => {
  test("should true", () =>
    expect(List.none([0, 1, 3, 0], (. a) => a == 2)) |> toEqual(true)
  );
  test("should false", () =>
    expect(List.none([0, 1, 2, 0], (. a) => a == 2)) |> toEqual(false)
  );
});
