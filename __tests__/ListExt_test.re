open Jest;

open Expect;

open ListExt;

describe("List.all", () => {
  test("should be true", () =>
    expect(List.all(x => x > 0, [1, 2, 3])) |> toBe(true)
  );
  test("should be false", () =>
    expect(List.all(x => x > 2, [1, 2, 3])) |> toBe(false)
  );
});

describe("List.any", () => {
  test("should be true", () =>
    expect(List.any(x => x > 2, [1, 2, 3])) |> toBe(true)
  );
  test("should be false", () =>
    expect(List.any(x => x < 0, [1, 2, 3])) |> toBe(false)
  );
});

describe("List.bifurcate", () =>
  test("should extract \"foo\"", () =>
    expect(
      List.bifurcate(
        [true, true, false, true],
        ["beep", "boop", "foo", "bar"]
      )
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("List.bifurcateBy", () =>
  test("should extract \"foo\"", () =>
    expect(
      List.bifurcateBy(
        str => String.contains(str, 'b'),
        ["beep", "boop", "foo", "bar"]
      )
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("List.take", () => {
  test("should extract \"bar\"", () =>
    expect(List.take(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo"])
  );
  test("should return full list when i > List.length()", () =>
    expect(List.take(5, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo", "bar"])
  );
  test("should return empty list when i <= 0", () =>
    expect(List.take(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("List.takeLast", () => {
  test("should extract \"beep\"", () =>
    expect(List.takeLast(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["boop", "foo", "bar"])
  );
  test("should return full list when i > List.length()", () =>
    expect(List.takeLast(5, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo", "bar"])
  );
  test("should return empty list when i <= 0", () =>
    expect(List.takeLast(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("List.takeWhile", () => {
  test("should extract [3, 1]", () =>
    expect(List.takeWhile((i) => i < 3, [1, 2, 3, 1]))
    |> toEqual([1, 2])
  );
});

describe("List.takeLastWhile", () => {
  test("should extract [1, 2, 3]", () =>
    expect(List.takeLastWhile((i) => i < 3, [1, 2, 3, 1]))
    |> toEqual([1])
  );
});

describe("List.chunk", () => {
  test("should return 2 elements list", () =>
    expect(List.chunk(2, ["beep", "boop", "foo", "bar"]))
    |> toEqual([["beep", "boop"], ["foo", "bar"]])
  );
  test("should return rest elements", () =>
    expect(List.chunk(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual([["beep", "boop", "foo"], ["bar"]])
  );
  test("should return empty list when size <= 0", () =>
    expect(List.chunk(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("List.countOccurrences", () => {
  test("should return 3", () =>
    expect(List.countOccurrences(1, [1, 1, 2, 1, 2, 3]))
    |> toEqual(3)
  );

  test("should return 0 when there is no value", () =>
    expect(List.countOccurrences(-1, [1, 1, 2, 1, 2, 3]))
    |> toEqual(0)
  );

  test("should return 0 when arr is empty", () =>
    expect(List.countOccurrences(1, []))
    |> toEqual(0)
  );
});

describe("List.drop", () => {
  test("should drop 1", () =>
    expect(List.drop(1, [1, 2, 3]))
    |> toEqual([2, 3])
  );
  test("should drop 1, 2", () =>
    expect(List.drop(2, [1, 2, 3]))
    |> toEqual([3])
  );
  test("should return empty list when i > ary.length", () =>
    expect(List.drop(42, [1, 2, 3]))
    |> toEqual([])
  );
  test("should not drop when i == 0", () => {
    expect(List.drop(0, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
  test("should not drop when i < 0", () => {
    expect(List.drop(-10, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
});

describe("List.dropRight", () => {
  test("should drop 3", () =>
    expect(List.dropRight(1, [1, 2, 3]))
    |> toEqual([1, 2])
  );
  test("should drop 2, 3", () =>
    expect(List.dropRight(2, [1, 2, 3]))
    |> toEqual([1])
  );
  test("should return empty list when i > ary.length", () =>
    expect(List.dropRight(42, [1, 2, 3]))
    |> toEqual([])
  );
  test("should not drop when i == 0", () => {
    expect(List.dropRight(0, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
  test("should not drop when i < 0", () => {
    expect(List.dropRight(-10, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
});

describe("List.dropRightWhile", () => {
  test("should drop [3, 4]", () => {
    expect(List.dropRightWhile(n => n < 3, [1, 2, 3, 4]))
    |> toEqual([1, 2,])
  });
});

describe("List.everyNth", () => {
  test("should skip 1, 3, 5", () => {
    expect(List.everyNth(2, [1, 2, 3, 4, 5, 6]))
    |> toEqual([2, 4, 6])
  });
  test("should skip 1, 2, 4, 5", () => {
    expect(List.everyNth(3, [1, 2, 3, 4, 5, 6]))
    |> toEqual([3, 6])
  });
});

describe("List.findLast", () => {
  test("should skip 4", () => {
    expect(List.findLast((n) => n mod 2 == 1, [1, 2, 3, 4]))
    |> toEqual(3)
  });
});

describe("List.findLastIndex", () => {
  test("should skip 4", () => {
    expect(List.findLastIndex((n) => n mod 2 == 1, [1, 2, 3, 4]))
    |> toEqual(2)
  });
});

describe("List.head", () => {
  test("should get 1", () => {
    expect(List.head([1, 2, 3, 4]))
    |> toEqual(1)
  });
});

describe("List.indexOfAll", () => {
  test("should get indices", () => {
    expect(List.indexOfAll(1, [1, 2, 3, 1, 2, 3]))
    |> toEqual([0, 3])
  });

  test("should get empty list", () => {
    expect(List.indexOfAll(4, [1, 2, 3, 1, 2, 3]))
    |> toEqual([])
  });
});

describe("List.initial", () => {
  test("should drop last element", () => {
    expect(List.initial([1, 2, 3]))
    |> toEqual([1, 2])
  });
});

describe("List.initialize2DArray", () => {
  test("should generate 2 x 2 List filled with 0", () => {
    expect(List.initialize2DArray(2, 2, 0))
    |> toEqual([[0, 0], [0, 0]])
  });
});

describe("List.initializeArrayWithRange", () => {
  test("should generate natural number list", () => {
    expect(List.initializeArrayWithRange(0, 5, 1))
    |> toEqual([0, 1, 2, 3, 4])
  });

  test("should generate even number list", () => {
    expect(List.initializeArrayWithRange(0, 9, 2))
    |> toEqual([0, 2, 4, 6, 8])
  });
});
