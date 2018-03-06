open Jest;

describe("Array.all", () => {
  open Expect;

  test("should be true", () => {
    expect(Array.all((x) => x > 0, [1, 2, 3]))
      |> toBe(true)
  });
  test("should be false", () =>
    expect(Array.all((x) => x > 2, [1, 2, 3]))
      |> toBe(false)
  );
});

describe("Array.any", () => {
  open Expect;

  test("should be true", () =>
    expect(Array.any((x) => x > 2, [1, 2, 3]))
      |> toBe(true)
  );
  test("should be false", () => {
    expect(Array.any((x) => x < 0, [1, 2, 3]))
      |> toBe(false)
  });
});

describe("Array.bifurcate", () => {
  open Expect;

  test("should extract \"foo\"", () =>
    expect(Array.bifurcate([true, true, false, true], ["beep", "boop", "foo", "bar"]))
      |> toEqual((["beep", "boop", "bar"], ["foo"]))
  );
});

describe("Array.bifurcateBy", () => {
  open Expect;

  test("should extract \"foo\"", () =>
    expect(Array.bifurcateBy((str) => String.contains(str, 'b'), ["beep", "boop", "foo", "bar"]))
      |> toEqual((["beep", "boop", "bar"], ["foo"]))
  );
});

describe("Array.take", () => {
  open Expect;

  test("should extract \"bar\"", () =>
    expect(Array.take(3, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["beep", "boop", "foo"])
  );

  test("should return full list when i > List.length()", () =>
    expect(Array.take(5, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["beep", "boop", "foo", "bar"])
  );

  test("should return empty list when i <= 0", () =>
    expect(Array.take(-1, ["beep", "boop", "foo", "bar"]))
      |> toEqual([])
  );
});

describe("Array.takeLast", () => {
  open Expect;

  test("should extract \"beep\"", () =>
    expect(Array.takeLast(3, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["boop", "foo", "bar"])
  );

  test("should return full list when i > List.length()", () =>
    expect(Array.takeLast(5, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["beep", "boop", "foo", "bar"])
  );

  test("should return empty list when i <= 0", () =>
    expect(Array.takeLast(-1, ["beep", "boop", "foo", "bar"]))
      |> toEqual([])
  );
});

describe("Array.chunk", () => {
  open Expect;

  test("should return 2 elements list", () =>
    expect(Array.chunk(2, ["beep", "boop", "foo", "bar"]))
      |> toEqual([["beep", "boop"], ["foo", "bar"]])
  );

  test("should return rest elements", () =>
    expect(Array.chunk(3, ["beep", "boop", "foo", "bar"]))
      |> toEqual([["beep", "boop", "foo"], ["bar"]])
  );

  test("should return empty list when size <= 0", () =>
    expect(Array.chunk(-1, ["beep", "boop", "foo", "bar"]))
      |> toEqual([])
  );
});
