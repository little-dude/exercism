defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count([]), do: 0

  def count([_head | tail]) do
    1 + count(tail)
  end

  @spec reverse(list) :: list
  def reverse(list) do
    do_reverse(list, [])
  end

  defp do_reverse([], reversed), do: reversed

  defp do_reverse([head | tail], reversed) do
    do_reverse(tail, [head | reversed])
  end

  @spec map(list, (any -> any)) :: list
  def map([], _), do: []

  def map([head | tail], f) do
    [f.(head) | map(tail, f)]
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], f), do: []

  def filter([head | tail], f) do
    if f.(head) do
      [head | filter(tail, f)]
    else
      filter(tail, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, f), do: acc

  def reduce([head | l], acc, f) do
    reduce(l, f.(head, acc), f)
  end

  @spec append(list, list) :: list
  def append([], b), do: b

  def append([head | tail], rest) do
    [head | append(tail, rest)]
  end

  @spec concat([[any]]) :: [any]
  def concat([]), do: []

  def concat([head | tail]) do
    append(head, concat(tail))
  end
end
