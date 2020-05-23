defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count(charlist(), char()) :: non_neg_integer()
  def count(strand, nucleotide) do
    Enum.count(strand, &(&1 == nucleotide))
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram(charlist()) :: map()
  def histogram(strand) do
    # This is short but iterate through "strand" four times:
    #
    # Map.new(@nucleotides, &{&1, count(strand, &1)})

    # Same solution with `for`:
    #
    # for c <- @nucleotides, into: %{}, do: {c, count(strand, c)}

    # Single pass solution:
    map = Map.new(@nucleotides, &{&1, 0})
    Enum.reduce(strand, map, &Map.update!(&2, &1, fn v -> v + 1 end))
  end
end
