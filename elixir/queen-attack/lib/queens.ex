defmodule Queens do
  @type t :: %Queens{black: {integer, integer}, white: {integer, integer}}
  defstruct [:white, :black]

  @doc """
  Creates a new set of Queens
  """
  @spec new(Keyword.t()) :: Queens.t()
  def new(opts \\ []) do
    queens = %Queens{}
    {queens, opts} = new_queen(opts, queens, :white)
    {queens, opts} = new_queen(opts, queens, :black)

    cond do
      queens.white == queens.black ->
        raise ArgumentError, "queens cannot have the same position"

      length(opts) > 0 ->
        raise ArgumentError, "unexpected arguments"

      true ->
        queens
    end
  end

  def new_queen(opts, queens, color) do
    case Keyword.pop(opts, color) do
      {nil, opts} ->
        {queens, opts}

      {position, opts} ->
        check_position!(position)
        queens = Map.put(queens, color, position)
        {queens, opts}
    end
  end

  def check_position!(position) do
    case position do
      {col, row} when 0 <= col and col < 8 and 0 <= row and row < 8 ->
        true

      _ ->
        raise ArgumentError, "invalid position"
    end
  end

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    board(queens)
    |> Enum.chunk_every(8)
    |> Enum.map(fn row -> Enum.join(row, " ") end)
    |> Enum.join("\n")
  end

  def board(queens) do
    white_index = index(queens.white)
    black_index = index(queens.black)

    for i <- 0..63 do
      cond do
        i == white_index ->
          "W"

        i == black_index ->
          "B"

        true ->
          "_"
      end
    end
  end

  defp index({row, col}), do: col + 8 * row
  defp index(nil), do: nil

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(queens) do
    same_diagonal?(queens) || same_row?(queens) || same_col?(queens)
  end

  def same_diagonal?(%Queens{black: {bx, by}, white: {wx, wy}}) do
    abs(bx - wx) == abs(by - wy)
  end

  def same_diagonal?(_), do: false

  def same_row?(%Queens{black: {_, b}, white: {_, w}}) when b == w, do: true
  def same_row?(_), do: false

  def same_col?(%Queens{black: {b, _}, white: {w, _}}) when b == w, do: true
  def same_col?(_), do: false
end
