defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    text
    |> String.to_charlist()
    |> Enum.map(&rotate_char(&1, rem(shift, 26)))
    |> List.to_string()
  end

  def rotate_char(c, shift) do
    cond do
      c >= ?a and c <= ?z ->
        if c + shift > ?z do
          ?a + rem(c + shift, ?z) - 1
        else
          c + shift
        end

      c >= ?A and c <= ?Z ->
        if c + shift > ?Z do
          ?A + rem(c + shift, ?Z) - 1
        else
          c + shift
        end

      true ->
        c
    end
  end
end
