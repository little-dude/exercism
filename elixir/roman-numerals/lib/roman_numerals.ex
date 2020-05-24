defmodule RomanNumerals do
  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(n) do
    cond do
      n >= 1000 -> "M"  <> numeral(n - 1000)
      n >= 900  -> "CM" <> numeral(n - 900)
      n >= 500  -> "D"  <> numeral(n - 500)
      n >= 400  -> "CD" <> numeral(n - 400)
      n >= 100  -> "C"  <> numeral(n - 100)
      n >= 90   -> "XC" <> numeral(n - 90)
      n >= 50   -> "L"  <> numeral(n - 50)
      n >= 40   -> "XL" <> numeral(n - 40)
      n >= 10   -> "X"  <> numeral(n - 10)
      n >= 9    -> "IX" <> numeral(n - 9)
      n >= 5    -> "V"  <> numeral(n - 5)
      n >= 4    -> "IV" <> numeral(n - 4)
      n >= 1    -> "I"  <> numeral(n - 1)
      true      -> ""
    end
  end
end
