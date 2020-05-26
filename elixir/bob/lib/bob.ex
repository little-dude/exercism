defmodule Bob do
  def hey(input) do
    answer(is_nothing?(input), is_yelling?(input), is_question?(input))
  end

  defp answer(is_nothing, is_yelling, is_question)
  defp answer(true, _, _), do: "Fine. Be that way!"
  defp answer(false, true, true), do: "Calm down, I know what I'm doing!"
  defp answer(false, false, true), do: "Sure."
  defp answer(false, true, false), do: "Whoa, chill out!"
  defp answer(false, false, false), do: "Whatever."

  def is_nothing?(input), do: String.trim(input) == ""

  def is_question?(input) do
    input
    |> String.trim()
    |> String.ends_with?(["?"])
  end

  def is_yelling?(input) do
    Regex.match?(~r/[[:alpha:]]/, input) && String.upcase(input) == input
  end
end
