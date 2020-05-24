defmodule PigLatin do
  import Regex, only: [named_captures: 2]

  @rule1 ~r/^(?<c>((x|y)[^aeiou])|[aeiou])(?<post>.*)/
  @rules23 ~r/^(?<c>((qu)|[^aeiou])+)(?<post>.*)/
  @rule4 ~r/^(?<pre>.*?)(?<c>[^aeiou]*)y(?<post>.*)/

  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ")
    |> Enum.map(&translate_word(&1))
    |> Enum.join(" ")
  end

  def translate_word(word) do
    rule1(named_captures(@rule1, word)) ||
      rules23(named_captures(@rules23, word)) ||
      rule4(named_captures(@rule4, word)) ||
      word
  end

  def rule1(nil), do: nil

  def rule1(%{"c" => c, "post" => post}) do
    c <> post <> "ay"
  end

  defp rules23(nil), do: nil

  defp rules23(%{"c" => c, "post" => post}) do
    post <> c <> "ay"
  end

  defp rule4(nil), do: nil

  defp rule4(%{"c" => c, "post" => post}) do
    post <> c <> "quay"
  end
end
