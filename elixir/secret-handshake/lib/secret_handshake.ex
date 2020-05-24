defmodule SecretHandshake do
  use Bitwise

  @actions [
    {1 <<< 3, "jump"},
    {1 <<< 2, "close your eyes"},
    {1 <<< 1, "double blink"},
    {1 <<< 0, "wink"},
    {1 <<< 4, &Enum.reverse/1}
  ]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    reducer = fn {action_code, action}, acc ->
      do_command(acc, do?(code, action_code), action)
    end

    Enum.reduce(@actions, [], reducer)
  end

  defp do_command(acc, false, _), do: acc
  defp do_command(acc, true, str) when is_binary(str), do: [str | acc]
  defp do_command(acc, true, fun), do: fun.(acc)

  defp do?(code, action_code), do: action_code == (code &&& action_code)
end
