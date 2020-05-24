defmodule SecretHandshake do
  use Bitwise

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
    []
    |> do_commands(has_action_code?(code, 0x08), "jump")
    |> do_commands(has_action_code?(code, 0x04), "close your eyes")
    |> do_commands(has_action_code?(code, 0x02), "double blink")
    |> do_commands(has_action_code?(code, 0x01), "wink")
    |> do_commands(has_action_code?(code, 0x10), &Enum.reverse/1)
  end

  defp do_commands(acc, do?, todo)
  defp do_commands(acc, false, _), do: acc
  defp do_commands(acc, true, str) when is_binary(str), do: [str | acc]
  defp do_commands(acc, true, fun), do: fun.(acc)

  defp has_action_code?(code, action_code) do
    action_code == (code &&& action_code)
  end
end
