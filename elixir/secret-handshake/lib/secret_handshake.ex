defmodule SecretHandshake do
  use Bitwise

  @action_codes [
    {1 <<< 0, "wink"},
    {1 <<< 1, "double blink"},
    {1 <<< 2, "close your eyes"},
    {1 <<< 3, "jump"}
  ]
  @reverse_code 1 <<< 4

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
    @action_codes
    |> Enum.filter(fn {action_code, _} -> (code &&& action_code) != 0 end)
    |> Enum.map(fn {_, action} -> action end)
    |> reverse?(code)
  end

  def reverse?(actions, code) do
    if (code &&& @reverse_code) != 0 do
      Enum.reverse(actions)
    else
      actions
    end
  end
end
