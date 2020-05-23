defmodule SecretHandshake do
  use Bitwise

  @actions [
    wink: 1,
    double_blink: 1 <<< 1,
    close_your_eyes: 1 <<< 2,
    jump: 1 <<< 3
  ]

  @reverse 1 <<< 4

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
    actions =
      @actions
      |> Enum.filter(fn {_, action_code} ->
        (code &&& action_code) != 0
      end)
      |> Enum.map(fn {action, _} ->
        action |> Atom.to_string |> String.replace("_", " ")
      end)

    if code &&& @reverse do
      Enum.reverse(actions)
    else
      actions
    end
  end
end
