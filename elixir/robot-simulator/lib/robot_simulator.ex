defmodule RobotSimulator do
  defstruct [:direction, :position]

  @type direction :: :north | :east | :south | :west
  @type t :: %__MODULE__{
          direction: direction(),
          position: {integer, integer}
        }

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: direction, position :: {integer, integer}) :: any
  def create(direction \\ :north, position \\ {0, 0}) do
    cond do
      !is_direction(direction) ->
        {:error, "invalid direction"}

      !is_position(position) ->
        {:error, "invalid position"}

      true ->
        %__MODULE__{
          direction: direction,
          position: position
        }
    end
  end

  defp is_direction(direction) do
    Enum.member?([:north, :east, :south, :west], direction)
  end

  defp is_position(position) do
    case position do
      {x, y} when is_integer(x) and is_integer(y) ->
        true

      _ ->
        false
    end
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t()) :: any
  def simulate(robot, instructions) do
    case instructions do
      "" ->
        robot

      "R" <> rest ->
        turn(robot, :right) |> simulate(rest)

      "L" <> rest ->
        turn(robot, :left) |> simulate(rest)

      "A" <> rest ->
        advance(robot) |> simulate(rest)

      _ ->
        {:error, "invalid instruction"}
    end
  end

  defp advance(robot) do
    new_position =
      case {robot.direction, robot.position} do
        {:north, {x, y}} ->
          {x, y + 1}

        {:east, {x, y}} ->
          {x + 1, y}

        {:south, {x, y}} ->
          {x, y - 1}

        {:west, {x, y}} ->
          {x - 1, y}
      end

    %__MODULE__{robot | position: new_position}
  end

  defp turn(robot, direction) do
    new_dir =
      case {robot.direction, direction} do
        {:north, :right} ->
          :east

        {:west, :right} ->
          :north

        {:south, :right} ->
          :west

        {:east, :right} ->
          :south

        {:north, :left} ->
          :west

        {:east, :left} ->
          :north

        {:south, :left} ->
          :east

        {:west, :left} ->
          :south
      end

    %__MODULE__{robot | direction: new_dir}
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(robot) do
    robot.direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: any) :: {integer, integer}
  def position(robot) do
    robot.position
  end
end
