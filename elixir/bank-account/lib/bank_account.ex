defmodule BankAccount do
  use GenServer

  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  defstruct [:balance, :state]

  def start_link(opts \\ []) do
    GenServer.start_link(
      __MODULE__,
      %__MODULE__{balance: 0, state: :opened},
      opts
    )
  end

  @impl true
  def init(account) do
    {:ok, account}
  end

  @impl true
  def handle_call(_, _from, account = %__MODULE{state: :closed}) do
    {:reply, {:error, :account_closed}, account}
  end

  @impl true
  def handle_call(:stop, _from, account) do
    {:reply, nil, %__MODULE__{account | state: :closed}}
  end

  @impl true
  def handle_call(:balance, _from, account) do
    {:reply, account.balance, account}
  end

  @impl true
  def handle_call({:update, amount}, _from, account) do
    account = Map.update!(account, :balance, fn balance -> balance + amount end)
    {:reply, nil, account}
  end

  defp ensure_open(%__MODULE__{state: :closed}) do
    {:error, :account_closed}
  end

  defp ensure_open(%__MODULE__{state: :opened}) do
    :ok
  end

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, pid} = start_link()
    pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    GenServer.call(account, :stop)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    GenServer.call(account, :balance)
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    GenServer.call(account, {:update, amount})
  end
end
