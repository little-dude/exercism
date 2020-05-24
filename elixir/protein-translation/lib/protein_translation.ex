defmodule ProteinTranslation do
  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {atom, list(String.t())}
  def of_rna(rna) do
    if String.length(rna) < 3 do
      {:ok, []}
    else
      translate(rna)
    end
  end

  defp translate(rna) do
    {codon, rna} = String.split_at(rna, 3)

    case {of_codon(codon), of_rna(rna)} do
      {{:ok, "STOP"}, _} ->
        {:ok, []}

      {{:ok, protein}, {:ok, proteins}} ->
        {:ok, [protein | proteins]}

      {_, _} ->
        {:error, "invalid RNA"}
    end
  end

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: {atom, String.t()}
  def of_codon(codon) do
    cond do
      codon in ~w(UGU UGC) ->
        {:ok, "Cysteine"}

      codon in ~w(UUA UUG) ->
        {:ok, "Leucine"}

      codon == "AUG" ->
        {:ok, "Methionine"}

      codon in ~w(UUU UUC) ->
        {:ok, "Phenylalanine"}

      codon in ~w(UCU UCC UCA UCG) ->
        {:ok, "Serine"}

      codon == "UGG" ->
        {:ok, "Tryptophan"}

      codon in ~w(UAU UAC) ->
        {:ok, "Tyrosine"}

      codon in ~w(UAA UAG UGA) ->
        {:ok, "STOP"}

      true ->
        {:error, "invalid codon"}
    end
  end
end
