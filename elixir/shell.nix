with import <nixpkgs> { };
mkShell rec {
  name = "exercism-elixir";
  buildInputs = [ elixir ];
  src = null;
  shellHook = ''
    set -e
    export MIX_ARCHIVES=$(pwd)/.mix
  '';
}
