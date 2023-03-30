{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
    py = import ./poetry/py.nix {requirementsFile=./p/requirements.txt; inherit pkgs;};
    #graphviz=pkgs.graphviz;
    #mkShell = pkgs.mkShell;

in    
pkgs.mkShell {
        name = "pg-shell";
        buildInputs = [
        py
        ];
      }

