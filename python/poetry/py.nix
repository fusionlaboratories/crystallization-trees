{
  requirementsFile
  ,pkgs
  }:
let
  myAppEnv = pkgs.poetry2nix.mkPoetryEnv {
    projectDir = ./.;
  
    #editablePackageSources = {
    #  my-app = ./src;
    #};
  };
in 
  myAppEnv
