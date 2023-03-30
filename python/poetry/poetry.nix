{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell  {
buildInputs  =  [
	pkgs.python3
	pkgs.poetry
	];
shellHook =''
if test -f pyproject.toml; then
    rm pyproject.toml
fi

if test -f poetry.lock; then
    rm poetry.lock
fi
poetry init --no-interaction;
cat requirements.txt | xargs poetry add;
exit '';
}