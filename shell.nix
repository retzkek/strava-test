{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name="strava-test";
  buildInputs = [
    babashka
  ];
}
