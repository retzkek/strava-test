{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name="strava-test";
  buildInputs = [
    jdk
    babashka
  ];
}
