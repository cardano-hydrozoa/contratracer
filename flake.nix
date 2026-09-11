{
  description = "contra-tracer: a typed contravariant tracer for Scala 3 / Cats";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.jdk21;
        # The nixpkgs sbt launcher (1.x) reads project/build.properties and bootstraps the sbt
        # version it names — including sbt 2.x — so no launcher pin is needed. NB: the bundled
        # `sbtn` thin client is sbt 1.x and cannot drive an sbt 2 server; use `sbt`, not `sbtn`.
        sbt = pkgs.sbt.override { jre = jdk; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            jdk
            sbt
            pkgs.scalafmt
          ];
        };
      }
    );
}
