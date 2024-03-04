{
  description = "Lisp implementation of snake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:/numtide/flake-utils";
    cl-nix-lite.url = "github:hraban/cl-nix-lite";
    clOpengl.url = "github:ChristopherSegale/cl-opengl-flake";
    clSDL2.url = "github:ChristopherSegale/cl-sdl2-flake";
  };
  outputs = inputs @ { self, nixpkgs, cl-nix-lite, flake-utils, clOpengl, clSDL2 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend cl-nix-lite.overlays.default;
        name = "snake";
        version = "1.0";
        inherit (pkgs) lispPackagesLite;
        inherit (lispPackagesLite) lispDerivation;
        cl-opengl = clOpengl.packages.${system}.default;
        cl-sdl2 = clSDL2.packages.${system}.default;
      in
        {
          packages = {
            # This is how you would create a derivation using SBCL (the default)
            default = lispDerivation {
              inherit name version;
              lispSystem = name;
              buildInputs = with pkgs; [
                SDL2
                libGL
              ];
              lispDependencies = [
                cl-opengl
                cl-sdl2
              ];
              src = pkgs.lib.cleanSource ./.;
              dontStrip = true;
              LD_LIBRARY_PATH = "${nixpkgs.lib.strings.makeLibraryPath [ pkgs.SDL2 pkgs.libGL ]}";
              meta = {
                license = pkgs.lib.licenses.mit;
              };
            };
          };
          apps.default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/${name}";
          };
        });
  }
