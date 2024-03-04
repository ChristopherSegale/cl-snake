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
        buildInputs = with pkgs; [ coreutils SDL2 libGL ];
      in
        {
          packages = rec {
            # This is how you would create a derivation using SBCL (the default)
            default = lispDerivation {
              inherit name version;
              lispSystem = name;
              buildInputs = buildInputs ++ [ pkgs.makeWrapper ];
              propagatedBuildInputs = buildInputs;
              lispDependencies = [
                cl-opengl
                cl-sdl2
              ];
              src = pkgs.lib.cleanSource ./.;
              dontStrip = true;
              postInstall = ''
                              wrapProgram $out/bin/snake \
                              --set PATH ${pkgs.lib.makeBinPath buildInputs} \
                              --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath buildInputs}
                            '';
              meta = {
                license = pkgs.lib.licenses.mit;
              };
            };
          };
        });
  }
