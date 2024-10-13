{
  description = "An Emacs Config From Hell.";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    telega = {
      url = "github:zevlg/telega.el";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          system = system;
          overlays = [ emacs-overlay.overlays.emacs ];
        };
        emacs-with-config = (pkgs.emacsWithPackagesFromUsePackage {
          config = "${./config.org}"; # Does not support unicode symbols...
          defaultInitFile = true;
          package = pkgs.emacs-git;
          alwaysTangle = true;
          extraEmacsPackages = epkgs: [
            epkgs.melpaPackages.jinx
            epkgs.melpaPackages.pdf-tools
            epkgs.melpaPackages.vterm
            epkgs.melpaPackages.telega
            epkgs.treesit-grammars.with-all-grammars
          ];
          override = final: prev: {
            telega = prev.melpaPackages.telega.overrideAttrs (_: {
              version = "0.8.290";
              src = inputs.telega;
            });
          };
        });
      in {
        defaultPackage = self.devShell.${system};
        devShell = pkgs.mkShell { packages = [ emacs-with-config ]; };
      });
}
